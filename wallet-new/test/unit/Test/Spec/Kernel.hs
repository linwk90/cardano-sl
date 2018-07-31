module Test.Spec.Kernel (
    spec
  ) where

import           Universum

import qualified Data.Set as Set

import           Formatting (sformat, build, (%))

import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.DB.BlockMeta (BlockMeta)
import qualified Cardano.Wallet.Kernel.Diffusion as Kernel
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.MonadDBReadAdaptor (rocksDBNotAvailable)

import           Control.Exception (throw)

import           Pos.Core (Coeff (..), TxSizeLinear (..))
import           Pos.Core.Chrono

import           Test.Infrastructure.Generator
import           Test.Infrastructure.Genesis
import           Test.Pos.Configuration (withDefConfiguration)
import           Test.Spec.BlockMetaScenarios
import           Util.Buildable.Hspec
import           Util.Buildable.QuickCheck
import           Util.Validated
import           UTxO.Bootstrap
import           UTxO.Context
import           UTxO.Crypto
import           UTxO.DSL
import           UTxO.Interpreter(BlockMeta' (..), IntCtxt, IntException, Interpret, int, runIntT')
import           UTxO.Translate
import           Wallet.Abstract
import           Wallet.Inductive
import           Wallet.Inductive.Cardano
import           Wallet.Inductive.Validation

import qualified Wallet.Rollback.Full as Full

{-------------------------------------------------------------------------------
  Compare the wallet kernel with the pure model
-------------------------------------------------------------------------------}

spec :: Spec
spec =
    describe "Compare wallet kernel to pure model" $ do
      describe "Using hand-written inductive wallets" $ do
        it "computes identical results in presence of dependent pending transactions" $
          bracketActiveWallet $ \activeWallet -> do
            checkEquivalent activeWallet (dependentPending genesis)

        it "computes the expected block metadata for blockMetaScenarioA" $
          bracketActiveWallet $ checkBlockMeta' (blockMetaScenarioA genesis)
        it "computes the expected block metadata for blockMetaScenarioB" $
          bracketActiveWallet $ checkBlockMeta' (blockMetaScenarioB genesis)
        it "computes the expected block metadata for blockMetaScenarioC" $
          bracketActiveWallet $ checkBlockMeta' (blockMetaScenarioC genesis)
        it "computes the expected block metadata for blockMetaScenarioD" $
          bracketActiveWallet $ checkBlockMeta' (blockMetaScenarioD genesis)
        it "computes the expected block metadata for blockMetaScenarioE" $
          bracketActiveWallet $ checkBlockMeta' (blockMetaScenarioE genesis)

      it "computes identical results using generated inductive wallets" $
        forAll (genInductiveUsingModel model) $ \ind -> do
          conjoin [
              shouldBeValidated $ void (inductiveIsValid ind)
            , bracketActiveWallet $ \activeWallet -> do
                checkEquivalent activeWallet ind
            ]
  where
    transCtxt = runTranslateNoErrors ask
    boot      = bootstrapTransaction transCtxt
    model     = (cardanoModel linearFeePolicy boot) {
                     gmMaxNumOurs    = 1
                   , gmPotentialOurs = isPoorAddr
                   }

    -- TODO: These constants should not be hardcoded here.
    linearFeePolicy :: TxSizeLinear
    linearFeePolicy = TxSizeLinear (Coeff 155381) (Coeff 43.946)

    genesis :: GenesisValues GivenHash Addr
    genesis = genesisValues linearFeePolicy boot

    checkEquivalent :: forall h. Hash h Addr
                    => Kernel.ActiveWallet
                    -> Inductive h Addr
                    -> Expectation
    checkEquivalent w ind = shouldReturnValidated $ evaluate w ind

    -- | Evaluate the inductive step by step and compare the DSL and Cardano results
    --   at the end of each step.
    -- NOTE: This evaluation leaves side effects: it changes the state of the active wallet
    --       and also updates the inductive context, which we return to enable
    --       further custom interpretation.
    evaluate :: forall h. Hash h Addr
             => Kernel.ActiveWallet
             -> Inductive h Addr
             -> IO (Validated EquivalenceViolation (IntCtxt h))
    evaluate activeWallet ind = do
       fmap (fmap snd) $ runTranslateTNoErrors $ do
         equivalentT activeWallet (encKpEnc ekp) (mkWallet (== addr)) ind
      where
        [addr]       = Set.toList $ inductiveOurs ind
        AddrInfo{..} = resolveAddr addr transCtxt
        Just ekp     = addrInfoMasterKey

    evaluate' :: forall h. Hash h Addr
             => Kernel.ActiveWallet
             -> Inductive h Addr
             -> IO (IntCtxt h)
    evaluate' activeWallet ind = do
        res <- evaluate activeWallet ind
        case res of
            Invalid _ e   ->
               error $ sformat ("Inductive wallet evaluation failed: "%build) e
            Valid intCtxt' ->
               return intCtxt'

    mkWallet :: Hash h Addr => Ours Addr -> Transaction h Addr -> Wallet h Addr
    mkWallet = walletBoot Full.walletEmpty

    -- | Translates the DSL BlockMeta' value to BlockMeta
    intBlockMeta :: forall h. (Hash h Addr, Interpret h (BlockMeta' h))
                 => IntCtxt h
                 -> (BlockMeta' h)
                 -> TranslateT IntException IO BlockMeta
    intBlockMeta intCtxt a = do
        ma' <- catchTranslateErrors $ runIntT' intCtxt $ int a
        case ma' of
          Left err -> throw err
          Right (a', _ic') -> return a'

    checkBlockMeta' :: Hash h Addr
                    => (Inductive h Addr, BlockMeta' h)
                    -> Kernel.ActiveWallet
                    -> IO ()
    checkBlockMeta' (ind, blockMeta') activeWallet
        = do
            -- evaluates and verifies the inductive, leaving changes in wallet state and the interpretation context
            intCtxt <- evaluate' activeWallet ind

            -- translate DSL BlockMeta' to Cardano BlockMeta
            expected' <- runTranslateT $ intBlockMeta intCtxt blockMeta'

            -- grab a snapshot of the wallet state to get the BlockMeta produced by evaluating the inductive
            snapshot <- liftIO (Kernel.getWalletSnapshot (Kernel.walletPassive activeWallet))
            let actual' = actualBlockMeta snapshot

            shouldBe actual' expected'

{-------------------------------------------------------------------------------
  Manually written inductives

  NOTE: In order to test the wallet we want a HD structure. This means that
  the rich actors are not suitable test subjects.
-------------------------------------------------------------------------------}

-- | Inductive where the rollback causes dependent transactions to exist
--
-- This tests that when we report the 'change' of the wallet, we don't include
-- any outputs from pending transactions that are consumed by /other/ pending
-- transactions.
dependentPending :: forall h. Hash h Addr
                 => GenesisValues h Addr -> Inductive h Addr
dependentPending GenesisValues{..} = Inductive {
      inductiveBoot   = boot
    , inductiveOurs   = Set.singleton p0
    , inductiveEvents = OldestFirst [
          NewPending t0                  -- t0 pending
        , ApplyBlock $ OldestFirst [t0]  -- t0 new confirmed, change available
        , NewPending t1                  -- t1 pending, uses change from t0
        , Rollback                       -- now we have a dependent pending tr
        ]
    }
  where
    fee = overestimate txFee 1 2

    t0 :: Transaction h Addr
    t0 = Transaction {
             trFresh = 0
           , trIns   = Set.fromList [ fst initUtxoP0 ]
           , trOuts  = [ Output p1 1000
                       , Output p0 (initBalP0 - 1 * (1000 + fee))
                       ]
           , trFee   = fee
           , trHash  = 1
           , trExtra = []
           }

    t1 :: Transaction h Addr
    t1 = Transaction {
             trFresh = 0
           , trIns   = Set.fromList [ Input (hash t0) 1 ]
           , trOuts  = [ Output p1 1000
                       , Output p0 (initBalP0 - 2 * (1000 + fee))
                       ]
           , trFee   = fee
           , trHash  = 2
           , trExtra = []
           }

{-------------------------------------------------------------------------------
  Wallet resource management
-------------------------------------------------------------------------------}

-- | Initialize passive wallet in a manner suitable for the unit tests
bracketPassiveWallet :: (Kernel.PassiveWallet -> IO a) -> IO a
bracketPassiveWallet postHook = do
      Keystore.bracketTestKeystore $ \keystore ->
          Kernel.bracketPassiveWallet logMessage keystore rocksDBNotAvailable postHook
  where
   -- TODO: Decide what to do with logging.
   -- For now we are not logging them to stdout to not alter the output of
   -- the test runner, but in the future we could store them into a mutable
   -- reference or a TBQueue and perform assertions on them.
    logMessage _ _  = return ()

-- | Initialize active wallet in a manner suitable for generator-based testing
bracketActiveWallet :: (Kernel.ActiveWallet -> IO a) -> IO a
bracketActiveWallet test =
    withDefConfiguration $ \pm -> do
        bracketPassiveWallet $ \passive ->
          Kernel.bracketActiveWallet pm passive diffusion $ \active ->
            test active

-- TODO: Decide what we want to do with submitted transactions
diffusion :: Kernel.WalletDiffusion
diffusion =  Kernel.WalletDiffusion {
    walletSendTx = \_tx -> return False
  }
