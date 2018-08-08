{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Spec.GetTransactions (spec) where

import           Universum

import           Test.Hspec (Spec, describe, expectationFailure, shouldBe,
                     shouldMatchList, shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (withMaxSuccess)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO, pick)

import           Control.Lens (to)
--import           Data.Acid (update)
import           Formatting (build, sformat)

--import           Pos.Crypto (EncryptedSecretKey, safeDeterministicKeyGen)

import           Cardano.Wallet.API.Request
import           Cardano.Wallet.API.Request.Pagination
--- import           Cardano.Wallet.API.Request.Filter
-- import           Cardano.Wallet.API.Request.Sort
import qualified Cardano.Wallet.Kernel as Kernel
-- import qualified Cardano.Wallet.Kernel.Addresses as Kernel
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountIx (..),
                     HdRootId (..), hdAccountIdIx)
--import           Cardano.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.TxMeta
--import           Cardano.Wallet.Kernel.Internal (PassiveWallet, wallets)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.Types (AccountId (..), WalletId (..))
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer

--import           Cardano.Wallet.API.V1.Handlers.Addresses as Handlers
import qualified Cardano.Wallet.API.V1.Types as V1
--import           Control.Monad.Except (runExceptT)
--import           Servant.Server

import           Pos.Core as Core
import           Test.Spec.CreateAddress hiding (spec)
import qualified Test.Spec.Fixture as Fixture
import           TxMetaStorageSpecs (Isomorphic (..), genMeta)
import           Util.Buildable (ShowThroughBuild (..))

withFixture :: MonadIO m
            => (  Keystore.Keystore
               -> PassiveWalletLayer m
               -> Kernel.PassiveWallet
               -> Fixture
               -> IO a
               )
            -> PropertyM IO a
withFixture = Fixture.withPassiveWalletFixture prepareFixtures

spec :: Spec
spec =
    describe "Get Transactions after Address Creation" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $
            monadicIO $ do
                testMetaSTB <- pick genMeta
                withFixture $ \keystore layer pwallet Fixture{..} -> do
                    liftIO $ Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                    let (HdRootId hdRoot) = fixtureHdRootId
                        (AccountIdHdRnd myAccountId) = fixtureAccountId
                        wId = sformat build (view fromDb hdRoot)
                        accIdx = myAccountId ^. hdAccountIdIx . to getHdAccountIx
                        hdl = (pwallet ^. Kernel.walletMeta)
                        testMeta = unSTB testMetaSTB
                    case decodeTextAddress wId of
                        Left _         -> expectationFailure "decodeTextAddress failed"
                        Right rootAddr -> do
                            let meta = testMeta {_txMetaWalletId = rootAddr, _txMetaAccountId = accIdx}
                            _ <- liftIO ((WalletLayer._pwlCreateAddress layer) (V1.NewAddress Nothing accIdx (V1.WalletId wId)))
                            putTxMeta (pwallet ^. Kernel.walletMeta) meta
                            (result, mbCount) <- (getTxMetas hdl) (Offset 0) (Limit 10) Everything Nothing NoFilterOp NoFilterOp Nothing
                            map Isomorphic result `shouldMatchList` [Isomorphic meta]
                            eiResp <- WalletLayer._pwlGetTransactions
                                    layer
                                    Nothing
                                    Nothing
                                    Nothing
                                    (RequestParams $ PaginationParams (Page 1) (PerPage 10))
                                    NoFilters
                                    NoSorts
                            eiResp `shouldSatisfy` isRight
                            mbCount `shouldBe` (Just 1)
