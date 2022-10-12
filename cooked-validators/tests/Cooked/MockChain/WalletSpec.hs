module Cooked.MockChain.WalletSpec (tests) where

import qualified Cardano.Crypto.Wallet as Crypto
import Cooked (quickValue)
import Cooked.MockChain.Wallet
import qualified Cooked.MockChain.Wallet as Pl
import qualified Ledger as Pl
import qualified Ledger.CardanoWallet as CW
import qualified Plutus.V1.Ledger.Ada as Pl
import Test.Tasty
import Test.Tasty.HUnit

-- these instances are needed by `shouldBe` below.

instance Eq Crypto.XPrv where
  x == y = Crypto.toXPub x == Crypto.toXPub y

instance Show Crypto.XPrv where
  show = show . Crypto.toXPub

tests :: [TestTree]
tests =
  [ testCase "Hack: unwrapping a MockPrivateKey into a XPrv" $
      hackUnMockPrivateKey (CW.mwPaymentKey $ wallet 1) @?= walletSK (wallet 1),
    testCase "Not mentioning ada still gives valid initial distribution" $
      assertBool "doesn't satisfy" $
        validInitialDistribution $
          initialDistribution' [(wallet 1, [quickValue "foo" 20])],
    testCase "Auto-adding ada doesn't over-add ada " $ do
      let distr = initialDistribution' [(wallet 11, [quickValue "foo" 20 <> Pl.toValue (Pl.minAdaTxOut - 1)])]
      assertBool "invalid" $ validInitialDistribution distr
      [theValue] <- pure $ valuesForWallet distr (wallet 11)
      assertBool "over-added" $ Pl.fromValue theValue == Pl.minAdaTxOut
  ]
