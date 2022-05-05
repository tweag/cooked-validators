module Cooked.MockChain.WalletSpec (tests) where

import qualified Cardano.Crypto.Wallet as Crypto
import Cooked.MockChain.Wallet
import qualified Ledger.CardanoWallet as CW
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
      hackUnMockPrivateKey (CW.mwPaymentKey $ wallet 1) @?= walletSK (wallet 1)
  ]
