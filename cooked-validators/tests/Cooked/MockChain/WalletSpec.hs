module Cooked.MockChain.WalletSpec where

import qualified Cardano.Crypto.Wallet as Crypto
import Cooked.MockChain.Wallet
import qualified Ledger.CardanoWallet as CW
import Test.Hspec

-- these instances are needed by `shouldBe` below.

instance Eq Crypto.XPrv where
  x == y = Crypto.toXPub x == Crypto.toXPub y

instance Show Crypto.XPrv where
  show = show . Crypto.toXPub

spec :: SpecWith ()
spec = do
  describe "Hack: unwrapping a MockPrivateKey into a XPrv" $ do
    it "Works" $
      hackUnMockPrivateKey (CW.mwPaymentKey $ wallet 1)
        `shouldBe` walletSK (wallet 1)
