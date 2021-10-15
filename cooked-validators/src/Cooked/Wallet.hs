module Cooked.Wallet where

import qualified Ledger.Crypto as L
import qualified Wallet.Emulator as L

-- * MockChain Wallets
--
-- We keep the private key associated with each wallet so we can sign transactions
-- from any wallet easily

type Wallet = (L.Wallet, L.PrivateKey)

wallet :: Int -> Wallet
wallet j
  | j > 0 && j <= 10 = let i = j - 1 in (L.knownWallets !! i , L.knownPrivateKeys !! i)
  | otherwise        = error "There are only 10 wallets, starting index is 1"

walletPK :: Wallet -> L.PubKey
walletPK = L.walletPubKey . fst

walletPKHash :: Wallet -> L.PubKeyHash
walletPKHash = L.pubKeyHash . walletPK
