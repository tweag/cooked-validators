module Cooked.Wallet where

import qualified Ledger.Crypto as Pl
import qualified Wallet.Emulator as Pl

-- * MockChain Wallets
--
-- We keep the private key associated with each wallet so we can sign transactions
-- from any wallet easily

type Wallet = (Pl.Wallet, Pl.PrivateKey)

wallet :: Int -> Wallet
wallet j
  | j > 0 && j <= 10 = let i = j - 1 in (Pl.knownWallets !! i , Pl.knownPrivateKeys !! i)
  | otherwise        = error "There are only 10 wallets, starting index is 1"

walletPK :: Wallet -> Pl.PubKey
walletPK = Pl.walletPubKey . fst

walletPKHash :: Wallet -> Pl.PubKeyHash
walletPKHash = Pl.pubKeyHash . walletPK
