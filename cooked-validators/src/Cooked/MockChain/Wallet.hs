{-# LANGUAGE NumericUnderscores #-}

module Cooked.MockChain.Wallet where

import qualified Data.Map.Strict as M
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import qualified Ledger.Credential as Pl
import qualified Plutus.Contract.Trace as Pl

-- * MockChain Wallets

--
-- We keep the private key associated with each wallet so we can sign transactions
-- from any wallet easily

type Wallet = (Pl.Wallet, Pl.PrivateKey)

knownWallets :: [Wallet]
knownWallets = zip Pl.knownWallets Pl.knownPrivateKeys

wallet :: Int -> Wallet
wallet j
  | j > 0 && j <= 10 = let i = j - 1 in knownWallets !! i
  | otherwise = error "There are only 10 wallets, starting index is 1"

walletPK :: Wallet -> Pl.PubKey
walletPK = Pl.walletPubKey . fst

walletPKHash :: Wallet -> Pl.PubKeyHash
walletPKHash = Pl.pubKeyHash . walletPK

walletPKHashToIdMap :: M.Map Pl.PubKeyHash Int
walletPKHashToIdMap = M.fromList . flip zip [1 ..] . map walletPKHash $ knownWallets

walletAddress :: Wallet -> Pl.Address
walletAddress = (`Pl.Address` Nothing) . Pl.PubKeyCredential . walletPKHash

-- * Signs a transaction

txAddSignature :: Wallet -> Pl.Tx -> Pl.Tx
txAddSignature (_, sk) = Pl.addSignature sk

-- * Initial distribution of funds

--
-- Are nothing but is a map from Wallet to Value; we'll just proxy
-- the underlying plutus definitions to make it easer when we have
-- to plug our own, if we ever have the need

type InitialDistribution = M.Map Wallet Pl.Value

initialDistribution :: InitialDistribution
initialDistribution = M.fromList $ zip knownWallets (repeat def)
  where
    def = Pl.lovelaceValueOf 100_000

initialTxFor :: InitialDistribution -> Pl.Tx
initialTxFor initDist =
  mempty
    { Pl.txMint = mconcat (map snd initDist'),
      Pl.txOutputs = map (\wv -> Pl.TxOut (walletAddress $ fst wv) (snd wv) Nothing) initDist'
    }
  where
    initDist' = M.toList initDist
