{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.Wallet where

import Data.Function (on)
import qualified Data.Map.Strict as M
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import qualified Ledger.CardanoWallet as CW
import qualified Ledger.Credential as Pl

-- * MockChain Wallets

-- $mockchainwallets
--
-- Because the mock wallets from the plutus-apps changes somewhat often, we will
-- provide our own wrapper on top of them to make sure that we can easily deal
-- changes from Plutus.

type Wallet = CW.MockWallet

instance Eq Wallet where
  (==) = (==) `on` CW.mwWalletId

instance Ord Wallet where
  compare = compare `on` CW.mwWalletId

knownWallets :: [Wallet]
knownWallets = CW.knownWallets

wallet :: Int -> Wallet
wallet j
  | j > 0 && j <= 10 = let i = j - 1 in knownWallets !! i
  | otherwise = CW.fromWalletNumber (CW.WalletNumber $ fromIntegral j)

walletPK :: Wallet -> Pl.PubKey
walletPK = CW.pubKey

walletPKHash :: Wallet -> Pl.PubKeyHash
walletPKHash = Pl.pubKeyHash . walletPK

walletAddress :: Wallet -> Pl.Address
walletAddress = (`Pl.Address` Nothing) . Pl.PubKeyCredential . walletPKHash

walletSK :: CW.MockWallet -> Pl.PrivateKey
walletSK = CW.privateKey

-- * Signs a transaction

txAddSignature :: Wallet -> Pl.Tx -> Pl.Tx
txAddSignature w = Pl.addSignature (CW.privateKey w)

-- * Initial distribution of funds

-- $initfundsdistr
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
