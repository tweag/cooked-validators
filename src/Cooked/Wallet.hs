{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module defines convenient wrappers for mock chain wallets (around
-- Plutus mock wallets) with an associate API to construct them, manipulate
-- them, and fetch information (such as public/private and staking keys).
module Cooked.Wallet
  ( knownWallets,
    wallet,
    walletPKHashToId,
    walletPK,
    walletStakingPK,
    walletPKHash,
    walletStakingPKHash,
    walletAddress,
    walletSK,
    walletStakingSK,
    Wallet,
    PrivateKey,
  )
where

import Cardano.Crypto.Wallet qualified as Crypto
import Data.Function (on)
import Data.List (elemIndex)
import Ledger.Address qualified as Ledger
import Ledger.CardanoWallet qualified as Ledger
import Ledger.Crypto qualified as Ledger
import PlutusLedgerApi.V3 qualified as Api

-- * MockChain Wallets

-- $mockchainwallets
--
-- Because mock wallets from plutus-ledger change often, we provide our own
-- wrapper on top of them to ensure that we can easily deal changes from Plutus.

type Wallet = Ledger.MockWallet

type PrivateKey = Crypto.XPrv

instance Eq Wallet where
  (==) = (==) `on` Ledger.mwWalletId

instance Ord Wallet where
  compare = compare `on` Ledger.mwWalletId

-- | All the wallets corresponding to known Plutus mock wallets. This is a list
-- of 10 wallets which will
--
-- - receive funds in the standard initial distribution of cooked-validators,
--
-- - be pretty-printed as part the final state after running a few transactions.
knownWallets :: [Wallet]
knownWallets = Ledger.knownMockWallets

-- | Wallet corresponding to a given wallet number (or wallet ID) with an offset
-- of 1 to start at 1 instead of 0
wallet :: Integer -> Wallet
wallet j
  | j > 0 && j <= 10 = Ledger.knownMockWallet j
  | otherwise = Ledger.fromWalletNumber $ Ledger.WalletNumber j

-- | Retrieves the id of the known wallet that corresponds to a public key hash
--
-- @walletPKHashToId (walletPKHash (wallet 3)) == Just 3@
walletPKHashToId :: Api.PubKeyHash -> Maybe Int
walletPKHashToId = (succ <$>) . flip elemIndex (walletPKHash <$> knownWallets)

-- | Retrieves a wallet public key (PK)
walletPK :: Wallet -> Ledger.PubKey
walletPK = Ledger.unPaymentPubKey . Ledger.paymentPubKey

-- | Retrieves a wallet's public staking key (PK), if any
walletStakingPK :: Wallet -> Maybe Ledger.PubKey
walletStakingPK = fmap Ledger.toPublicKey . walletStakingSK

-- | Retrieves a wallet's public key hash
walletPKHash :: Wallet -> Api.PubKeyHash
walletPKHash = Ledger.pubKeyHash . walletPK

-- | Retrieves a wallet's public staking key hash, if any
walletStakingPKHash :: Wallet -> Maybe Api.PubKeyHash
walletStakingPKHash = fmap Ledger.pubKeyHash . walletStakingPK

-- | Retrieves a wallet's address
walletAddress :: Wallet -> Api.Address
walletAddress w =
  Api.Address
    (Api.PubKeyCredential $ walletPKHash w)
    (Api.StakingHash . Api.PubKeyCredential <$> walletStakingPKHash w)

-- | Retrieves a wallet private key (secret key SK)
walletSK :: Wallet -> PrivateKey
walletSK = Ledger.unPaymentPrivateKey . Ledger.paymentPrivateKey

-- | Retrieves a wallet's private staking key (secret key SK), if any
walletStakingSK :: Wallet -> Maybe PrivateKey
walletStakingSK = fmap Ledger.unStakePrivateKey . Ledger.stakePrivateKey
