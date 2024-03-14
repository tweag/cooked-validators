{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module defines convenient wrappers for mock chain wallets (around
-- Plutus mock wallets) and initial distributions (that is the initial state
-- associating a list of UTxOs with some initial values to each known wallet).
-- It also exposes a convenient API to construct wallets and distributions,
-- manipulate them, and fetch information (such as public/private keys and
-- staking keys).
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

import qualified Cardano.Crypto.Wallet as Cardano
import Data.Function (on)
import qualified Data.Map.Strict as Map
import qualified Ledger.Address as Pl
import qualified Ledger.CardanoWallet as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Crypto as Pl
import Unsafe.Coerce

-- * MockChain Wallets

-- $mockchainwallets
--
-- Because the mock wallets from the plutus-apps change somewhat often, we
-- provide our own wrapper on top of them to make sure that we can easily deal
-- changes from Plutus.

type Wallet = Pl.MockWallet

type PrivateKey = Cardano.XPrv

instance Eq Wallet where
  (==) = (==) `on` Pl.mwWalletId

instance Ord Wallet where
  compare = compare `on` Pl.mwWalletId

-- | All the wallets corresponding to known Plutus mock wallets. This is a list
-- of 10 wallets which will
--
-- - receive funds in the standard initial distribution of cooked-validators,
--   and
--
-- - be pretty-printed as part the final state after running a few
--   transactions.
knownWallets :: [Wallet]
knownWallets = Pl.knownMockWallets

-- | Wallet corresponding to a given wallet number (or wallet ID)
wallet :: Int -> Wallet
wallet j
  | j > 0 && j <= 10 = let i = j - 1 in knownWallets !! i
  | otherwise = Pl.fromWalletNumber (Pl.WalletNumber $ fromIntegral j)

-- | Retrieves the id of the known wallet that corresponds to a public key
-- hash, if any.
--
-- @walletPKHashToId (walletPKHash (wallet 3)) == Just 3@
walletPKHashToId :: Pl.PubKeyHash -> Maybe Int
walletPKHashToId = flip Map.lookup walletPKHashToIdMap
  where
    walletPKHashToIdMap =
      Map.fromList . flip zip [1 ..] . map walletPKHash $ knownWallets

-- | Retrieves a wallet public key (PK)
walletPK :: Wallet -> Pl.PubKey
walletPK = Pl.unPaymentPubKey . Pl.paymentPubKey

-- | Retrieves a wallet's public staking key (PK), if any
walletStakingPK :: Wallet -> Maybe Pl.PubKey
walletStakingPK = fmap Pl.toPublicKey . walletStakingSK

-- | Retrieves a wallet's public key hash
walletPKHash :: Wallet -> Pl.PubKeyHash
walletPKHash = Pl.pubKeyHash . walletPK

-- | Retrieves a wallet's public staking key hash, if any
walletStakingPKHash :: Wallet -> Maybe Pl.PubKeyHash
walletStakingPKHash = fmap Pl.pubKeyHash . walletStakingPK

-- | Retrieves a wallet's address
walletAddress :: Wallet -> Pl.Address
walletAddress w =
  Pl.Address
    (Pl.PubKeyCredential $ walletPKHash w)
    (Pl.StakingHash . Pl.PubKeyCredential <$> walletStakingPKHash w)

-- | Retrieves a wallet private key (secret key SK)
walletSK :: Pl.MockWallet -> PrivateKey
walletSK = Pl.unPaymentPrivateKey . Pl.paymentPrivateKey

-- FIXME Massive hack to be able to open a 'MockPrivateKey'; this is needed
-- because the constructor and accessors to 'MockPrivateKey' are not exported.
-- Hence, we make an isomorphic datatype, 'unsafeCoerce' to this datatype then
-- extract whatever we need from it.
newtype HACK = HACK PrivateKey

-- | Retrieves a wallet's private staking key (secret key SK), if any
walletStakingSK :: Wallet -> Maybe PrivateKey
walletStakingSK = fmap hackUnMockPrivateKey . Pl.mwStakeKey
  where
    -- Make sure that you only apply it to @MockPrivateKey@; the function is
    -- polymorphic because @MockPrivateKey@ is not exported either
    hackUnMockPrivateKey :: a -> PrivateKey
    hackUnMockPrivateKey x = let HACK y = unsafeCoerce x in y
