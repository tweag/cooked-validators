{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
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
    txAddSignature,
    initialDistribution,
    InitialDistribution (..),
    Wallet,
    PrivateKey,
  )
where

import qualified Cardano.Crypto.Wallet as Cardano
import Data.Default
import Data.Function (on)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Ledger.Address as Pl
import qualified Ledger.CardanoWallet as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Crypto as Pl
import qualified Ledger.Tx as Pl
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Plutus.Script.Utils.Value as Pl
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

-- | All the wallets corresponding to known Plutus mock wallets.
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

-- | Retrieves a wallet publik key (PK)
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

-- FIXME Massive hack to be able to open a MockPrivateKey; this is needed
-- because the constructor and accessor to MockPrivateKey are not exported.
-- Hence, we make an isomorphic datatype, unsafeCoerce to this datatype then
-- extract whatever we need from it.
newtype HACK = HACK Cardano.XPrv

-- | Retrieves a wallet's private staking key (secret key SK), if any
walletStakingSK :: Wallet -> Maybe PrivateKey
walletStakingSK = fmap hackUnMockPrivateKey . Pl.mwStakeKey
  where
    -- Don't move this function to outside the where clause; its a hack and
    -- will be deprecated once we have time to make a PR into plutus exporting
    -- the things we need. If you use this anyway, make sure that you only
    -- apply it to @MockPrivateKey@; the function is polymorphic because
    -- @MockPrivateKey@ is not exported either
    hackUnMockPrivateKey :: a -> Cardano.XPrv
    hackUnMockPrivateKey x = let HACK y = unsafeCoerce x in y

-- | Signs a transaction
txAddSignature :: Wallet -> Pl.Tx -> Pl.Tx
txAddSignature w = Pl.addSignature' (walletSK w)

-- * Initial distribution of funds

-- $initfundsdistr
--
-- Are nothing but is a map from Wallet to Value; we'll just proxy
-- the underlying plutus definitions to make it easer when we have
-- to plug our own, if we ever have the need

-- | Describes the initial distribution of /UTxOs/ per wallet. This is
-- important since transaction validation must specify a /collateral/, hence,
-- wallets must posses more than one UTxO to begin with in order to execute a
-- transaction and have some collateral option. The @txCollateral@ is
-- transfered to the node operator in case the transaction fails to validate.
--
--  An initial distribution defined by:
--
--  > i0 = InitialDistribution $ M.fromList
--  >        [ (wallet 1 , [ Pl.lovelaveValueOf 42000000
--  >                      , Pl.lovelaceValueOf 2000000 <> quickValue "TOK" 1
--  >                      ]
--  >        , (wallet 2 , [Pl.lovelaveValueOf 10000000])
--  >        , (wallet 3 , [Pl.lovelaceValueOf 10000000 <> permanentValue "XYZ" 10])
--  >        ]
--
--  Specifies a starting state where @wallet 1@ contains two /UTxOs/, one with
--  42 Ada and one with 2 Ada and one "TOK" token; @wallet 2@ contains a single
--  /UTxO/ with 10 Ada and @wallet 3@ has 10 Ada and a permanent value. Check
--  #quickvalues for more on quick and permanent values. (Remember: 1 Ada =
--  1000000 Lovelace)
--
--  Check the corresponding @Default InitialDistribution@ instance for the
--  default value.
newtype InitialDistribution = InitialDistribution
  { unInitialDistribution :: Map Wallet [Pl.Value]
  }
  deriving (Eq, Show)

instance Semigroup InitialDistribution where
  (InitialDistribution i) <> (InitialDistribution j) =
    InitialDistribution $ Map.unionWith (<>) i j

instance Monoid InitialDistribution where
  mempty = InitialDistribution Map.empty

instance Default InitialDistribution where
  def =
    InitialDistribution $
      Map.fromList $
        zip knownWallets (repeat $ replicate 10 defLovelace)
    where
      defLovelace = Pl.lovelaceValueOf 100_000_000

-- | Ensures the distribution is valid by adding any missing Ada to all utxos.
distributionFromList :: [(Wallet, [Pl.Value])] -> InitialDistribution
distributionFromList = InitialDistribution . Map.fromList

-- | Extension of the default initial distribution with additional value in
-- some wallets.
initialDistribution :: [(Wallet, [Pl.Value])] -> InitialDistribution
initialDistribution = (def <>) . distributionFromList
