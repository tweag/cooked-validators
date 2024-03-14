{-# LANGUAGE NumericUnderscores #-}

module Cooked.Distribution
  ( initialDistribution,
    InitialDistribution (..),
  )
where

import Cooked.Wallet
import Data.Default
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Plutus.Script.Utils.Value as Pl

-- * Initial distribution of funds

-- | Describes the initial distribution of UTxOs per wallet. This is important
-- since transaction validation must specify a /collateral/. Hence, wallets
-- must have more than one UTxO to begin with in order to execute a transaction
-- and have some collateral option. The @txCollateral@ is transferred to the
-- node operator in case the transaction fails to validate.
--
--  The following specifies a starting state where @wallet 1@ contains two
--  UTxOs, one with 42 Ada and one with 2 Ada and one "TOK" token; @wallet 2@
--  contains a single UTxO with 10 Ada and @wallet 3@ has 10 Ada and a
--  permanent value. See "Cooked.Currencies" for more information on quick and
--  permanent values.
--
--  > i0 = InitialDistribution $ M.fromList
--  >        [ (wallet 1 , [ Pl.lovelaveValueOf 42_000_000
--  >                      , Pl.lovelaceValueOf 2_000_000 <> quickValue "TOK" 1
--  >                      ]
--  >        , (wallet 2 , [Pl.lovelaveValueOf 10_000_000])
--  >        , (wallet 3 , [Pl.lovelaceValueOf 10_000_000 <> permanentValue "XYZ" 10])
--  >        ]
newtype InitialDistribution = InitialDistribution
  { unInitialDistribution :: Map Wallet [Pl.Value]
  }
  deriving (Eq, Show)

instance Semigroup InitialDistribution where
  (InitialDistribution i) <> (InitialDistribution j) =
    InitialDistribution $ Map.unionWith (<>) i j

instance Monoid InitialDistribution where
  mempty = InitialDistribution Map.empty

-- | 10 UTxOs with 100 Ada each, for each of the 'knownWallets'.
instance Default InitialDistribution where
  def =
    InitialDistribution $
      Map.fromList $
        zip knownWallets (repeat $ replicate 10 defLovelace)
    where
      defLovelace = Pl.lovelaceValueOf 100_000_000

distributionFromList :: [(Wallet, [Pl.Value])] -> InitialDistribution
distributionFromList = InitialDistribution . Map.fromList

initialDistribution :: [(Wallet, [Pl.Value])] -> InitialDistribution
initialDistribution = (def <>) . distributionFromList
