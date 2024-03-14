{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}

module Cooked.InitialDistribution
  ( initialDistribution,
    InitialDistribution (..),
  )
where

import Cooked.Wallet
import Data.Default
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Pl
import qualified Plutus.Script.Utils.Value as Pl
import qualified Plutus.V2.Ledger.Api as Pl

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

-- | This represents what can be placed within Utxos in the initial distribution:
-- * A value
-- * A datum with its builtin data representation
-- * A script hash to represent a reference script
type UtxoContent = (Pl.Value, Pl.OutputDatum, Maybe Pl.ScriptHash)

-- | An initial distribution associates a list of UtxoContent to wallets
newtype InitialDistribution = InitialDistribution
  { unInitialDistribution :: Map Wallet [UtxoContent]
  }
  deriving (Eq, Show)

instance Semigroup InitialDistribution where
  (InitialDistribution i) <> (InitialDistribution j) =
    InitialDistribution $ Map.unionWith (<>) i j

instance Monoid InitialDistribution where
  mempty = InitialDistribution Map.empty

-- | 5 UTxOs with 100 Ada each, for each of the 'knownWallets', without any datum nor scripts
instance Default InitialDistribution where
  def = distributionFromList . zip knownWallets . repeat . replicate 5 . (,Pl.NoOutputDatum,Nothing) . Pl.lovelaceValueOf $ 100_000_000

distributionFromList :: [(Wallet, [UtxoContent])] -> InitialDistribution
distributionFromList = InitialDistribution . Map.fromList

initialDistribution :: [(Wallet, [UtxoContent])] -> InitialDistribution
initialDistribution = (def <>) . distributionFromList

unitDistribution :: (Pl.ToData a) => Wallet -> Pl.Value -> a -> Pl.TypedValidator b -> InitialDistribution
unitDistribution user value datum validator =
  initialDistribution
    [ ( user,
        [ ( value,
            Pl.OutputDatum $ Pl.Datum $ Pl.toBuiltinData datum,
            Just $ Pl.validatorHash validator
          )
        ]
      )
    ]
