-- | This module provides a convenient way to spread assets between
-- wallets and scripts at the initialization of the mock chain. These
-- initial assets can be accompanied by datums and reference scripts.
module Cooked.InitialDistribution
  ( initialDistribution,
    InitialDistribution (..),
    valueToUTxOContent,
    UTxOContent (..),
    addDatum,
    addReferenceScript,
    datumToUTxOContent,
    referenceScriptToUTxOContent,
    distributionFromList,
    distributionFromValueList,
  )
where

import Cooked.Output
import Cooked.ValueUtils
import Cooked.Wallet
import Data.Bifunctor (second)
import Data.Default
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Pl
import qualified Plutus.Script.Utils.Value as Pl
import qualified Plutus.V2.Ledger.Api as Pl

-- * Initial distribution of funds

-- | Describes the initial distribution of UTxOs per wallet. This is
-- important since transaction validation must specify a
-- /collateral/. Hence, wallets must have more than one UTxO to begin
-- with in order to execute a transaction and have some collateral
-- option. The @txCollateral@ is transferred to the node operator in
-- case the transaction fails to validate.
--
--  The following specifies a starting state where @wallet 1@ owns two
--  UTxOs, one with 42 Ada and one with 2 Ada and one "TOK" token;
--  @wallet 2@ owns a single UTxO with 10 Ada and @wallet 3@ has 10
--  Ada and a permanent value. See "Cooked.Currencies" for more
--  information on quick and permanent values.
--
--  > i0 = InitialDistribution $ M.fromList
--  >        [ (wallet 1 , [ ada 42 , ada 2 <> quickValue "TOK" 1 ]
--  >        , (wallet 2 , [ ada 10 ])
--  >        , (wallet 3 , [ ada 10 <> permanentValue "XYZ" 10])
--  >        ]

-- | This represents what can be placed within UTxOs in the initial
-- distribution: a value, a datum andq a possible reference
data UTxOContent = UTxOContent
  { ucValue :: Pl.Value,
    ucDatum :: Pl.OutputDatum,
    ucScript :: Maybe Pl.ScriptHash
  }
  deriving (Eq, Show)

valueToUTxOContent :: Pl.Value -> UTxOContent
valueToUTxOContent val = UTxOContent val Pl.NoOutputDatum Nothing

instance Default UTxOContent where
  def = valueToUTxOContent (ada 2)

addDatum :: (Pl.ToData a) => UTxOContent -> a -> UTxOContent
addDatum content datum = content {ucDatum = toOutputDatum $ Pl.toBuiltinData datum}

datumToUTxOContent :: (Pl.ToData a) => a -> UTxOContent
datumToUTxOContent = addDatum def

addReferenceScript :: UTxOContent -> Pl.TypedValidator a -> UTxOContent
addReferenceScript content script = content {ucScript = Just $ toScriptHash script}

referenceScriptToUTxOContent :: Pl.TypedValidator a -> UTxOContent
referenceScriptToUTxOContent = addReferenceScript def

-- | An initial distribution associates a list of UTxOContent to
-- wallets
newtype InitialDistribution = InitialDistribution
  { unInitialDistribution :: Map Wallet [UTxOContent]
  }
  deriving (Eq, Show)

instance Semigroup InitialDistribution where
  (InitialDistribution i) <> (InitialDistribution j) =
    InitialDistribution $ Map.unionWith (<>) i j

instance Monoid InitialDistribution where
  mempty = InitialDistribution Map.empty

-- | 5 UTxOs with 100 Ada each, for each of the 'knownWallets',
-- without any datum nor scripts
instance Default InitialDistribution where
  def =
    distributionFromList
      . zip knownWallets
      . repeat
      . replicate 5
      . valueToUTxOContent
      $ ada 100

distributionFromList :: [(Wallet, [UTxOContent])] -> InitialDistribution
distributionFromList = InitialDistribution . Map.fromList

distributionFromValueList :: [(Wallet, [Pl.Value])] -> InitialDistribution
distributionFromValueList = distributionFromList . map (second $ map valueToUTxOContent)

initialDistribution :: [(Wallet, [UTxOContent])] -> InitialDistribution
initialDistribution = (def <>) . distributionFromList
