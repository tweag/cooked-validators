module Cooked.MockChain.UtxoState where

import Cooked.Skeleton (TxSkelOutDatum)
import Data.Function (on)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Ledger as Pl hiding (Value)
import qualified Plutus.V1.Ledger.Value as Pl1

-- TODO Migrate to V2 values (see imports)

-- | A 'UtxoState' provides us with the mental picture of the state of the UTxO graph:
-- Each address has a set of UTxOs that consist in a value and some potential datum.
newtype UtxoState = UtxoState {utxoState :: Map Pl.Address UtxoValueSet}
  deriving (Eq)

instance Semigroup UtxoState where
  (UtxoState a) <> (UtxoState b) = UtxoState $ Map.unionWith (<>) a b

-- | Represents a /set/ of values, yet, we use a list instead of a set because 'Pl.Value'
-- doesn't implement 'Ord' and because it is possible that we want to distinguish between utxo states
-- that have additional utxos, even if these could have been merged together.
newtype UtxoValueSet = UtxoValueSet {utxoValueSet :: [(Pl1.Value, TxSkelOutDatum)]}
  deriving (Show)

instance Eq UtxoValueSet where
  (UtxoValueSet xs) == (UtxoValueSet ys) = xs' == ys'
    where
      k (val, m) = (Pl1.flattenValue val, m)
      xs' = List.sortBy (compare `on` k) xs
      ys' = List.sortBy (compare `on` k) ys

instance Semigroup UtxoValueSet where
  UtxoValueSet a <> UtxoValueSet b = UtxoValueSet $ a ++ b

instance Monoid UtxoValueSet where
  mempty = UtxoValueSet []

-- | Computes the total value in a set
utxoValueSetTotal :: UtxoValueSet -> Pl1.Value
utxoValueSetTotal = mconcat . map fst . utxoValueSet

-- | Computes the total value in the entire state
utxoStateTotal :: UtxoState -> Pl1.Value
utxoStateTotal = mconcat . map utxoValueSetTotal . Map.elems . utxoState
