module Cooked.MockChain.UtxoState
  ( UtxoState (..),
    UtxoPayloadSet (..),
    UtxoPayload (..),
    holdsInState,
  )
where

import Cooked.Skeleton (TxSkelOutDatum)
import Data.Function (on)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Ledger as Pl hiding (Value)
import qualified Plutus.Script.Utils.Value as Pl
import qualified Plutus.V2.Ledger.Api as PV2

-- | A 'UtxoState' provides us with the mental picture of the state of the UTxO graph:
-- Each address has a set of UTxOs that consist in a value and some potential datum.
newtype UtxoState = UtxoState {utxoState :: Map Pl.Address UtxoPayloadSet}
  deriving (Eq)

-- | Returns the total value accessible to the entity designed by the
-- address.
holdsInState :: PV2.Address -> UtxoState -> PV2.Value
holdsInState address (UtxoState m) =
  maybe mempty utxoPayloadSetTotal (Map.lookup address m)

instance Semigroup UtxoState where
  (UtxoState a) <> (UtxoState b) = UtxoState $ Map.unionWith (<>) a b

-- | Represents a /set/ of payloads. Payloads are the name we give to the
-- information we care about on a UTxO: value, datum, and reference script. We
-- use a list instead of a set because 'Pl.Value' doesn't implement 'Ord' and
-- because it is possible that we want to distinguish between utxo states that
-- have additional utxos, even if these could have been merged together.
newtype UtxoPayloadSet = UtxoPayloadSet {utxoPayloadSet :: [UtxoPayload]}
  deriving (Show)

data UtxoPayload = UtxoPayload
  { utxoPayloadTxOutRef :: Pl.TxOutRef,
    utxoPayloadValue :: PV2.Value,
    utxoPayloadSkelOutDatum :: TxSkelOutDatum,
    utxoPayloadReferenceScript :: Maybe Pl.ScriptHash
  }
  deriving (Eq, Show)

instance Eq UtxoPayloadSet where
  (UtxoPayloadSet xs) == (UtxoPayloadSet ys) = xs' == ys'
    where
      k (UtxoPayload ref val dat rs) = (ref, Pl.flattenValue val, dat, rs)
      xs' = List.sortBy (compare `on` k) xs
      ys' = List.sortBy (compare `on` k) ys

instance Semigroup UtxoPayloadSet where
  UtxoPayloadSet a <> UtxoPayloadSet b = UtxoPayloadSet $ a ++ b

instance Monoid UtxoPayloadSet where
  mempty = UtxoPayloadSet []

-- | Computes the total value in a set
utxoPayloadSetTotal :: UtxoPayloadSet -> PV2.Value
utxoPayloadSetTotal = mconcat . fmap utxoPayloadValue . utxoPayloadSet
