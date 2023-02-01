module Cooked.MockChain.UtxoState where

import Cooked.Pretty.Class (DocCooked)
import Data.Function (on)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Ledger as Pl
import qualified Ledger.Value as Pl
import qualified PlutusTx.Numeric as Pl

-- | A 'UtxoState' provides us with the mental picture of the state of the UTxO graph:
-- Each address has a set of UTxOs that consist in a value and some potential datum.
newtype UtxoState = UtxoState {utxoState :: M.Map Pl.Address UtxoValueSet}
  deriving (Eq)

instance Semigroup UtxoState where
  (UtxoState a) <> (UtxoState b) = UtxoState $ M.unionWith (<>) a b

-- | Represents a /set/ of values, yet, we use a list instead of a set because 'Pl.Value'
-- doesn't implement 'Ord' and because it is possible that we want to distinguish between utxo states
-- that have additional utxos, even if these could have been merged together.
newtype UtxoValueSet = UtxoValueSet {utxoValueSet :: [(Pl.Value, Maybe UtxoDatum)]}
  deriving (Show)

instance Eq UtxoValueSet where
  (UtxoValueSet xs) == (UtxoValueSet ys) = xs' == ys'
    where
      k (val, m) = (Pl.flattenValue val, m)
      xs' = L.sortBy (compare `on` k) xs
      ys' = L.sortBy (compare `on` k) ys

instance Semigroup UtxoValueSet where
  UtxoValueSet a <> UtxoValueSet b = UtxoValueSet $ a ++ b

instance Monoid UtxoValueSet where
  mempty = UtxoValueSet []

-- | Computes the total value in a set
utxoValueSetTotal :: UtxoValueSet -> Pl.Value
utxoValueSetTotal = mconcat . map fst . utxoValueSet

-- | Computes the total value in the entire state
utxoStateTotal :: UtxoState -> Pl.Value
utxoStateTotal = mconcat . map utxoValueSetTotal . M.elems . utxoState

-- | A 'UtxoDatum' contains a datum which is @Datum $ Pl.toBuiltinData x@ for
-- some @x :: X@, but we also include @pretty x@ to be able to print this value
-- in a more user friendly fashion.
data UtxoDatum = UtxoDatum
  { utxoDatum :: Pl.Datum,
    utxoInlined :: Bool,
    utxoDoc :: DocCooked
  }

-- We ignore the pretty-printed document when implementing ordering and
-- equality since 'Doc' do not implement 'Eq' and 'Ord'.

instance Eq UtxoDatum where
  (UtxoDatum datum1 inlined1 _) == (UtxoDatum datum2 inlined2 _) = (datum1, inlined1) == (datum2, inlined2)

instance Ord UtxoDatum where
  compare (UtxoDatum datum1 inlined1 _) (UtxoDatum datum2 inlined2 _) = compare (datum1, inlined1) (datum2, inlined2)

instance Show UtxoDatum where
  show (UtxoDatum _ _ doc) = show doc

-- * Differences between two 'UtxoState'

-- | The differences between two states @a@ and @b@ is a map whose keys
-- are @M.keys a `union` M.keys b@ and values dictate what happened to that
-- particular value set with a 'UtxoValueSetDiff'
type UtxoStateDiff = M.Map Pl.Address UtxoValueSetDiff

-- | A 'UtxoValueSet' can have been inserted, deleted or modified.
data UtxoValueSetDiff
  = Inserted UtxoValueSet
  | Deleted UtxoValueSet
  | -- | In case it has been modified, the order is irrelevant; hence we just need to keep the deltas around.
    Modified
      { modDeleted :: UtxoValueSet,
        modInserted :: UtxoValueSet,
        modUnchanged :: UtxoValueSet
      }
  deriving (Show)

-- | Computes the numeric difference between the new total and the old total
-- of the source and destinations of this diff of set of values
utxoValueSetDiffTotal :: UtxoValueSetDiff -> Pl.Value
utxoValueSetDiffTotal (Inserted ins) = utxoValueSetTotal ins
utxoValueSetDiffTotal (Deleted del) = Pl.negate $ utxoValueSetTotal del
utxoValueSetDiffTotal (Modified del ins _) = utxoValueSetTotal ins <> Pl.negate (utxoValueSetTotal del)

-- | Computes the total difference in value between a new state and an old state:
-- @utxoStateDiff new old@. If this difference is positive for some token @t@,
-- it means some @t@ it was burnt. If it is negative, some @t@ was minted.
utxoStateDiffTotal :: UtxoStateDiff -> Pl.Value
utxoStateDiffTotal = mconcat . map utxoValueSetDiffTotal . M.elems

-- | Computes the difference between two states.
utxoStateDiff :: UtxoState -> UtxoState -> UtxoStateDiff
utxoStateDiff (UtxoState stA) (UtxoState stB) =
  M.union (M.mapWithKey deleteOrModify stA) (M.map Inserted $ M.difference stB stA)
  where
    deleteOrModify :: Pl.Address -> UtxoValueSet -> UtxoValueSetDiff
    deleteOrModify addr valA@(UtxoValueSet a) =
      case M.lookup addr stB of
        Nothing -> Deleted valA
        Just (UtxoValueSet b) ->
          Modified
            { modDeleted = UtxoValueSet $ a L.\\ b,
              modInserted = UtxoValueSet $ b L.\\ a,
              modUnchanged = UtxoValueSet $ a `L.intersect` b
            }

-- | Returns the source of a difference. Satisfies: @utxoStateDiffSrc (utxoStateDiff a b) == a@
utxoStateDiffSrc :: UtxoStateDiff -> UtxoState
utxoStateDiffSrc = UtxoState . M.foldlWithKey keepIfModOrDel M.empty
  where
    keepIfModOrDel :: M.Map Pl.Address UtxoValueSet -> Pl.Address -> UtxoValueSetDiff -> M.Map Pl.Address UtxoValueSet
    keepIfModOrDel m addr (Deleted v) = M.insert addr v m
    keepIfModOrDel m addr (Modified del _ins unch) = M.insert addr (del <> unch) m
    keepIfModOrDel m _ _ = m

-- | Returns the target of a difference. Satisfies: @utxoStateDiffTgt (utxoStateDiff a b) == b@
utxoStateDiffTgt :: UtxoStateDiff -> UtxoState
utxoStateDiffTgt = UtxoState . M.foldlWithKey keepIfModOrIns M.empty
  where
    keepIfModOrIns :: M.Map Pl.Address UtxoValueSet -> Pl.Address -> UtxoValueSetDiff -> M.Map Pl.Address UtxoValueSet
    keepIfModOrIns m addr (Inserted v) = M.insert addr v m
    keepIfModOrIns m addr (Modified _del ins unch) = M.insert addr (ins <> unch) m
    keepIfModOrIns m _ _ = m
