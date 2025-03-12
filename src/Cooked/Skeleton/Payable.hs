module Cooked.Skeleton.Payable
  ( Payable (..),
    (<&&>),
  )
where

import Cooked.Conversion
import Cooked.Skeleton.Datum
import Data.Kind (Constraint, Type)
import GHC.TypeLits

-- | Constraint that a given type does not appear in a list of types
type family (∉) (el :: a) (els :: [a]) :: Constraint where
  x ∉ '[] = ()
  x ∉ (x ': xs) = TypeError ('Text "Cannot have two payable elements of type: " ':<>: 'ShowType x)
  x ∉ (_ ': xs) = x ∉ xs

-- | Disjoint lists of types
type family (⩀) (els :: [a]) (els' :: [a]) :: Constraint where
  '[] ⩀ _ = ()
  (x ': xs) ⩀ ys = (x ∉ ys, xs ⩀ ys)

-- | Union with duplicates, which will not occur by construction in the
-- concrete implentation of @Payable@ due to the @⩀@ constraint.
type family (∪) (xs :: [a]) (ys :: [a]) :: [a] where
  '[] ∪ ys = ys
  (x ': xs) ∪ ys = x ': (xs ∪ ys)

-- | Payable elements. Created from concrete elements or composed. Notice that
-- there is no way of building an element of Type @Payable '[]@ so when using an
-- element of Type @Payable els@ we are sure that something was in fact paid.
data Payable :: [Symbol] -> Type where
  -- | Hashed datums visible in the transaction are payable
  VisibleHashedDatum :: (TxSkelOutDatumConstrs a) => a -> Payable '["Datum"]
  -- | Inline datums are payable
  InlineDatum :: (TxSkelOutDatumConstrs a) => a -> Payable '["Datum"]
  -- | Hashed datums hidden from the transaction are payable
  HiddenHashedDatum :: (TxSkelOutDatumConstrs a) => a -> Payable '["Datum"]
  -- | Reference scripts are payable
  ReferenceScript :: (ToVersionedScript s) => s -> Payable '["Reference Script"]
  -- | Values are payable
  Value :: (ToValue a) => a -> Payable '["Value"]
  -- | Staking credentials are payable
  StakingCredential :: (ToMaybeStakingCredential cred) => cred -> Payable '["Staking Credential"]
  -- | Payables can be combined as long as their list of tags are disjoint
  PayableAnd :: (els ⩀ els') => Payable els -> Payable els' -> Payable (els ∪ els')

-- | Basically re-exporting @PayableAnd@ as a builtin operator
(<&&>) :: (els ⩀ els') => Payable els -> Payable els' -> Payable (els ∪ els')
(<&&>) = PayableAnd
