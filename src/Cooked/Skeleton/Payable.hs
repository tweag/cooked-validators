{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cooked.Skeleton.Payable
  ( Payable (..),
    (<&&>),
  )
where

import Cooked.Conversion
import Cooked.Skeleton.Datum
import Data.Kind (Constraint, Type)
import GHC.TypeLits

-- | Will be lifted at type level to represent which payable elements have
-- already been established.
data Elems = Dat | RefScript | Val | StCred

-- | Constraints that a given element does not appear in a list of elements
type family IsNew (new :: a) (existing :: [a]) :: Constraint where
  IsNew x '[] = () ~ ()
  IsNew x (x ': xs) = TypeError ('Text "Tag already existing " ':<>: 'ShowType x)
  IsNew x (_ ': xs) = IsNew x xs

-- | Contrainst that a given list of elements is disjoint from another
type family AllNew (news :: [a]) (existing :: [a]) :: Constraint where
  AllNew '[] _ = ()
  AllNew (x ': xs) ys = (IsNew x ys, AllNew xs ys)

-- | Union (with duplicates, which will not occur by construction in the
-- concrete implentation of @Payable@ due to the @AllNew@ constraint.
type family (∪) (xs :: [a]) (ys :: [a]) :: [a] where
  '[] ∪ ys = ys
  (x ': xs) ∪ ys = x ': (xs ∪ ys)

-- | Payable elements. Created from concrete elements or composed. Notice that
-- there is no way of building an element of Type @Payable '[]@ so when using an
-- element of Type @Payable els@ we are sure that something was in fact paid.
data Payable :: [Elems] -> Type where
  -- | Hashed datums visible in the transaction are payable
  VisibleHashedDatum :: (TxSkelOutDatumConstrs a) => a -> Payable '[Dat]
  -- | Inline datums are payable
  InlineDatum :: (TxSkelOutDatumConstrs a) => a -> Payable '[Dat]
  -- | Hashed datums hidden from the transaction are payable
  HiddenHashedDatum :: (TxSkelOutDatumConstrs a) => a -> Payable '[Dat]
  -- | Reference scripts are payable
  ReferenceScript :: (ToVersionedScript s) => s -> Payable '[RefScript]
  -- | Values are payable
  Value :: (ToValue a) => a -> Payable '[Val]
  -- | Staking credentials are payable
  StakingCredential :: (ToMaybeStakingCredential cred) => cred -> Payable '[StCred]
  -- | Payables can be combined as long as their list of tags are disjoint
  PayableAnd :: (AllNew els els') => Payable els -> Payable els' -> Payable (els ∪ els')

-- | Basically re-exporting @PayableAnd@ as a builtin operator
(<&&>) :: (AllNew els els') => Payable els -> Payable els' -> Payable (els ∪ els')
(<&&>) = PayableAnd
