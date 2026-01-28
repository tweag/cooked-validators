{-# LANGUAGE UndecidableInstances #-}

-- | This module exposes some type families used to either directly constraint
-- values within our skeletons, or constrant inputs of smart constructors for
-- components of these skeletons.
module Cooked.Families
  ( -- * Type-level constraints
    type (∈),
    type (∉),
    type (⩀),

    -- * Main type-level functions
    type (∪),
    type Rev,
    type (++),

    -- * Auxiliary type-level functions
    type RevAux,
    type Member,
    type NonMember,

    -- * Heterogeneous lists
    HList (..),
    hHead,
    hTail,
  )
where

import Data.Kind
import GHC.TypeLits

-- | Reverses a type level with an accumulator
type family RevAux (els :: [a]) (done :: [a]) :: [a] where
  RevAux '[] done = done
  RevAux (x ': xs) done = RevAux xs (x ': done)

-- | Reverses a type level list starting with an empty accumulator
type Rev els = RevAux els '[]

-- | Type level append
type family (++) (els :: [a]) (els' :: [a]) :: [a] where
  '[] ++ els' = els'
  (x ': xs) ++ els' = (x ': xs ++ els')

-- | Type level list union with duplicates
type family (∪) (xs :: [a]) (ys :: [a]) :: [a] where
  '[] ∪ ys = ys
  (x ': xs) ∪ ys = x ': (xs ∪ ys)

-- | A type family representing membership. This requires @UndecidableInstances@
-- because the type checker is not smart enough to understand that this type
-- family decreases in @els@, due to the presence of @extras@. @extras@ is used
-- to keep track of the original list and output a relevant message in the empty
-- case, which could otherwise be omitted altogther at no loss of type safety.
type family Member (el :: a) (els :: [a]) (extras :: [a]) :: Constraint where
  Member x (x ': xs) _ = ()
  Member x (y ': xs) l = Member x xs (y ': l)
  Member x '[] l = TypeError ('ShowType x ':<>: 'Text " is not a member of " ':<>: 'ShowType (Rev l))

-- | Type level list membership
type (∈) el els = Member el els '[]

-- | A type family representing non membership. @extra@ is used to keep track of
-- the already browsed to output a relevant message. It could be omitted with no
-- loss of type safety.
type family NonMember (el :: a) (els :: [a]) (extras :: [a]) :: Constraint where
  NonMember x '[] _ = ()
  NonMember x (x ': xs) extras = TypeError ('ShowType x ':<>: 'Text " is already a member of " ':<>: 'ShowType (Rev extras ++ (x ': xs)))
  NonMember x (x' ': xs) extras = NonMember x xs (x' ': extras)

-- | Type level list non-membership
type (∉) el els = NonMember el els '[]

-- | Type level disjunction check between lists
type family (⩀) (els :: [a]) (els' :: [a]) :: Constraint where
  '[] ⩀ _ = ()
  (x ': xs) ⩀ ys = (x ∉ ys, xs ⩀ ys)

-- | Heterogeneous lists
data HList :: [Type] -> Type where
  HEmpty :: HList '[]
  HCons :: a -> HList l -> HList (a ': l)

-- | Head of an heterogeneous list
hHead :: HList (a ': l) -> a
hHead (HCons a _) = a

-- | Tail of an heterogeneous list
hTail :: HList (a ': l) -> HList l
hTail (HCons _ l) = l

instance Eq (HList '[]) where
  _ == _ = True

instance (Eq (HList l), Eq a) => Eq (HList (a ': l)) where
  HCons h t == HCons h' t' = h == h' && t == t'

instance Show (HList '[]) where
  show _ = "[]"

instance (Show (HList l), Show a) => Show (HList (a ': l)) where
  show (HCons h t) = show h <> " : " <> show t
