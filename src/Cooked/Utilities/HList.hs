-- | Heterogeneous list utilities used by UTxO search and pretty-printing.
module Cooked.Utilities.HList
  ( HList (..),
    hHead,
    hTail,
    hSingleton,
    FetchByType (..),
  )
where

import Data.Kind

-- | Heterogeneous lists
data HList :: [Type] -> Type where
  HEmpty :: HList '[]
  HCons :: a -> HList l -> HList (a ': l)

instance Eq (HList '[]) where
  _ == _ = True

instance (Eq (HList l), Eq a) => Eq (HList (a ': l)) where
  HCons h t == HCons h' t' = h == h' && t == t'

instance Show (HList '[]) where
  show _ = "[]"

instance (Show (HList l), Show a) => Show (HList (a ': l)) where
  show (HCons h t) = show h <> " : " <> show t

-- | Head of an heterogeneous list
hHead :: HList (a ': l) -> a
hHead (HCons a _) = a

-- | Tail of an heterogeneous list
hTail :: HList (a ': l) -> HList l
hTail (HCons _ l) = l

-- | A singleton wrapped in an 'HList'
hSingleton :: a -> HList '[a]
hSingleton = (`HCons` HEmpty)

-- | A class to fetch a value from an 'HList' by its type. Note that if there
-- are multiple values of the same type in the list, this function will return
-- the first one it finds.
class FetchByType (e :: Type) (l :: [Type]) where
  fetchByType :: HList l -> e

instance {-# OVERLAPPING #-} FetchByType e (e ': l) where
  fetchByType = hHead

instance {-# OVERLAPPABLE #-} (FetchByType e l) => FetchByType e (x ': l) where
  fetchByType (HCons _ tl) = fetchByType tl
