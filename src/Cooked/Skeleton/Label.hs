-- | This module exposes the labels that can be used to stamp
-- 'Cooked.Skeleton.TxSkel' with additional arbitrary pieces of information.
module Cooked.Skeleton.Label
  ( LabelConstrs,
    TxLabel (..),
  )
where

import Cooked.Pretty.Common
import Type.Reflection

-- | These are type constraints that must be satisfied by labels
type LabelConstrs x = (PrettyCooked x, Show x, Typeable x, Eq x, Ord x)

-- | Labels are arbitrary information that can be added to skeleton. They are
-- meant to be pretty-printed. The common use case we currently have is to tag
-- skeletons that have been modified by tweaks and automated attacks.
data TxLabel where
  TxLabel :: (LabelConstrs x) => x -> TxLabel

instance Eq TxLabel where
  a == x = compare a x == EQ

instance Show TxLabel where
  show (TxLabel x) = show x

instance PrettyCooked TxLabel where
  prettyCookedOpt opts (TxLabel x) = prettyCookedOpt opts x

instance Ord TxLabel where
  compare (TxLabel a) (TxLabel x) =
    case compare (SomeTypeRep (typeOf a)) (SomeTypeRep (typeOf x)) of
      LT -> LT
      GT -> GT
      EQ -> case typeOf a `eqTypeRep` typeOf x of
        Just HRefl -> compare a x
        -- This can never happen, since 'eqTypeRep' is implemented in terms of
        -- '==' on the type representation:
        Nothing -> error "Type representations compare as EQ, but are not eqTypeRep"
