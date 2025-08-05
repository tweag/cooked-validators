-- | This module exposes the labels that can be used to stamp a
-- 'Cooked.Skeleton.TxSkel' with additional arbitrary pieces of information.
module Cooked.Skeleton.Label
  ( -- * Type constraints
    LabelConstrs,

    -- * Data types
    TxSkelLabel (..),

    -- * Optics
    txSkelLabelTypedP,
  )
where

import Cooked.Pretty.Class
import Data.Typeable (cast)
import Optics.Core
import Type.Reflection

-- | These are type constraints that must be satisfied by labels
type LabelConstrs x = (PrettyCooked x, Show x, Typeable x, Eq x, Ord x)

-- | Labels are arbitrary information that can be added to skeleton. They are
-- meant to be pretty-printed. The common use case we currently have is to tag
-- skeletons that have been modified by tweaks and automated attacks.
data TxSkelLabel where
  TxSkelLabel :: (LabelConstrs x) => x -> TxSkelLabel

instance Eq TxSkelLabel where
  a == x = compare a x == EQ

instance Show TxSkelLabel where
  show (TxSkelLabel x) = show x

instance PrettyCooked TxSkelLabel where
  prettyCookedOpt opts (TxSkelLabel x) = prettyCookedOpt opts x

instance Ord TxSkelLabel where
  compare (TxSkelLabel a) (TxSkelLabel b) =
    maybe
      (compare (SomeTypeRep (typeOf a)) (SomeTypeRep (typeOf b)))
      (compare b)
      (cast a)

-- | A prism to create a label and retrieve a typed content
txSkelLabelTypedP :: (LabelConstrs a) => Prism' TxSkelLabel a
txSkelLabelTypedP =
  prism
    TxSkelLabel
    (\txSkelLabel@(TxSkelLabel lbl) -> maybe (Left txSkelLabel) Right (cast lbl))
