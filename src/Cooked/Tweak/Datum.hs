-- | This module provides 'Tweak's that modify the datums of a 'TxSkel'.
module Cooked.Tweak.Datum
  ( TamperedDatumLabel (..),
    tamperDatumsTweak,
    tamperAllDatumsTweak,
  )
where

import Control.Applicative
import Cooked.Pretty.Class
import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Labels
import Optics.Core
import Polysemy
import Polysemy.NonDet

-- | A label added to a 'TxSkel' on which a tweak tampering a datum has been
-- applied. The label contains all the datum contents that have been
-- modified, before the modification was applied.
newtype TamperedDatumLabel a = TamperedDatumLabel [a]
  deriving (Show, Eq, Ord)

instance (PrettyCooked a) => PrettyCooked (TamperedDatumLabel a) where
  prettyCookedOpt opts (TamperedDatumLabel dats) =
    prettyItemize opts "Tampered Datums" "-" dats

-- | Applies a modification to all datums of type @a@ focused by a given
-- optic. Returns the list of modified datums, as they were before being
-- modified.
tamperDatumsTweak ::
  ( DatumConstrs a,
    Ord a,
    DatumConstrs b,
    Members '[NonDet, Tweak] effs,
    Foldable t,
    Alternative t,
    Is k A_Traversal
  ) =>
  -- | The branching options
  Branching ->
  -- | An optic focusing the redeemers to consider
  Optic' k is TxSkel TxSkelOutDatum ->
  -- | The modification to attempt on each typed datum
  (a -> t b) ->
  Sem effs [a]
tamperDatumsTweak branching optic mChange = do
  modified <- modifyTweakFromParams $ modifyTweakParamsAllIndexes branching optic txSkelOutDatumTypedAT mChange
  addLabelTweak $ TamperedDatumLabel modified
  return modified

-- | Same as 'tamperDatumsTweak', focusing all the datums in the 'TxSkel'
tamperAllDatumsTweak ::
  ( DatumConstrs a,
    DatumConstrs b,
    Members '[NonDet, Tweak] effs,
    Foldable t,
    Alternative t,
    Ord a
  ) =>
  -- | The branching options
  Branching ->
  -- | The modification to attempt on each typed datum
  (a -> t b) ->
  Sem effs [a]
tamperAllDatumsTweak branching =
  tamperDatumsTweak branching (txSkelOutputsL % traversed % txSkelOutDatumL)
