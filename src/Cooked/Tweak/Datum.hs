module Cooked.Tweak.Datum
  ( -- * Optic-based datum modifications
    tamperDatumsOfTypeTweak,
    tamperDatumsOfTypeTweakAny,

    -- * All-position datum modifications
    tamperAllDatumsOfTypeTweak,
    tamperAllDatumsOfTypeTweakAny,

    -- * Tampered label
    TamperedDatumLbl (..),
  )
where

import Control.Applicative
import Cooked.Pretty.Class
import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Labels
import Data.Maybe
import Optics.Core
import Polysemy
import Polysemy.NonDet

-- | A label added to a 'TxSkel' on which a tweak tampering a datum has been
-- applied. The label contains all the datum contents that have been
-- modified, before the modification was applied.
newtype TamperedDatumLbl a = TamperedDatumLbl [a]
  deriving (Show, Eq, Ord)

instance (PrettyCooked a) => PrettyCooked (TamperedDatumLbl a) where
  prettyCookedOpt opts (TamperedDatumLbl dats) =
    prettyItemize opts "Tampered Datums" "-" dats

-- | Applies a modification to all datums of type @a@ focused by a given
-- optic. Returns the list of modified datums, as they were before being
-- modified.
tamperDatumsOfTypeTweak ::
  forall a b k is t effs.
  ( DatumConstrs a,
    Ord a,
    DatumConstrs b,
    Members '[NonDet, Tweak] effs,
    Foldable t,
    Alternative t,
    Is k A_Traversal
  ) =>
  -- | An optic focusing the redeemers to consider
  Optic' k is TxSkel TxSkelOutDatum ->
  -- | The modification to attempt on each typed redeemer
  (a -> t b) ->
  Sem effs [TxSkelOutDatum]
tamperDatumsOfTypeTweak optic mChange = do
  modified <- overModsTweakAll optic $ embedTypeChange txSkelOutDatumTypedAT mChange
  addLabelTweak $ TamperedDatumLbl $ fromJust . preview (txSkelOutDatumTypedAT @a) <$> modified
  return modified

-- | Same as 'tamperDatumsOfTypeTweak' but branches on each focus
tamperDatumsOfTypeTweakAny ::
  forall a b k is t effs.
  ( DatumConstrs a,
    Ord a,
    DatumConstrs b,
    Members '[NonDet, Tweak] effs,
    Foldable t,
    Alternative t,
    Is k A_Traversal
  ) =>
  -- | An optic focusing the redeemers to consider
  Optic' k is TxSkel TxSkelOutDatum ->
  -- | The modification to attempt on each typed redeemer
  (a -> t b) ->
  Sem effs [TxSkelOutDatum]
tamperDatumsOfTypeTweakAny optic mChange = do
  modified <- overModsTweakAny optic $ embedTypeChange txSkelOutDatumTypedAT mChange
  addLabelTweak $ TamperedDatumLbl $ fromJust . preview (txSkelOutDatumTypedAT @a) <$> modified
  return modified

-- | Same as 'tamperDatumsOfTypeTweak', focusing all the datums in the 'TxSkel'
tamperAllDatumsOfTypeTweak ::
  ( DatumConstrs a,
    DatumConstrs b,
    Members '[NonDet, Tweak] effs,
    Foldable t,
    Alternative t,
    Ord a
  ) =>
  (a -> t b) ->
  Sem effs [TxSkelOutDatum]
tamperAllDatumsOfTypeTweak =
  tamperDatumsOfTypeTweak (txSkelOutputsL % traversed % txSkelOutDatumL)

-- | Same as 'tamperDatumsOfTypeTweakAny', focusing all the datums in the
-- 'TxSkel'
tamperAllDatumsOfTypeTweakAny ::
  ( DatumConstrs a,
    DatumConstrs b,
    Members '[NonDet, Tweak] effs,
    Foldable t,
    Alternative t,
    Ord a
  ) =>
  (a -> t b) ->
  Sem effs [TxSkelOutDatum]
tamperAllDatumsOfTypeTweakAny =
  tamperDatumsOfTypeTweakAny (txSkelOutputsL % traversed % txSkelOutDatumL)
