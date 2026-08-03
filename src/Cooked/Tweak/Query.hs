-- | This module provides tweaks that query the stored 'TxSkel' based on
-- various kinds of optics.
module Cooked.Tweak.Query
  ( -- * Querying via a getter
    viewTweak,
    iviewTweak,

    -- * Querying via a fold
    toListOfTweak,
    itoListOfTweak,

    -- * Querying via an affine fold
    previewTweak,
    ipreviewTweak,
  )
where

import Cooked.Skeleton
import Cooked.Tweak.Common
import Optics.Core
import Polysemy

-- | Retrieves the focus from the 'TxSkel' given a getter
viewTweak ::
  (Member Tweak effs, Is k A_Getter) =>
  Optic' k is TxSkel a ->
  Sem effs a
viewTweak optic = view optic <$> getTxSkel

-- | Like 'viewTweak', for indexed optics
iviewTweak ::
  (Member Tweak effs, Is k A_Getter) =>
  Optic' k (WithIx i) TxSkel a ->
  Sem effs (i, a)
iviewTweak optic = iview optic <$> getTxSkel

-- | Like 'viewTweak', but returns all the foci as a list
toListOfTweak ::
  (Member Tweak effs, Is k A_Fold) =>
  Optic' k is TxSkel a ->
  Sem effs [a]
toListOfTweak optic = toListOf optic <$> getTxSkel

-- | Like 'toListOfTweak', for indexed optics
itoListOfTweak ::
  (Member Tweak effs, Is k A_Fold) =>
  Optic' k (WithIx i) TxSkel a ->
  Sem effs [(i, a)]
itoListOfTweak optic = itoListOf optic <$> getTxSkel

-- | Like 'viewTweak', but the foci might not exist
previewTweak ::
  (Member Tweak effs, Is k An_AffineFold) =>
  Optic' k is TxSkel a ->
  Sem effs (Maybe a)
previewTweak optic = preview optic <$> getTxSkel

-- | Like 'previewTweak', for indexed optics
ipreviewTweak ::
  (Member Tweak effs, Is k An_AffineFold) =>
  Optic' k (WithIx i) TxSkel a ->
  Sem effs (Maybe (i, a))
ipreviewTweak optic = ipreview optic <$> getTxSkel
