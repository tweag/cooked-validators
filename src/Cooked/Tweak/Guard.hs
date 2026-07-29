module Cooked.Tweak.Guard
  ( assertTweak,
    assertPredTweak,
    guardTweak,
    guardPredTweak,
  )
where

import Control.Monad
import Cooked.Skeleton
import Cooked.Tweak.Common
import Optics.Core
import Polysemy
import Polysemy.NonDet

assertTweak ::
  ( Member Tweak effs,
    Is k A_Fold
  ) =>
  Optic' k is TxSkel a ->
  Sem effs Bool
assertTweak = fmap (not . null) . viewAllTweak

assertPredTweak ::
  ( Member Tweak effs,
    Is k A_Fold
  ) =>
  Optic' k is TxSkel a ->
  (a -> Bool) ->
  Sem effs Bool
assertPredTweak (castOptic @A_Fold -> optic) p =
  assertTweak (optic % filtered p)

guardTweak ::
  ( Members '[Tweak, NonDet] effs,
    Is k A_Fold
  ) =>
  Optic' k is TxSkel a ->
  Sem effs ()
guardTweak optic = assertTweak optic >>= guard

guardPredTweak ::
  ( Members '[Tweak, NonDet] effs,
    Is k A_Fold
  ) =>
  Optic' k is TxSkel a ->
  (a -> Bool) ->
  Sem effs ()
guardPredTweak optic p = assertPredTweak optic p >>= guard
