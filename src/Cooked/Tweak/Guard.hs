module Cooked.Tweak.Guard
  ( assertTweak,
    assertPredTweak,
    guardTweak,
    guardPredTweak,
    labelled,
    labelled',
  )
where

import Control.Monad
import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Query
import Data.Text (Text)
import Optics.Core
import Polysemy
import Polysemy.NonDet

assertTweak ::
  ( Member Tweak effs,
    Is k A_Fold
  ) =>
  Optic' k is TxSkel a ->
  Sem effs Bool
assertTweak = fmap (not . null) . toListOfTweak

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

-- | Apply a tweak to a given transaction if it has a specific label. Fails if
-- it does not.
--
-- >
-- > someEndpoint = do
-- >   ...
-- >   validateTxSkel' txSkelTemplate
-- >      { txSkelLabels =
-- >         [ TxSkelLabel "InitialMinting"
-- >         , TxSkelLabel "AuctionWorkflow"
-- >         , TxSkelLabel SomeLabelType]
-- >      }
-- >
-- > someTest = someEndpoint & eveywhere (labelled SomeLabelType someTweak)
-- > anotherTest = someEndpoint & somewhere (labelled SomeLabelType someTweak)
labelled ::
  ( LabelConstrs lbl,
    Members '[Tweak, NonDet] effs
  ) =>
  lbl ->
  Sem effs a ->
  Sem effs a
labelled lbl = (guardTweak (txSkelLabelsL % at (TxSkelLabel lbl) % _Just) >>)

-- | `labelled` specialised to Text labels
--
-- >
-- > someEndpoint = do
-- >   ...
-- >   validateTxSkel' txSkelTemplate
-- >      { txSkelLabels =
-- >         [ TxSkelLabel "InitialMinting"
-- >         , TxSkelLabel "AuctionWorkflow"
-- >         , TxSkelLabel "Spending"
-- >         , TxSkelLabel SomeLabelType]
-- >      }
-- >
-- > someTest = someEndpoint & somewhere (labelled' "Spending" doubleSatAttack)
labelled' ::
  (Members '[Tweak, NonDet] effs) =>
  Text ->
  Sem effs a ->
  Sem effs a
labelled' = labelled
