-- | This module exposes tweaks revolving around parts of a 'TxSkel' satisfying
-- given conditions. The only parameter these tweaks take is an optic, and the
-- guards ensure that at least one focus is targeted by it. This might look
-- insufficient, but thanks to @filtered@ which turns a predicate into an
-- optic, this is actually sufficiently expressive. For example, if you have an
-- optic @o@ targeting an element of type @a@, and a predicate @p@ and would
-- like to ensure the targeted elements satisfy @p@, use @o % filtered p@.
module Cooked.Tweak.Guard
  ( -- * Standard guarding tweaks
    assertTweak,
    guardTweak,
    condTweak,

    -- * Custom guarding tweaks
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

-- | Asserts whether a given optic targets at least one focus
assertTweak ::
  ( Member Tweak effs,
    Is k A_Fold
  ) =>
  Optic' k is TxSkel a ->
  Sem effs Bool
assertTweak = fmap (not . null) . toListOfTweak

-- | Ensures a given optic targets at least one focus, failing otherwise
guardTweak ::
  ( Members '[Tweak, NonDet] effs,
    Is k A_Fold
  ) =>
  Optic' k is TxSkel a ->
  Sem effs ()
guardTweak optic = assertTweak optic >>= guard

-- | Only executes the given computation provided the given optic targets at
-- least one focus, failing otherwise.
condTweak ::
  ( Members '[Tweak, NonDet] effs,
    Is k A_Fold
  ) =>
  Optic' k is TxSkel a ->
  Sem effs b ->
  Sem effs b
condTweak optic = (guardTweak optic >>)

-- | Apply a tweak to a given transaction if it has a specific label. Fails if
-- it does not.
--
-- >
-- > someEndpoint = do
-- >   ...
-- >   validateTxSkel' txSkelEmulatorTemplate
-- >      { txSkelLabels =
-- >         [ TxSkelLabel "InitialMinting"
-- >         , TxSkelLabel "AuctionWorkflow"
-- >         , TxSkelLabel SomeLabelType]
-- >      }
-- >
-- > someTest = someEndpoint & everywhere (labelled SomeLabelType someTweak)
-- > anotherTest = someEndpoint & somewhere (labelled SomeLabelType someTweak)
labelled ::
  ( LabelConstrs lbl,
    Members '[Tweak, NonDet] effs
  ) =>
  lbl ->
  Sem effs a ->
  Sem effs a
labelled lbl = condTweak $ txSkelLabelsL % at (TxSkelLabel lbl) % _Just

-- | `labelled` specialised to Text labels
--
-- >
-- > someEndpoint = do
-- >   ...
-- >   validateTxSkel' txSkelEmulatorTemplate
-- >      { txSkelLabels =
-- >         [ TxSkelLabel "InitialMinting"
-- >         , TxSkelLabel "AuctionWorkflow"
-- >         , TxSkelLabel "Spending"
-- >         , TxSkelLabel SomeLabelType]
-- >      }
-- >
-- > someTest = someEndpoint & somewhere (labelled' "Spending" someTweak)
labelled' ::
  (Members '[Tweak, NonDet] effs) =>
  Text ->
  Sem effs a ->
  Sem effs a
labelled' = labelled
