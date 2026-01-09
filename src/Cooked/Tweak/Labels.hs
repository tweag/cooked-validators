-- | This module provides tweaks operating on transaction labels
module Cooked.Tweak.Labels
  ( labelled,
    addLabelTweak,
    removeLabelTweak,
    hasLabelTweak,
    ensureLabelTweak,
    labelled',
  )
where

import Control.Monad
import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.Functor
import Data.Set qualified as Set
import Data.Text (Text)

-- | Adds a label to a 'TxSkel'.
addLabelTweak :: (LabelConstrs lbl, MonadTweak m) => lbl -> m ()
addLabelTweak = overTweak txSkelLabelL . Set.insert . TxSkelLabel

-- | Checks if a given label is present in the 'TxSkel'
hasLabelTweak :: (LabelConstrs lbl, MonadTweak m) => lbl -> m Bool
hasLabelTweak = (viewTweak txSkelLabelL <&>) . Set.member . TxSkelLabel

-- | Ensures a given label is present in the 'TxSkel'
ensureLabelTweak :: (LabelConstrs lbl, MonadTweak m) => lbl -> m ()
ensureLabelTweak = hasLabelTweak >=> guard

-- | Removes a label from a 'TxSkel' when possible, fails otherwise
removeLabelTweak :: (LabelConstrs lbl, MonadTweak m) => lbl -> m ()
removeLabelTweak lbl = do
  ensureLabelTweak lbl
  overTweak txSkelLabelL . Set.delete $ TxSkelLabel lbl

-- | Apply a tweak to a given transaction if it has a specific label. Fails if
-- it does not.
--
-- >
-- > someEndpoint = do
-- >   ...
-- >   validateTxSkel' txSkelTemplate
-- >      { txSkelLabels =
-- >         [ "InitialMinting"
-- >         , "AuctionWorkflow"
-- >         , label SomeLabelType]
-- >      }
-- >
-- > someTest = someEndpoint & eveywhere (labelled SomeLabelType someTweak)
-- > anotherTest = someEndpoint & somewhere (labelled SomeLabelType someTweak)
labelled :: (LabelConstrs lbl, MonadTweak m) => lbl -> m a -> m a
labelled lbl = (ensureLabelTweak lbl >>)

-- | `labelled` specialised to Text labels
--
-- >
-- > someEndpoint = do
-- >   ...
-- >   validateTxSkel' txSkelTemplate
-- >      { txSkelLabels =
-- >         [ "InitialMinting"
-- >         , "AuctionWorkflow"
-- >         , "Spending"
-- >         , label SomeLabelType]
-- >      }
-- >
-- > someTest = someEndpoint & somewhere (labelled' "Spending" doubleSatAttack)
labelled' :: (MonadTweak m) => Text -> m a -> m a
labelled' = labelled
