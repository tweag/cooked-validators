-- | This module provides tweaks operating on transaction labels
module Cooked.Tweak.Labels
  ( labelled,
    labelled',
    addLabelTweak,
    removeLabelTweak,
    hasLabelTweak,
    ensureLabelTweak,
    labelledT,
    labelledT',
  )
where

import Control.Monad
import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.Functor
import Data.Set qualified as Set
import Data.Text (Text)

-- | Adds a label to a 'TxSkel'.
addLabelTweak :: (MonadTweak m, LabelConstrs x) => x -> m ()
addLabelTweak = overTweak txSkelLabelL . Set.insert . TxSkelLabel

-- | Checks if a given label is present in the 'TxSkel'
hasLabelTweak :: (MonadTweak m, LabelConstrs x) => x -> m Bool
hasLabelTweak = (viewTweak txSkelLabelL <&>) . Set.member . TxSkelLabel

-- | Ensures a given label is present in the 'TxSkel'
ensureLabelTweak :: (MonadTweak m, LabelConstrs x) => x -> m ()
ensureLabelTweak = hasLabelTweak >=> guard

-- | Removes a label from a 'TxSkel' when possible, fails otherwise
removeLabelTweak :: (MonadTweak m, LabelConstrs x) => x -> m ()
removeLabelTweak lbl = do
  ensureLabelTweak lbl
  overTweak txSkelLabelL . Set.delete $ TxSkelLabel lbl

-- | Apply a tweak to a given transaction if it has a specific label. Fails if
-- it does not.This can be useful to apply a tweak to any transaction in a trace
-- using 'Cooked.MockChain.Staged.somewhere'.
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
labelled :: (MonadTweak m, LabelConstrs lbl) => lbl -> m a -> m a
labelled lbl = (ensureLabelTweak lbl >>)

-- | Similar to 'labelled', but does not fail when the label is not present,
-- thus making this tweak suitable to be used with
-- 'Cooked.MockChain.Staged.everywhere'
labelled' :: (MonadTweak m, LabelConstrs lbl) => lbl -> m a -> m ()
labelled' lbl tweak = hasLabelTweak lbl >>= (`when` void tweak)

-- | `labelled'` specialised to Text labels
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
-- > someTest = someEndpoint & somewhere (labelledT "Spending" doubleSatAttack)
labelledT :: (MonadTweak m) => Text -> m a -> m a
labelledT = labelled

-- | 'labelledT' specialised to Text labels
labelledT' :: (MonadTweak m) => Text -> m a -> m ()
labelledT' = labelled'
