-- | This module provides tweaks operating on transaction labels
module Cooked.Tweak.Labels
  ( labelled,
    labelled',
    addLabelTweak,
    removeLabelTweak,
    hasLabelTweak,
    ensureLabelTweak,
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

-- | `labelled'` specialised to Text labels.
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
-- > someTest = someEndpoint & somewhere (labelled "Spending" doubleSatAttack)
labelled :: (MonadTweak m) => Text -> m a -> m a
labelled = labelled'

-- | Apply a tweak to a given transaction if it has a specific label. This
-- can be useful to apply a tweak to every (or any) transaction in a set of
-- associated transactions.
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
-- > someTest = someEndpoint & eveywhere (labelled' SomeLabelType someTweak)
-- > anotherTest = someEndpoint & somewhere (labelled' SomeLabelType someTweak)
labelled' :: (MonadTweak m, LabelConstrs lbl) => lbl -> m a -> m a
labelled' lbl = (ensureLabelTweak lbl >>)
