-- | This module provides tweaks operating on transaction labels
module Cooked.Tweak.Labels
  ( addLabelTweak,
    removeLabelTweak,
    hasLabelTweak,
  )
where

import Control.Monad
import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.Functor
import Data.Set qualified as Set

-- | Adds a label to a 'TxSkel'.
addLabelTweak :: (MonadTweak m, LabelConstrs x) => x -> m ()
addLabelTweak = overTweak txSkelLabelL . Set.insert . TxSkelLabel

-- | Checks if a given label is present in the 'TxSkel'
hasLabelTweak :: (MonadTweak m, LabelConstrs x) => x -> m Bool
hasLabelTweak = (viewTweak txSkelLabelL <&>) . Set.member . TxSkelLabel

-- | Removes a label from a 'TxSkel' when possible, fails otherwise
removeLabelTweak :: (MonadTweak m, LabelConstrs x) => x -> m ()
removeLabelTweak label = do
  hasLabelTweak label >>= guard
  overTweak txSkelLabelL . Set.delete $ TxSkelLabel label
