{-# LANGUAGE ImportQualifiedPost #-}

module Cooked.Tweak.Labels
  ( addLabelTweak,
    removeLabelTweak,
    hasLabelTweak,
  )
where

import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.Functor ((<&>))
import Data.Set qualified as Set

-- | Add a label to a 'TxSkel'.
addLabelTweak :: (MonadTweak m, LabelConstrs x) => x -> m ()
addLabelTweak = overTweak txSkelLabelL . Set.insert . TxLabel

-- | Removes a label from a 'TxSkel'
removeLabelTweak :: (MonadTweak m, LabelConstrs x) => x -> m ()
removeLabelTweak = overTweak txSkelLabelL . Set.delete . TxLabel

-- | Checks if a given label is present in the 'TxSkel'
hasLabelTweak :: (MonadTweak m, LabelConstrs x) => x -> m Bool
hasLabelTweak = (viewTweak txSkelLabelL <&>) . Set.member . TxLabel
