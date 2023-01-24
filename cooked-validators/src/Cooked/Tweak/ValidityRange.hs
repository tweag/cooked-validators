-- | This module defines 'Tweaks' working on the validity range of a transaction
module Cooked.Tweak.ValidityRange where

import Control.Monad (guard)
import Cooked.Skeleton (txSkelValidityRangeL)
import Cooked.Tweak.Common (MonadTweak, setTweak, viewTweak)
import Plutus.V1.Ledger.Interval (always, contains, intersection, member, never)
import Plutus.V2.Ledger.Api (POSIXTime, POSIXTimeRange)

-- | Changes the current validity range, returning the old one
setValidityRangeTweak :: MonadTweak m => POSIXTimeRange -> m POSIXTimeRange
setValidityRangeTweak newRange = do
  oldRange <- viewTweak txSkelValidityRangeL
  setTweak txSkelValidityRangeL newRange
  return oldRange

-- | Ensures the skeleton makes for an unconstrained validity range
setAlwaysValidRangeTweak :: MonadTweak m => m POSIXTimeRange
setAlwaysValidRangeTweak = setValidityRangeTweak always

-- | Checks if the validity range satisfies a certain predicate
validityRangeSatisfiesTweak :: MonadTweak m => (POSIXTimeRange -> Bool) -> m Bool
validityRangeSatisfiesTweak = (<$> viewTweak txSkelValidityRangeL)

-- | Checks if a given time belongs to the validity range of a transaction
isValidAtTweak :: MonadTweak m => POSIXTime -> m Bool
isValidAtTweak = validityRangeSatisfiesTweak . member

-- | Checks if a given range is included in the validity range of a transaction
isValidDuringTweak :: MonadTweak m => POSIXTimeRange -> m Bool
isValidDuringTweak = validityRangeSatisfiesTweak . flip contains

-- | Checks if the validation range is empty
hasEmptyTimeRangeTweak :: MonadTweak m => m Bool
hasEmptyTimeRangeTweak = validityRangeSatisfiesTweak (never ==)

-- | Checks if the validation range is unconstrained
hasFullTimeRangeTweak :: MonadTweak m => m Bool
hasFullTimeRangeTweak = validityRangeSatisfiesTweak (always ==)

-- | Adds a constraint to the current validation range
-- Returns the old range, and fails is the resulting interval is empty
addToValidityRangeTweak :: MonadTweak m => POSIXTimeRange -> m POSIXTimeRange
addToValidityRangeTweak newRange = do
  oldRange <- viewTweak txSkelValidityRangeL
  let combinedRange = intersection newRange oldRange
  guard (combinedRange /= never)
  setTweak txSkelValidityRangeL combinedRange
  return oldRange
