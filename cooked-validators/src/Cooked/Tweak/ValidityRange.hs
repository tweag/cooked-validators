-- | This module defines 'Tweaks' revolving around the validity range of a transaction
module Cooked.Tweak.ValidityRange where

import Control.Monad (guard, void)
import Cooked (awaitTime, currentTime)
import Cooked.Skeleton (txSkelValidityRangeL)
import Cooked.Tweak.Common (MonadTweak, setTweak, viewTweak)
import Plutus.V1.Ledger.Interval (Extended (..), LowerBound (..), always, before, contains, intersection, interval, isEmpty, ivFrom, member, never, singleton)
import Plutus.V2.Ledger.Api (POSIXTime (..), POSIXTimeRange)

getValidityRangeTweak :: MonadTweak m => m POSIXTimeRange
getValidityRangeTweak = viewTweak txSkelValidityRangeL

-- | Changes the current validity range, returning the old one
setValidityRangeTweak :: MonadTweak m => POSIXTimeRange -> m POSIXTimeRange
setValidityRangeTweak newRange = do
  oldRange <- getValidityRangeTweak
  setTweak txSkelValidityRangeL newRange
  return oldRange

-- | Ensures the skeleton makes for an unconstrained validity range
setAlwaysValidRangeTweak :: MonadTweak m => m POSIXTimeRange
setAlwaysValidRangeTweak = setValidityRangeTweak always

-- | Checks if the validity range satisfies a certain predicate
validityRangeSatisfiesTweak :: MonadTweak m => (POSIXTimeRange -> Bool) -> m Bool
validityRangeSatisfiesTweak = (<$> getValidityRangeTweak)

-- | Checks if a given time belongs to the validity range of a transaction
isValidAtTweak :: MonadTweak m => POSIXTime -> m Bool
isValidAtTweak = validityRangeSatisfiesTweak . member

-- | Checks if a given range is included in the validity range of a transaction
isValidDuringTweak :: MonadTweak m => POSIXTimeRange -> m Bool
isValidDuringTweak = validityRangeSatisfiesTweak . flip contains

-- | Checks if the validity range is empty
hasEmptyTimeRangeTweak :: MonadTweak m => m Bool
hasEmptyTimeRangeTweak = validityRangeSatisfiesTweak isEmpty

-- | Checks if the validity range is unconstrained
hasFullTimeRangeTweak :: MonadTweak m => m Bool
hasFullTimeRangeTweak = validityRangeSatisfiesTweak (always ==)

-- | Adds a constraint to the current validity range
-- Returns the old range, and fails is the resulting interval is empty
addToValidityRangeTweak :: MonadTweak m => POSIXTimeRange -> m POSIXTimeRange
addToValidityRangeTweak newRange = do
  oldRange <- viewTweak txSkelValidityRangeL
  let combinedRange = intersection newRange oldRange
  guard (combinedRange /= never)
  setTweak txSkelValidityRangeL combinedRange
  return oldRange

-- | Centers the validity range around a value with a certain radius
centerAroundValidityRangeTweak :: MonadTweak m => POSIXTime -> Integer -> m POSIXTimeRange
centerAroundValidityRangeTweak t r = do
  let radius = POSIXTime r
      left = t - radius
      right = t + radius
      newRange = interval left right
  setValidityRangeTweak newRange

-- | Makes a transaction range equal to a singleton
makeValidityRangeSingleton :: MonadTweak m => POSIXTime -> m POSIXTimeRange
makeValidityRangeSingleton = setValidityRangeTweak . singleton

-- | Makes the transaction validity range comply with the current time
makeValidityRangeNowTweak :: MonadTweak m => m POSIXTimeRange
makeValidityRangeNowTweak = currentTime >>= makeValidityRangeSingleton

-- | Makes current time comply with the current validity range
-- returns the new current time after the modification
-- fails if current time is already after the validity range
makeCurrentTimeInValidityRangeTweak :: MonadTweak m => m POSIXTime
makeCurrentTimeInValidityRangeTweak = do
  now <- currentTime
  vRange <- getValidityRangeTweak
  if member now vRange
    then return now
    else do
      guard $ before now vRange
      guard $ not $ isEmpty vRange
      later <- case ivFrom vRange of
        LowerBound (Finite left) isClosed ->
          return $ left + POSIXTime (toInteger $ fromEnum $ not isClosed)
        _ -> fail "Invalid interval"
      void $ awaitTime later
      return later
