-- | This module defines 'Tweaks' revolving around the validity range of a transaction
module Cooked.Tweak.ValidityRange where

import Control.Monad (guard, void)
import Cooked (awaitSlot, currentSlot)
import Cooked.Skeleton (txSkelValidityRangeL)
import Cooked.Tweak.Common (MonadTweak, setTweak, viewTweak)
import Ledger.Slot (Slot (Slot), SlotRange)
import Plutus.V1.Ledger.Interval (before, contains, intersection, interval, isEmpty, member, never, singleton)
import Plutus.V2.Ledger.Api (Extended (Finite), LowerBound (LowerBound), always, ivFrom)

getValidityRangeTweak :: MonadTweak m => m SlotRange
getValidityRangeTweak = viewTweak txSkelValidityRangeL

-- | Changes the current validity range, returning the old one
setValidityRangeTweak :: MonadTweak m => SlotRange -> m SlotRange
setValidityRangeTweak newRange = do
  oldRange <- getValidityRangeTweak
  setTweak txSkelValidityRangeL newRange
  return oldRange

-- | Ensures the skeleton makes for an unconstrained validity range
setAlwaysValidRangeTweak :: MonadTweak m => m SlotRange
setAlwaysValidRangeTweak = setValidityRangeTweak always

-- | Checks if the validity range satisfies a certain predicate
validityRangeSatisfiesTweak :: MonadTweak m => (SlotRange -> Bool) -> m Bool
validityRangeSatisfiesTweak = (<$> getValidityRangeTweak)

-- | Checks if a given time belongs to the validity range of a transaction
isValidAtTweak :: MonadTweak m => Slot -> m Bool
isValidAtTweak = validityRangeSatisfiesTweak . member

-- | Checks if the current validity range includes the current time
isValidNowTweak :: MonadTweak m => m Bool
isValidNowTweak = currentSlot >>= isValidAtTweak

-- | Checks if a given range is included in the validity range of a transaction
isValidDuringTweak :: MonadTweak m => SlotRange -> m Bool
isValidDuringTweak = validityRangeSatisfiesTweak . flip contains

-- | Checks if the validity range is empty
hasEmptyTimeRangeTweak :: MonadTweak m => m Bool
hasEmptyTimeRangeTweak = validityRangeSatisfiesTweak isEmpty

-- | Checks if the validity range is unconstrained
hasFullTimeRangeTweak :: MonadTweak m => m Bool
hasFullTimeRangeTweak = validityRangeSatisfiesTweak (always ==)

-- | Adds a constraint to the current validity range
-- Returns the old range, and fails is the resulting interval is empty
intersectValidityRangeTweak :: MonadTweak m => SlotRange -> m SlotRange
intersectValidityRangeTweak newRange = do
  oldRange <- viewTweak txSkelValidityRangeL
  let combinedRange = intersection newRange oldRange
  guard (combinedRange /= never)
  setTweak txSkelValidityRangeL combinedRange
  return oldRange

-- | Centers the validity range around a value with a certain radius
centerAroundValidityRangeTweak :: MonadTweak m => Slot -> Integer -> m SlotRange
centerAroundValidityRangeTweak t r = do
  let radius = Slot r
      left = t - radius
      right = t + radius
      newRange = interval left right
  setValidityRangeTweak newRange

-- | Makes a transaction range equal to a singleton
makeValidityRangeSingletonTweak :: MonadTweak m => Slot -> m SlotRange
makeValidityRangeSingletonTweak = setValidityRangeTweak . singleton

-- | Makes the transaction validity range comply with the current time
makeValidityRangeNowTweak :: MonadTweak m => m SlotRange
makeValidityRangeNowTweak = currentSlot >>= makeValidityRangeSingletonTweak

-- | Makes current time comply with the validity range of the transaction under
-- modification. Returns the new current time after the modification; fails if
-- current time is already after the validity range.
waitUntilValidTweak :: MonadTweak m => m Slot
waitUntilValidTweak = do
  now <- currentSlot
  vRange <- getValidityRangeTweak
  if member now vRange
    then return now
    else do
      guard $ before now vRange
      guard $ not $ isEmpty vRange
      later <- case ivFrom vRange of
        LowerBound (Finite left) isClosed ->
          return $ left + Slot (toInteger $ fromEnum $ not isClosed)
        _ -> error "this should never happen: left-finite interval without left border"
      void $ awaitSlot later
      return later
