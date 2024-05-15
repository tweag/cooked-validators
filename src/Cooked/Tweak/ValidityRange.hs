-- | This module defines 'Tweaks' revolving around the validity range of a
-- transaction
module Cooked.Tweak.ValidityRange where

import Control.Monad
import Cooked.MockChain
import Cooked.Skeleton
import Cooked.Tweak.Common
import Ledger.Slot qualified as Ledger
import PlutusLedgerApi.V1.Interval qualified as Api

getValidityRangeTweak :: (MonadTweak m) => m Ledger.SlotRange
getValidityRangeTweak = viewTweak txSkelValidityRangeL

-- | Changes the current validity range, returning the old one
setValidityRangeTweak :: (MonadTweak m) => Ledger.SlotRange -> m Ledger.SlotRange
setValidityRangeTweak newRange = do
  oldRange <- getValidityRangeTweak
  setTweak txSkelValidityRangeL newRange
  return oldRange

-- | Ensures the skeleton makes for an unconstrained validity range
setAlwaysValidRangeTweak :: (MonadTweak m) => m Ledger.SlotRange
setAlwaysValidRangeTweak = setValidityRangeTweak Api.always

-- | Sets the left bound of the validity range. Leaves the right bound unchanged
setValidityStartTweak :: (MonadTweak m) => Ledger.Slot -> m Ledger.SlotRange
setValidityStartTweak left = getValidityRangeTweak >>= setValidityRangeTweak . Api.Interval (Api.LowerBound (Api.Finite left) True) . Api.ivTo

-- | Sets the right bound of the validity range. Leaves the left bound unchanged
setValidityEndTweak :: (MonadTweak m) => Ledger.Slot -> m Ledger.SlotRange
setValidityEndTweak right = getValidityRangeTweak >>= setValidityRangeTweak . flip Api.Interval (Api.UpperBound (Api.Finite right) True) . Api.ivFrom

-- | Checks if the validity range satisfies a certain predicate
validityRangeSatisfiesTweak :: (MonadTweak m) => (Ledger.SlotRange -> Bool) -> m Bool
validityRangeSatisfiesTweak = (<$> getValidityRangeTweak)

-- | Checks if a given time belongs to the validity range of a transaction
isValidAtTweak :: (MonadTweak m) => Ledger.Slot -> m Bool
isValidAtTweak = validityRangeSatisfiesTweak . Api.member

-- | Checks if the current validity range includes the current time
isValidNowTweak :: (MonadTweak m) => m Bool
isValidNowTweak = currentSlot >>= isValidAtTweak

-- | Checks if a given range is included in the validity range of a transaction
isValidDuringTweak :: (MonadTweak m) => Ledger.SlotRange -> m Bool
isValidDuringTweak = validityRangeSatisfiesTweak . flip Api.contains

-- | Checks if the validity range is empty
hasEmptyTimeRangeTweak :: (MonadTweak m) => m Bool
hasEmptyTimeRangeTweak = validityRangeSatisfiesTweak Api.isEmpty

-- | Checks if the validity range is unconstrained
hasFullTimeRangeTweak :: (MonadTweak m) => m Bool
hasFullTimeRangeTweak = validityRangeSatisfiesTweak (Api.always ==)

-- | Adds a constraint to the current validity range. Returns the old range, and
-- fails is the resulting interval is empty
intersectValidityRangeTweak :: (MonadTweak m) => Ledger.SlotRange -> m Ledger.SlotRange
intersectValidityRangeTweak newRange = do
  oldRange <- viewTweak txSkelValidityRangeL
  let combinedRange = Api.intersection newRange oldRange
  guard (combinedRange /= Api.never)
  setTweak txSkelValidityRangeL combinedRange
  return oldRange

-- | Centers the validity range around a value with a certain radius
centerAroundValidityRangeTweak :: (MonadTweak m) => Ledger.Slot -> Integer -> m Ledger.SlotRange
centerAroundValidityRangeTweak t r = do
  let radius = Ledger.Slot r
      left = t - radius
      right = t + radius
      newRange = Api.interval left right
  setValidityRangeTweak newRange

-- | Makes a transaction range equal to a singleton
makeValidityRangeSingletonTweak :: (MonadTweak m) => Ledger.Slot -> m Ledger.SlotRange
makeValidityRangeSingletonTweak = setValidityRangeTweak . Api.singleton

-- | Makes the transaction validity range comply with the current time
makeValidityRangeNowTweak :: (MonadTweak m) => m Ledger.SlotRange
makeValidityRangeNowTweak = currentSlot >>= makeValidityRangeSingletonTweak

-- | Makes current time comply with the validity range of the transaction under
-- modification. Returns the new current time after the modification; fails if
-- current time is already after the validity range.
waitUntilValidTweak :: (MonadTweak m) => m Ledger.Slot
waitUntilValidTweak = do
  now <- currentSlot
  vRange <- getValidityRangeTweak
  if Api.member now vRange
    then return now
    else do
      guard $ Api.before now vRange
      guard $ not $ Api.isEmpty vRange
      later <- case Api.ivFrom vRange of
        Api.LowerBound (Api.Finite left) isClosed ->
          return $ left + Ledger.Slot (toInteger $ fromEnum $ not isClosed)
        _ -> error "this should never happen: left-finite interval without left border"
      void $ awaitSlot later
      return later
