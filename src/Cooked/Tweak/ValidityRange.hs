-- | This module defines 'Tweak's revolving around the validity range of a
-- transaction
module Cooked.Tweak.ValidityRange
  ( getValidityRangeTweak,
    setValidityRangeTweak,
    setAlwaysValidRangeTweak,
    setValidityStartTweak,
    setValidityEndTweak,
    validityRangeSatisfiesTweak,
    isValidAtTweak,
    isValidNowTweak,
    isValidDuringTweak,
    hasEmptyTimeRangeTweak,
    hasFullTimeRangeTweak,
    intersectValidityRangeTweak,
    centerAroundValidityRangeTweak,
    makeValidityRangeSingletonTweak,
    makeValidityRangeNowTweak,
  )
where

import Control.Monad
import Cooked.MockChain.Read
import Cooked.Skeleton
import Cooked.Tweak.Common
import Ledger.Slot qualified as Ledger
import PlutusLedgerApi.V1.Interval qualified as Api
import Polysemy
import Polysemy.NonDet

-- | Looks up the current validity range of the transaction
getValidityRangeTweak ::
  (Member Tweak effs) =>
  Sem effs Ledger.SlotRange
getValidityRangeTweak = viewTweak txSkelValidityRangeL

-- | Changes the current validity range, returning the old one
setValidityRangeTweak ::
  (Member Tweak effs) =>
  Ledger.SlotRange ->
  Sem effs Ledger.SlotRange
setValidityRangeTweak newRange = do
  oldRange <- getValidityRangeTweak
  setTweak txSkelValidityRangeL newRange
  return oldRange

-- | Ensures the skeleton makes for an unconstrained validity range
setAlwaysValidRangeTweak ::
  (Member Tweak effs) =>
  Sem effs Ledger.SlotRange
setAlwaysValidRangeTweak = setValidityRangeTweak Api.always

-- | Sets the left bound of the validity range. Leaves the right bound unchanged
setValidityStartTweak ::
  (Member Tweak effs) =>
  Ledger.Slot ->
  Sem effs Ledger.SlotRange
setValidityStartTweak left =
  getValidityRangeTweak
    >>= setValidityRangeTweak
      . Api.Interval (Api.LowerBound (Api.Finite left) True)
      . Api.ivTo

-- | Sets the right bound of the validity range. Leaves the left bound unchanged
setValidityEndTweak ::
  (Member Tweak effs) =>
  Ledger.Slot ->
  Sem effs Ledger.SlotRange
setValidityEndTweak right =
  getValidityRangeTweak
    >>= setValidityRangeTweak
      . flip Api.Interval (Api.UpperBound (Api.Finite right) True)
      . Api.ivFrom

-- | Checks if the validity range satisfies a certain predicate
validityRangeSatisfiesTweak ::
  (Member Tweak effs) =>
  (Ledger.SlotRange -> Bool) ->
  Sem effs Bool
validityRangeSatisfiesTweak = (<$> getValidityRangeTweak)

-- | Checks if a given time belongs to the validity range of a transaction
isValidAtTweak ::
  (Member Tweak effs) =>
  Ledger.Slot ->
  Sem effs Bool
isValidAtTweak = validityRangeSatisfiesTweak . Api.member

-- | Checks if the current validity range includes the current time
isValidNowTweak ::
  (Members '[Tweak, MockChainRead] effs) =>
  Sem effs Bool
isValidNowTweak = currentSlot >>= isValidAtTweak

-- | Checks if a given range is included in the validity range of a transaction
isValidDuringTweak ::
  (Member Tweak effs) =>
  Ledger.SlotRange ->
  Sem effs Bool
isValidDuringTweak = validityRangeSatisfiesTweak . flip Api.contains

-- | Checks if the validity range is empty
hasEmptyTimeRangeTweak ::
  (Member Tweak effs) =>
  Sem effs Bool
hasEmptyTimeRangeTweak = validityRangeSatisfiesTweak Api.isEmpty

-- | Checks if the validity range is unconstrained
hasFullTimeRangeTweak ::
  (Member Tweak effs) =>
  Sem effs Bool
hasFullTimeRangeTweak = validityRangeSatisfiesTweak (Api.always ==)

-- | Adds a constraint to the current validity range. Returns the old range, and
-- fails is the resulting interval is empty
intersectValidityRangeTweak ::
  (Members '[Tweak, NonDet] effs) =>
  Ledger.SlotRange ->
  Sem effs Ledger.SlotRange
intersectValidityRangeTweak newRange = do
  oldRange <- viewTweak txSkelValidityRangeL
  let combinedRange = Api.intersection newRange oldRange
  guard (combinedRange /= Api.never)
  setTweak txSkelValidityRangeL combinedRange
  return oldRange

-- | Centers the validity range around a value with a certain radius
centerAroundValidityRangeTweak ::
  (Member Tweak effs) =>
  Ledger.Slot ->
  Integer ->
  Sem effs Ledger.SlotRange
centerAroundValidityRangeTweak t (Ledger.Slot -> radius) = do
  setValidityRangeTweak $ Api.interval (t - radius) (t + radius)

-- | Makes a transaction range equal to a singleton
makeValidityRangeSingletonTweak ::
  (Member Tweak effs) =>
  Ledger.Slot ->
  Sem effs Ledger.SlotRange
makeValidityRangeSingletonTweak = setValidityRangeTweak . Api.singleton

-- | Makes the transaction validity range comply with the current time
makeValidityRangeNowTweak ::
  (Members '[Tweak, MockChainRead] effs) =>
  Sem effs Ledger.SlotRange
makeValidityRangeNowTweak = currentSlot >>= makeValidityRangeSingletonTweak
