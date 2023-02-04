module Cooked.Tweak.ValidityRangeSpec (tests) where

import Control.Monad (join)
import Cooked (MonadTweak, awaitSlot, currentSlot, runTweak, txSkelTemplate)
import Cooked.Tweak.ValidityRange (addToValidityRangeTweak, centerAroundValidityRangeTweak, getValidityRangeTweak, hasFullTimeRangeTweak, isValidAtTweak, isValidDuringTweak, isValidNowTweak, makeCurrentTimeInValidityRangeTweak, makeValidityRangeNowTweak, setValidityRangeTweak)
import Data.Default (def)
import Data.Either (rights)
import Data.Function (on)
import Debug.Trace (trace)
import Ledger.Slot (Slot (Slot), getSlot)
import Plutus.V1.Ledger.Interval (interval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

toSlotRange = interval `on` Slot

toSlotRangeTranslate translation a b =
  toSlotRange
    (getSlot translation + a)
    (getSlot translation + b)

getSingleResult = fst . head . rights . flip runTweak txSkelTemplate

checkIsValidDuring :: MonadTweak m => m Assertion
checkIsValidDuring = do
  b <- hasFullTimeRangeTweak
  b1 <- isValidDuringTweak $ toSlotRange 101 1015
  setValidityRangeTweak $ toSlotRange 101 1015
  b2 <- isValidDuringTweak $ toSlotRange 110 1000
  b3 <- isValidDuringTweak $ toSlotRange 80 1015
  return $
    assertBool "interval inclusions are wrong" $
      b && b1 && b2 && not b3

checkAddToValidityRange :: MonadTweak m => m Assertion
checkAddToValidityRange = do
  timeOrigin <- currentSlot
  centerAroundValidityRangeTweak (timeOrigin + Slot 100) 80
  b <- isValidDuringTweak $ toSlotRangeTranslate timeOrigin 25 35
  b1 <- isValidAtTweak (timeOrigin + Slot 130)
  addToValidityRangeTweak $ toSlotRangeTranslate timeOrigin 110 220
  b2 <- isValidAtTweak (timeOrigin + Slot 130)
  now <- currentSlot
  awaitSlot $ timeOrigin + Slot 130
  b3 <- isValidNowTweak
  awaitSlot $ Slot 200
  b3 <- isValidNowTweak
  makeValidityRangeNowTweak
  b4 <- isValidNowTweak
  return $
    assertBool "interval intersection is wrong" $
      b && b1 && b2 && not b3 && b4

checkMoveCurrentSlot :: MonadTweak m => m Assertion
checkMoveCurrentSlot = do
  setValidityRangeTweak $ toSlotRange 10 20
  makeCurrentTimeInValidityRangeTweak
  b1 <- isValidNowTweak
  setValidityRangeTweak $ toSlotRange 30 60
  makeCurrentTimeInValidityRangeTweak
  b2 <- isValidNowTweak
  setValidityRangeTweak $ toSlotRange 80 100
  makeCurrentTimeInValidityRangeTweak
  b3 <- isValidNowTweak
  return $
    assertBool "Time shift did not occur" $
      b1 && b2 && b3

tests :: TestTree
tests =
  testGroup
    "Validity range tweaks"
    [ testCase "Validity inclusion" $ getSingleResult checkIsValidDuring,
      testCase "Validity intersection" $ getSingleResult checkAddToValidityRange,
      testCase "Time shifting in validity range" $ getSingleResult checkMoveCurrentSlot
    ]
