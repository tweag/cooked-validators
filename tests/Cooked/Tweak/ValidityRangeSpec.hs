module Cooked.Tweak.ValidityRangeSpec (tests) where

import Control.Monad (join)
import Cooked
import Data.Default (def)
import Data.Either (rights)
import Data.Function (on)
import Debug.Trace (trace)
import Ledger.Slot qualified as Ledger
import PlutusLedgerApi.V1.Interval qualified as Api
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

toSlotRange = Api.interval `on` Ledger.Slot

toSlotRangeTranslate translation a b =
  toSlotRange
    (Ledger.getSlot translation + a)
    (Ledger.getSlot translation + b)

getSingleResult = fst . head . rights . flip runTweak txSkelTemplate

checkIsValidDuring :: (MonadTweak m) => m Assertion
checkIsValidDuring = do
  b <- hasFullTimeRangeTweak
  b1 <- isValidDuringTweak $ toSlotRange 101 1015
  setValidityRangeTweak $ toSlotRange 101 1015
  b2 <- isValidDuringTweak $ toSlotRange 110 1000
  b3 <- isValidDuringTweak $ toSlotRange 80 1015
  return $
    assertBool "interval inclusions are wrong" $
      b && b1 && b2 && not b3

checkAddToValidityRange :: (MonadTweak m) => m Assertion
checkAddToValidityRange = do
  timeOrigin <- currentSlot
  centerAroundValidityRangeTweak (timeOrigin + Ledger.Slot 100) 80
  b <- isValidDuringTweak $ toSlotRangeTranslate timeOrigin 25 35
  b1 <- isValidAtTweak (timeOrigin + Ledger.Slot 130)
  intersectValidityRangeTweak $ toSlotRangeTranslate timeOrigin 110 220
  b2 <- isValidAtTweak (timeOrigin + Ledger.Slot 130)
  now <- currentSlot
  awaitSlot $ timeOrigin + Ledger.Slot 130
  b3 <- isValidNowTweak
  awaitSlot $ Ledger.Slot 200
  b3 <- isValidNowTweak
  makeValidityRangeNowTweak
  b4 <- isValidNowTweak
  return $
    assertBool "interval intersection is wrong" $
      b && b1 && b2 && not b3 && b4

checkMoveCurrentSlot :: (MonadTweak m) => m Assertion
checkMoveCurrentSlot = do
  setValidityRangeTweak $ toSlotRange 10 20
  waitUntilValidTweak
  b1 <- (\now -> now >= 10 && now <= 20) <$> currentSlot
  b2 <- isValidNowTweak
  setValidityRangeTweak $ toSlotRange 15 25
  waitUntilValidTweak
  b3 <- (\now -> now >= 15 && now <= 25) <$> currentSlot
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
