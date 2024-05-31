module Cooked.Tweak.ValidityRangeSpec (tests) where

import Control.Monad
import Cooked
import Data.Either (rights)
import Data.Function (on)
import Ledger.Slot qualified as Ledger
import PlutusLedgerApi.V1.Interval qualified as Api
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

toSlotRange :: Integer -> Integer -> Api.Interval Ledger.Slot
toSlotRange = Api.interval `on` Ledger.Slot

toSlotRangeTranslate :: Ledger.Slot -> Integer -> Integer -> Api.Interval Ledger.Slot
toSlotRangeTranslate translation a b =
  toSlotRange
    (Ledger.getSlot translation + a)
    (Ledger.getSlot translation + b)

checkIsValidDuring :: (MonadTweak m) => m Assertion
checkIsValidDuring = do
  b <- hasFullTimeRangeTweak
  b1 <- isValidDuringTweak $ toSlotRange 101 1015
  void $ setValidityRangeTweak $ toSlotRange 101 1015
  b2 <- isValidDuringTweak $ toSlotRange 110 1000
  b3 <- isValidDuringTweak $ toSlotRange 80 1015
  return $
    assertBool "interval inclusions are wrong" $
      b && b1 && b2 && not b3

checkAddToValidityRange :: (MonadTweak m) => m Assertion
checkAddToValidityRange = do
  timeOrigin <- currentSlot
  void $ centerAroundValidityRangeTweak (timeOrigin + Ledger.Slot 100) 80
  b <- isValidDuringTweak $ toSlotRangeTranslate timeOrigin 25 35
  b1 <- isValidAtTweak (timeOrigin + Ledger.Slot 130)
  void $ intersectValidityRangeTweak $ toSlotRangeTranslate timeOrigin 110 220
  b2 <- isValidAtTweak (timeOrigin + Ledger.Slot 130)
  void $ awaitSlot $ timeOrigin + Ledger.Slot 130
  b3 <- isValidNowTweak
  void $ awaitSlot $ Ledger.Slot 200
  b4 <- isValidNowTweak
  void makeValidityRangeNowTweak
  b5 <- isValidNowTweak
  return $
    assertBool "interval intersection is wrong" $
      b && b1 && b2 && b3 && not b4 && b5

checkMoveCurrentSlot :: (MonadTweak m) => m Assertion
checkMoveCurrentSlot = do
  void $ setValidityRangeTweak $ toSlotRange 10 20
  void waitUntilValidTweak
  b1 <- (\now -> now >= 10 && now <= 20) <$> currentSlot
  b2 <- isValidNowTweak
  void $ setValidityRangeTweak $ toSlotRange 15 25
  void waitUntilValidTweak
  b3 <- (\now -> now >= 15 && now <= 25) <$> currentSlot
  return $ assertBool "Time shift did not occur" $ b1 && b2 && b3

tests :: TestTree
tests =
  testGroup
    "Validity range tweaks"
    [ testCase "Validity inclusion" $ fst . head . rights $ runTweak checkIsValidDuring txSkelTemplate,
      testCase "Validity intersection" $ fst . head . rights $ runTweak checkAddToValidityRange txSkelTemplate,
      testCase "Time shifting in validity range" $ fst . head . rights $ runTweak checkMoveCurrentSlot txSkelTemplate
    ]
