module Cooked.Tweak.ValidityRangeSpec (tests) where

import Control.Monad (join)
import Cooked (MonadTweak, awaitTime, currentTime, runTweak, txSkelTemplate, waitNMilliSeconds)
import Cooked.Tweak.ValidityRange (addToValidityRangeTweak, centerAroundValidityRangeTweak, getValidityRangeTweak, hasFullTimeRangeTweak, isValidAtTweak, isValidDuringTweak, isValidNowTweak, makeValidityRangeNowTweak, setValidityRangeTweak)
import Data.Default (def)
import Data.Either (rights)
import Data.Function (on)
import Debug.Trace (trace)
import Plutus.V1.Ledger.Interval (interval)
import Plutus.V2.Ledger.Api (POSIXTime (POSIXTime, getPOSIXTime))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

toTimeRange = interval `on` POSIXTime

toTimeRangeTranslate translation a b =
  toTimeRange
    (getPOSIXTime translation + a)
    (getPOSIXTime translation + b)

getSingleResult = fst . head . rights . flip runTweak txSkelTemplate

checkIsValidDuring :: MonadTweak m => m Assertion
checkIsValidDuring = do
  b <- hasFullTimeRangeTweak
  b1 <- isValidDuringTweak $ toTimeRange 101 1015
  setValidityRangeTweak $ toTimeRange 101 1015
  b2 <- isValidDuringTweak $ toTimeRange 110 1000
  b3 <- isValidDuringTweak $ toTimeRange 80 1015
  return $
    assertBool "interval inclusions are wrong" $
      b && b1 && b2 && not b3

checkAddToValidityRange :: MonadTweak m => m Assertion
checkAddToValidityRange = do
  timeOrigin <- currentTime
  centerAroundValidityRangeTweak (timeOrigin + POSIXTime 100) 80
  b <- isValidDuringTweak $ toTimeRangeTranslate timeOrigin 25 35
  b1 <- isValidAtTweak (timeOrigin + POSIXTime 130)
  addToValidityRangeTweak $ toTimeRangeTranslate timeOrigin 110 220
  b2 <- isValidAtTweak (timeOrigin + POSIXTime 130)
  now <- currentTime
  awaitTime $ timeOrigin + POSIXTime 130
  later <- currentTime
  b3 <- trace (show now ++ "   " ++ show later) isValidNowTweak
  -- awaitTime $ POSIXTime 200
  -- b3 <- isValidNowTweak
  -- makeValidityRangeNowTweak
  -- b4 <- isValidNowTweak
  return $
    assertBool "interval intersection is wrong" $
      b && b1 && b2 -- && not b3 && b4

tests :: TestTree
tests =
  testGroup
    "Validity range tweaks"
    [ testCase "Validity inclusion" $ getSingleResult checkIsValidDuring,
      testCase "Validity intersection" $ getSingleResult checkAddToValidityRange
    ]
