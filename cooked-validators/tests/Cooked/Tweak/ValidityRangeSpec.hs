module Cooked.Tweak.ValidityRangeSpec (tests) where

import Control.Monad (join)
import Cooked (MonadTweak, runTweak, txSkelTemplate)
import Cooked.Tweak.ValidityRange (hasFullTimeRangeTweak, isValidDuringTweak, setValidityRangeTweak)
import Data.Default (def)
import Data.Either (rights)
import Data.Function (on)
import Plutus.V1.Ledger.Interval (interval)
import Plutus.V2.Ledger.Api (POSIXTime (POSIXTime))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

toTimeRange = interval `on` POSIXTime

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
  

tests :: TestTree
tests =
  testGroup
    "Validity range tweaks"
    [testCase "Validity inclusion" $ getSingleResult checkIsValidDuring]
