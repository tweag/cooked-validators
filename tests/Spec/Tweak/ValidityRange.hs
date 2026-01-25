module Spec.Tweak.ValidityRange (tests) where

import Control.Monad (void)
import Cooked.MockChain.Error
import Cooked.MockChain.Log
import Cooked.MockChain.Read
import Cooked.MockChain.State
import Cooked.MockChain.Testing
import Cooked.MockChain.Write
import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.ValidityRange
import Data.Default (def)
import Data.Either (rights)
import Data.Function (on)
import Ledger.Slot qualified as Ledger
import Ledger.Tx qualified as Ledger
import PlutusLedgerApi.V1.Interval qualified as Api
import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.NonDet
import Polysemy.State
import Polysemy.Writer
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)

toSlotRange :: Integer -> Integer -> Api.Interval Ledger.Slot
toSlotRange = Api.interval `on` Ledger.Slot

toSlotRangeTranslate :: Ledger.Slot -> Integer -> Integer -> Api.Interval Ledger.Slot
toSlotRangeTranslate translation a b =
  toSlotRange
    (Ledger.getSlot translation + a)
    (Ledger.getSlot translation + b)

checkIsValidDuring :: (Member Tweak effs) => Sem effs Assertion
checkIsValidDuring = do
  b <- hasFullTimeRangeTweak
  b1 <- isValidDuringTweak $ toSlotRange 101 1015
  void $ setValidityRangeTweak $ toSlotRange 101 1015
  b2 <- isValidDuringTweak $ toSlotRange 110 1000
  b3 <- isValidDuringTweak $ toSlotRange 80 1015
  return $
    assertBool "interval inclusions are wrong" $
      b && b1 && b2 && not b3

checkAddToValidityRange :: (Members '[Tweak, MockChainRead, MockChainWrite, NonDet] effs) => Sem effs Assertion
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

type ValidityRangeEffs =
  '[ Tweak,
     MockChainWrite,
     MockChainRead,
     NonDet
   ]

interpretValidityRange :: Sem ValidityRangeEffs a -> [a]
interpretValidityRange =
  run
    . fmap rights
    . runNonDet
    . fmap snd
    . runWriter
    . runMockChainLog (: [])
    . evalState def
    . runError
    . runFailInMockChainError
    . runToCardanoErrorInMockChainError
    . runMockChainRead
    . runMockChainWrite
    . evalTweak txSkelTemplate
    . insertAt @3
      @'[ Error Ledger.ToCardanoError,
          Fail,
          Error MockChainError,
          State MockChainState,
          MockChainLog,
          Writer [MockChainLogEntry]
        ]

tests :: TestTree
tests =
  testGroup
    "Validity range tweaks"
    [ testCase "Validity inclusion" $ testConjoin $ interpretValidityRange checkIsValidDuring,
      testCase "Validity intersection" $ testConjoin $ interpretValidityRange checkAddToValidityRange
    ]
