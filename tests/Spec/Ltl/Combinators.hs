module Spec.Ltl.Combinators where

import Cooked.Ltl (Ltl (..))
import Cooked.Ltl.Combinators as MUT
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "LTL Combinators"
    [ testCase "anyOf [1] - no LtlOr" $ anyOf @Int [1] @?= LtlAtom 1,
      testCase "anyOf [1,2] - single LtlOr" $ anyOf @Int [1, 2] @?= LtlOr (LtlAtom 1) (LtlAtom 2),
      testCase "anyOf [1,2,3] - right biased LtlOr tree" $ anyOf @Int [1, 2, 3] @?= LtlOr (LtlAtom 1) (LtlOr (LtlAtom 2) (LtlAtom 3)),
      testCase "allOf [1] - no LtlAnd" $ allOf @Int [1] @?= LtlAtom 1,
      testCase "allOf [1,2] - single LtlAnd" $ allOf @Int [1, 2] @?= LtlAnd (LtlAtom 1) (LtlAtom 2),
      testCase "allOf [1,2,3] - right biased LtlAnd tree" $ allOf @Int [1, 2, 3] @?= LtlAnd (LtlAtom 1) (LtlAnd (LtlAtom 2) (LtlAtom 3)),
      testCase "delay -1" $ delay @Int (-1) LtlTruth @?= LtlTruth,
      testCase "delay 0" $ delay @Int 0 LtlTruth @?= LtlTruth,
      testCase "delay 1" $ delay @Int 1 LtlTruth @?= LtlNext LtlTruth,
      testCase "delay 2" $ delay @Int 2 LtlTruth @?= LtlNext (LtlNext LtlTruth),
      testCase "eventually" $ eventually @Int 1 @?= LtlUntil LtlTruth (LtlAtom 1),
      testCase "always" $ always @Int 1 @?= LtlRelease LtlFalsity (LtlAtom 1)
    ]
