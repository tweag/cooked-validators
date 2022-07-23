-- | Some utilities to write tests for cooked-validators. The error reporting
-- could be better.
module Cooked.TestUtils where

import Cooked.MockChain.Testing
import Cooked.Tx.Constraints.Pretty
import Cooked.Tx.Constraints.Type
import Test.Tasty.HUnit

assertSubset :: (Show a, Eq a) => [a] -> [a] -> Assertion
assertSubset l r =
  testConjoin
    ( map
        ( \x ->
            assertBool
              ( "not a subset:\n\n" ++ show x
                  ++ "\n\nis not an element of\n\n"
                  ++ show r
              )
              $ x `elem` r
        )
        l
    )

assertSameSets :: (Show a, Eq a) => [a] -> [a] -> Assertion
assertSameSets l r = assertSubset l r .&&. assertSubset r l

instance Show MiscConstraint where
  show = show . prettyMiscConstraint

instance Show OutConstraint where
  show = show . prettyOutConstraint

-- | assert that two constraints are the same, up to reordering of inputs.
--
-- TODO, maybe: Test for logical equality.
assertSameConstraints :: Constraints -> Constraints -> Assertion
assertSameConstraints (is :=>: os) (is' :=>: os') =
  assertSameSets is is' .&&. (os @?= os')
