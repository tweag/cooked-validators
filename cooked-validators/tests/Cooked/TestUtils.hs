-- | Some utilities to write tests for cooked-validators
module Cooked.TestUtils where

import Cooked.MockChain.Testing
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
