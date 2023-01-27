-- | Some utilities to write tests for cooked-validators. The error reporting
-- could be better.
module Cooked.TestUtils where

import Cooked.MockChain.Testing
import Cooked.Skeleton
import Data.List
import qualified Ledger as Pl
import qualified Plutus.V1.Ledger.Interval as Pl
import qualified Plutus.V1.Ledger.Time as Pl
import qualified PlutusTx.Prelude as Pl
import Test.Tasty.HUnit
import Type.Reflection

assertSubset :: (Show a, Eq a) => [a] -> [a] -> Assertion
assertSubset l r =
  testConjoin
    ( map
        ( \x ->
            assertBool
              ( "not a subset:\n\n"
                  ++ show x
                  ++ "\n\nis not an element of\n\n"
                  ++ show r
              )
              $ x `elem` r
        )
        l
    )

assertSameSets :: (Show a, Eq a) => [a] -> [a] -> Assertion
assertSameSets l r = (length l @?= length r) .&&. assertSubset l r .&&. assertSubset r l
