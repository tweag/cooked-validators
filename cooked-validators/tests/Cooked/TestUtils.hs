{-# LANGUAGE LambdaCase #-}

-- | Some utilities to write tests for cooked-validators. The error reporting
-- could be better.
module Cooked.TestUtils where

import Cooked.MockChain.Testing
import Cooked.Tx.Constraints.Pretty
import Cooked.Tx.Constraints.Type
import qualified Plutus.V1.Ledger.Interval as Pl
import qualified Plutus.V1.Ledger.Time as Pl
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
assertSameSets l r = (length l @?= length r) .&&. assertSubset l r .&&. assertSubset r l

instance Show MiscConstraint where
  show = show . prettyMiscConstraint

instance Show OutConstraint where
  show = show . prettyOutConstraint

instance Show Constraints where
  show (is :=>: os) = show is ++ " :=>: " ++ show os

instance Show TxSkel where
  show = show . prettyTxSkel []

-- | Assert that two constraints are the same, up to reordering of inputs, and
-- substitutions of time constraints ('After', 'Before', 'ValidateIn') that
-- preserve the validity interval of the transaction.
assertSameConstraints :: Constraints -> Constraints -> Assertion
assertSameConstraints (is :=>: os) (is' :=>: os') =
  assertSameSets (filter isNoTimeConstraint is) (filter isNoTimeConstraint is')
    .&&. (os @?= os')
    .&&. (validityRange is @?= validityRange is')
  where
    isNoTimeConstraint :: MiscConstraint -> Bool
    isNoTimeConstraint (Before _) = False
    isNoTimeConstraint (After _) = False
    isNoTimeConstraint (ValidateIn _) = False
    isNoTimeConstraint _ = True

    validityRange :: [MiscConstraint] -> Pl.POSIXTimeRange
    validityRange =
      foldr
        ( \case
            -- I don't know if 'Before' and 'After' should include the endpoint
            -- or not. Since the current implementation of the 'Before' and
            -- 'After' constraints includes the endpoints, I will do so here as
            -- well.
            Before b -> Pl.intersection (Pl.to b)
            After a -> Pl.intersection (Pl.from a)
            ValidateIn i -> Pl.intersection i
            _ -> id
        )
        Pl.always
