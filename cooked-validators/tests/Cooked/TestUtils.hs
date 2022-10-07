{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | Some utilities to write tests for cooked-validators. The error reporting
-- could be better.
module Cooked.TestUtils where

import Cooked.MockChain.Testing
import Cooked.Tx.Constraints.Pretty
import Cooked.Tx.Constraints.Type
import Data.List
import qualified Ledger as Pl
import qualified Ledger.Value as Pl
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

-- | Assert that two 'Constraints' are semantically the same. This is almost a
-- literal copy of 'sameConstraints' from "Cooked.Tx.Constraints.Type", only
-- adapted to yield more informative failure messages.
assertSameConstraints :: Constraints -> Constraints -> Assertion
assertSameConstraints (is :=>: os) (is' :=>: os') =
  assertSameSets (filter isSpendsScriptOrSpendsPK is) (filter isSpendsScriptOrSpendsPK is')
    .&&. (os @=? os')
    .&&. (validityRange is @=? validityRange is')
    .&&. (sort (signers is) @=? sort (signers is'))
    .&&. assertBool "the minted values per redeemer differ" (sameMintedValuesWithRedeemers is is')
  where
    isSpendsScriptOrSpendsPK :: MiscConstraint -> Bool
    isSpendsScriptOrSpendsPK SpendsScript {} = True
    isSpendsScriptOrSpendsPK SpendsPK {} = True
    isSpendsScriptOrSpendsPK _ = False

    validityRange :: [MiscConstraint] -> Pl.POSIXTimeRange
    validityRange = foldr (Pl.intersection . toTimeRange) Pl.always
      where
        toTimeRange = \case
          Before b -> Pl.to b
          After a -> Pl.from a
          ValidateIn i -> i
          _ -> Pl.always

    signers :: [MiscConstraint] -> [Pl.PubKeyHash]
    signers = foldr (union . toSignerList) []
      where
        toSignerList (SignedBy s) = s
        toSignerList _ = []

    sameMintedValuesWithRedeemers :: [MiscConstraint] -> [MiscConstraint] -> Bool
    sameMintedValuesWithRedeemers cs cs' =
      all (\case Mints r _pols v -> mintedWithRedeemer r ms' `Pl.geq` v; _ -> True) ms
        && all (\case Mints r _pols v -> mintedWithRedeemer r ms `Pl.geq` v; _ -> True) ms'
      where
        isMintsConstraint = \case Mints {} -> True; _ -> False
        ms = filter isMintsConstraint cs
        ms' = filter isMintsConstraint cs'

        mintedWithRedeemer :: MintsConstrs a => Maybe a -> [MiscConstraint] -> Pl.Value
        mintedWithRedeemer r = foldr ((<>) . toMintedValueIfCorrectRedeemer) mempty
          where
            toMintedValueIfCorrectRedeemer (Mints r' _pols v) =
              case r ~*~? r' of
                Just HRefl -> if r Pl.== r' then v else mempty
                Nothing -> mempty
            toMintedValueIfCorrectRedeemer _ = mempty
