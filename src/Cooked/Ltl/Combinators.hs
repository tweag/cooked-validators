-- | This module provides helpers for writing common LTL expressions.
module Cooked.Ltl.Combinators
  ( anyOf,
    allOf,
    anyOf',
    allOf',
  )
where

import Cooked.Ltl (Ltl (..))

-- | Produce an Ltl expression which branches on any of the provided
-- inputs. It will not attempt combinations, only one input will be
-- applied in any branch. See 'LtlOr'.
anyOf :: [a] -> Ltl a
anyOf = anyOf' . map LtlAtom

-- | Combine a set of Ltl expressions into one where any of them may succeed.
-- Creates a branch for each input. See 'LtlOr' for the semantics of branching.
anyOf' :: [Ltl a] -> Ltl a
anyOf' = foldr LtlOr LtlFalsity

-- | Produce an Ltl expression which applies all the provided inputs. All must
-- apply for this to succeed. See 'LtlAnd'.
allOf :: [a] -> Ltl a
allOf = allOf' . map LtlAtom

-- | Combine a set of Ltl expressions into one where all must succeed. See
-- 'LtlAnd' for semantics of conjunction.
allOf' :: [Ltl a] -> Ltl a
allOf' = foldr LtlAnd LtlTruth
