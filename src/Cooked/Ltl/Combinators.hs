-- | This module provides helpers for writing common LTL expressions.
module Cooked.Ltl.Combinators
  ( anyOf,
    allOf,
    anyOf',
    allOf',
    delay,
    eventually,
    eventually',
    always,
    always',
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

-- | Delays a Ltl formula by @n@ time steps when @n > 0@
delay :: Integer -> Ltl a -> Ltl a
delay n | n <= 0 = id
delay n = LtlNext . delay (n - 1)

-- | Apply a modification once somewhere.
eventually :: a -> Ltl a
eventually = eventually' . LtlAtom

-- | Apply an Ltl expression once somewhere.
eventually' :: Ltl a -> Ltl a
eventually' = LtlUntil LtlTruth

-- | Apply a modification everywhere.
always :: a -> Ltl a
always = always' . LtlAtom

-- | Apply an Ltl expression everywhere.
always' :: Ltl a -> Ltl a
always' = LtlRelease LtlFalsity
