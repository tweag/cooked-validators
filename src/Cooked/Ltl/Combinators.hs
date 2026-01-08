-- | This module provides helpers for writing common LTL expressions.
module Cooked.Ltl.Combinators
  ( anyOf,
    allOf,
    anyOf',
    allOf',
    delay,
    delay',
    eventually,
    eventually',
    always,
    always',
    whenPossible',
    whenPossible,
    ifPossible',
    ifPossible,
    ltlImplies',
    ltlImplies,
    never',
    never,
  )
where

import Cooked.Ltl (Ltl (..))

-- | Same as `anyOf'`, but first wraps the elements in the input list in atomic
-- formulas.
anyOf :: [a] -> Ltl a
anyOf = anyOf' . map LtlAtom

-- | Produces an Ltl formula which consists of the disjunction of all the
-- formulas in the input list.
anyOf' :: [Ltl a] -> Ltl a
anyOf' = foldr LtlOr LtlFalsity

-- | Same as `allOf'`, but first wraps the elements in the input list in atomic
-- formulas.
allOf :: [a] -> Ltl a
allOf = allOf' . map LtlAtom

-- | Produces an Ltl formula which consists of the conjunction of all the
-- formulas in the input list.
allOf' :: [Ltl a] -> Ltl a
allOf' = foldr LtlAnd LtlTruth

-- | Same as `delay'`, but first wraps the elements in the input list in atomic
-- formulas.
delay :: Integer -> a -> Ltl a
delay n = delay' n . LtlAtom

-- | Produces an Ltl formula which consists of the delay of the input formula by
-- @n@ time steps, if @n > 0@. Otherwise, leaves the formula unchanged.
delay' :: Integer -> Ltl a -> Ltl a
delay' n | n <= 0 = id
delay' n = LtlNext . delay' (n - 1)

-- | Same as `eventually'`, but first wraps the elements in the input list in
-- atomic formulas.
eventually :: a -> Ltl a
eventually = eventually' . LtlAtom

-- | Produces an Ltl formula which ensures the input formula eventually holds
eventually' :: Ltl a -> Ltl a
eventually' = LtlUntil LtlTruth

-- | Same as `always'`, but first wraps the elements in the input list in
-- atomic formulas.
always :: a -> Ltl a
always = always' . LtlAtom

-- | Produces an Ltl formula which ensures the input formula always holds
always' :: Ltl a -> Ltl a
always' = LtlRelease LtlFalsity

-- | Same as `ifPossible'`, but first wraps the input in an atomic formula
ifPossible :: a -> Ltl a
ifPossible = ifPossible' . LtlAtom

-- | Produces an Ltl formula which attempts to apply a certain formula but does
-- not fail in case it fails.
ifPossible' :: Ltl a -> Ltl a
ifPossible' f = f `LtlOr` LtlNot f

-- | Same as `whenPossible'`, but first wraps the input in an atomic formula
whenPossible :: a -> Ltl a
whenPossible = whenPossible' . LtlAtom

-- | Produces an Ltl formula which attempts to apply a certain formula whenever
-- possible, while ignoring steps when it is not.
whenPossible' :: Ltl a -> Ltl a
whenPossible' = always' . ifPossible'

-- | Same as `never'`, but first wraps the input in an atomic formula
never :: a -> Ltl a
never = never' . LtlAtom

-- | Produces an Ltl formula ensuring the given formula always fails
never' :: Ltl a -> Ltl a
never' = always' . LtlNot

-- | Same as `ltlImplies'` but first wraps the inputs in atoms
ltlImplies :: a -> a -> Ltl a
ltlImplies a1 a2 = LtlAtom a1 `ltlImplies'` LtlAtom a2

-- | Produces a formula that succeeds if the first formula fails, or if both
-- formulas hold
ltlImplies' :: Ltl a -> Ltl a -> Ltl a
ltlImplies' f1 f2 = (f2 `LtlAnd` f1) `LtlOr` LtlNot f1
