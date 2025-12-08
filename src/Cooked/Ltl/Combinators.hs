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
anyOf' [] = LtlFalsity
anyOf' xs = foldr1 LtlOr xs

-- | Same as `allOf'`, but first wraps the elements in the input list in atomic
-- formulas.
allOf :: [a] -> Ltl a
allOf = allOf' . map LtlAtom

-- | Produces an Ltl formula which consists of the conjunction of all the
-- formulas in the input list.
allOf' :: [Ltl a] -> Ltl a
allOf' [] = LtlTruth
allOf' xs = foldr1 LtlAnd xs

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

-- | Produces an Ltl formula which consists the input formula eventually holds
eventually' :: Ltl a -> Ltl a
eventually' = LtlUntil LtlTruth

-- |  Same as `always'`, but first wraps the elements in the input list in
-- atomic formulas.
always :: a -> Ltl a
always = always' . LtlAtom

-- | Produces an Ltl formula which ensures the input formula always holds
always' :: Ltl a -> Ltl a
always' = LtlRelease LtlFalsity
