module Cooked.Ltl.Combinators
  ( anyOf,
    allOf,
  )
where

import Cooked.Ltl (Ltl (..))

anyOf :: [a] -> Ltl a
anyOf [] = LtlTruth
anyOf xs = foldr (LtlOr . LtlAtom) LtlTruth xs

allOf :: [a] -> Ltl a
allOf [] = LtlTruth
allOf xs = foldr (LtlAnd . LtlAtom) LtlTruth xs
