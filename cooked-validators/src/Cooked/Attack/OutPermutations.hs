module Cooked.Attack.OutPermutations where

import Cooked.Attack.Common
import Cooked.Tx.Constraints.Optics
import Optics.Core

-- This is hand-rolled so that:
--
-- - the identity permutation comes first, and
--
-- - duplicate entries in the input list don't give rise to duplicate
--   permutations.
distinctPermutations :: Eq a => [a] -> [[a]]
distinctPermutations = foldr (concatMap . insertSomewhere) [[]] . groupEq
  where
    -- group all equal elements. If we had @Ord a@, we could implement this more
    -- effifiently as @group . sort@.
    groupEq :: Eq a => [a] -> [[a]]
    groupEq l = map (\x -> replicate (count x l) x) $ makeUnique l
      where
        count :: Eq a => a -> [a] -> Int
        count _ [] = 0
        count a (b : bs) = if a /= b then count a bs else 1 + count a bs

        makeUnique :: Eq a => [a] -> [a]
        makeUnique [] = []
        makeUnique (x : xs) =
          let xs' = makeUnique xs
           in if x `elem` xs' then xs' else x : xs'

    -- all possibilities to insert elements from the left list into the right
    -- list
    insertSomewhere :: [a] -> [a] -> [[a]]
    insertSomewhere [] ys = [ys]
    insertSomewhere xs [] = [xs]
    insertSomewhere l@(x : xs) r@(y : ys) =
      map (x :) (insertSomewhere xs r) ++ map (y :) (insertSomewhere l ys)

nonIdentityPermutations :: Eq a => [a] -> [[a]]
nonIdentityPermutations = safeTail . distinctPermutations
  where
    safeTail :: [a] -> [a]
    safeTail [] = []
    safeTail (_ : xs) = xs

data PermutOutAttackMode = KeepIdentity | OmitIdentity

-- | Modify transactions by changing the ordering of output constraints. If the
-- 'PermutAttackMode' is 'KeepIdentity', the unmodified transaction is included
-- in the list of modified transactions; if it is 'OmitIdentity', only
-- transactions that are really different from the input transaction are
-- returned. (In particular, this is clever enough to only generate distinct
-- permutations)
permutOutAttack :: PermutOutAttackMode -> Attack
permutOutAttack mode _ skel =
  map (\os -> set outConstraintsL os skel) $
    perms $
      view outConstraintsL skel
  where
    perms = case mode of
      KeepIdentity -> distinctPermutations
      OmitIdentity -> nonIdentityPermutations

-- | For each modified 'TxSkel' returned by the given attack, try all 'TxSkel's
-- obtained by premuting the output constraints. If the given attack @a@
-- returns, say, a list of three modified 'TxSkel's with a, b, and c
-- 'OutConstraint's, respectively, then @tryOutPermutations a@ will return a
-- list of at most a!+b!+c! 'TxSkel's. There may be fewer than that because only
-- distinct permutations are generated.
tryOutPermutations :: Attack -> Attack
tryOutPermutations att mcst skel =
  concatMap (permutOutAttack KeepIdentity mcst) (att mcst skel)

-- TODO: Tests for this module
