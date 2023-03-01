module Cooked.Tweak.OutPermutations
  ( PermutOutTweakMode (..),
    allOutPermutsTweak,
    singleOutPermutTweak,

    -- * For testing purposes
    distinctPermutations,
  )
where

import Control.Monad
import Cooked.Skeleton
import Cooked.Tweak.Common
import System.Random
import System.Random.Shuffle

data PermutOutTweakMode = KeepIdentity (Maybe Int) | OmitIdentity (Maybe Int)

-- | Modify transactions by changing the ordering of output constraints. If the
-- 'PermutTweakMode' is
--
-- - @KeepIdentity (Just n)@, the unmodified transaction is included in the list
--   of modified transactions and only the first n outputs are permuted,
--
-- - @KeepIdentity Nothing@, the unmodified transaction is included and all
--   outputs are permuted. Use this with care; there might be a lot of
--   permutations!
--
-- - @OmitIdentity (Just n)@, the unmodified transaction is not included in the
--   list of modified transactions and only the first n outputs are permuted,
--
-- - @OmitIdentity Nothing@, the unmodified transaction is not included and all
--   outputs are permuted. Use this with care; there might be a lot of
--   permutations!
--
-- (In particular, this is clever enough to generate only the distinct
-- permutations, even if some outputs are identical.)
allOutPermutsTweak :: MonadTweak m => PermutOutTweakMode -> m ()
allOutPermutsTweak mode = do
  oldOut <- viewTweak txSkelOutsL
  msum $
    map
      (setTweak txSkelOutsL)
      (perms oldOut)
  where
    perms = case mode of
      KeepIdentity (Just n) -> \l -> map (++ drop n l) $ distinctPermutations (take n l)
      KeepIdentity Nothing -> distinctPermutations
      OmitIdentity (Just n) -> \l -> map (++ drop n l) $ nonIdentityPermutations (take n l)
      OmitIdentity Nothing -> nonIdentityPermutations

-- This is implemented so that duplicate entries in the input list don't give
-- rise to duplicate permutations.
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
nonIdentityPermutations l = removeFirst l $ distinctPermutations l
  where
    removeFirst :: Eq a => a -> [a] -> [a]
    removeFirst _ [] = []
    removeFirst x (y : ys) = if x == y then ys else y : removeFirst x ys

-- | This randomly permutes the outputs of a transaction with a given seed
-- Can be used to assess if a certain validator is order-dependant
singleOutPermutTweak :: MonadTweak m => Int -> m ()
singleOutPermutTweak seed = do
  outputs <- viewTweak txSkelOutsL
  let outputs' = shuffle' outputs (length outputs) (mkStdGen seed)
  guard $ outputs' /= outputs
  setTweak txSkelOutsL outputs'
