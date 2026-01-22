{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module defines 'Tweak's which are the building blocks of our DSL for
-- attacks. They are skeleton modifications aware of the mockchain state.
module Cooked.Tweak.Common
  ( -- * Tweak effect
    Tweak (..),
    runTweak,

    -- * Untyped tweaks
    UntypedTweak (..),

    -- * Optics
    selectP,

    -- * Tweak primitives
    getTxSkel,
    putTxSkel,

    -- * Optics tweaks
    viewTweak,
    viewAllTweak,
    setTweak,
    overTweak,
    overMaybeTweak,
    overMaybeSelectingTweak,
    combineModsTweak,
    iviewTweak,
  )
where

import Control.Arrow (second)
import Control.Monad
import Cooked.Skeleton
import Data.Either.Combinators (rightToMaybe)
import Data.List (mapAccumL)
import Data.Maybe
import Optics.Core
import Polysemy
import Polysemy.NonDet
import Polysemy.State

-- | An effet that allows to store or retrieve a `TxSkel` from a context
data Tweak :: Effect where
  -- | Retrieves the `TxSkel` from the context
  GetTxSkel :: Tweak m TxSkel
  -- | Overrides the `TxSkel` in the context
  PutTxSkel :: TxSkel -> Tweak m ()

makeSem ''Tweak

-- | Running a Tweak is equivalent to running a state monad storing a `TxSkel`
runTweak ::
  TxSkel ->
  Sem (Tweak : effs) a ->
  Sem effs (TxSkel, a)
runTweak txSkel =
  runState txSkel
    . reinterpret
      ( \case
          GetTxSkel -> get
          PutTxSkel skel -> put skel
      )

-- | Untyped tweaks are tweaks that will be deployed in time using
-- `Cooked.Ltl`. They encompass a computation which can branch and has access to
-- a `TxSkel` on top of other effects.
data UntypedTweak effs where
  UntypedTweak ::
    (Members tweakEffs effs) =>
    Sem (Tweak : NonDet : effs) a ->
    UntypedTweak effs

-- | Retrieves some value from the 'TxSkel'
viewTweak ::
  (Member Tweak effs, Is k A_Getter) =>
  Optic' k is TxSkel a ->
  Sem effs a
viewTweak optic = getTxSkel <&> view optic

-- | Like 'viewTweak', only for indexed optics.
iviewTweak ::
  (Member Tweak effs, Is k A_Getter) =>
  Optic' k (WithIx is) TxSkel a ->
  Sem effs (is, a)
iviewTweak optic = getTxSkel <&> iview optic

-- | Like the 'viewTweak', but returns a list of all foci
viewAllTweak ::
  (Member Tweak effs, Is k A_Fold) =>
  Optic' k is TxSkel a ->
  Sem effs [a]
viewAllTweak optic = getTxSkel <&> toListOf optic

-- | The tweak that sets a certain value in the 'TxSkel'.
setTweak ::
  (Member Tweak effs, Is k A_Setter) =>
  Optic' k is TxSkel a ->
  a ->
  Sem effs ()
setTweak optic = overTweak optic . const

-- | The tweak that modifies a certain value in the 'TxSkel'.
overTweak ::
  (Member Tweak effs, Is k A_Setter) =>
  Optic' k is TxSkel a ->
  (a -> a) ->
  Sem effs ()
overTweak optic change = getTxSkel >>= putTxSkel . over optic change

-- | Like 'overTweak', but only modifies foci on which the argument function
-- returns @Just@ the new focus. Returns a list of the foci that were modified,
-- as they were /before/ the tweak, and in the order in which they occurred on
-- the original transaction.
overMaybeTweak ::
  (Member Tweak effs, Is k A_Traversal) =>
  Optic' k is TxSkel a ->
  (a -> Maybe a) ->
  Sem effs [a]
overMaybeTweak optic mChange = overMaybeSelectingTweak optic mChange (const True)

-- | Sometimes 'overMaybeTweak' modifies too many foci. This might be the case
-- if there are several identical foci, but you only want to modify some of
-- them. This is where this 'Tweak' becomes useful: The @(Integer -> Bool)@
-- argument can be used to select which of the modifiable foci should be
-- actually modified.
overMaybeSelectingTweak ::
  (Member Tweak effs, Is k A_Traversal) =>
  Optic' k is TxSkel a ->
  (a -> Maybe a) ->
  (Integer -> Bool) ->
  Sem effs [a]
overMaybeSelectingTweak optic mChange select = do
  allFoci <- viewTweak $ partsOf optic
  let evaluatedFoci =
        snd $
          mapAccumL
            ( \i unmodifiedFocus ->
                case mChange unmodifiedFocus of
                  Just modifiedFocus ->
                    if select i
                      then (i + 1, (unmodifiedFocus, Just modifiedFocus))
                      else (i + 1, (unmodifiedFocus, Nothing))
                  Nothing -> (i, (unmodifiedFocus, Nothing))
            )
            0
            allFoci
  -- If the second component of the pair is @Just@, use it.
  setTweak (partsOf optic) $ map (uncurry fromMaybe) evaluatedFoci
  return $
    mapMaybe
      (\(original, mNew) -> if isJust mNew then Just original else Nothing)
      evaluatedFoci

-- | When constructing a tweak from an optic and a modification of foci, there
-- are in principle two options for optics with many foci: (a) apply the
-- modification to all foci and return /one/ modified transaction (b) generate a
-- number of transactions that contain different combinations of modified and
-- un-modified foci.
--
-- While most of the other "optic -> tweak" functions in this module take take
-- the route (a), this function enables strategy (b).
--
--
-- __Explanation of the arguments and return value__
--
-- - Each of the foci of the @Optic k (WithIx is) TxSkel x@ argument is
--   something in the transaction that we might want to modify.
--
-- - The @is -> x -> Sem effs [(x, l)]@ argument computes a list of possible
--   modifications for each focus, depending on its index. For each modified
--   focus, it also returns a "label" of type @l@, which somehow describes the
--   modification that was made.
--
-- - The @[is] -> [[is]]@ argument determines which combinations of (un-)
--   modified foci will be present on the modified transactions: The input is a
--   list of all of the indices of foci, and for each element @[i_1,...,i_n]@ of
--   the output list, all possible modified transactions that have a
--   modification applied to the foci with indices @i_1,...,i_n@ are generated.
--
-- - The return value of type @[l]@ is the list of labels of all modified foci,
--   in the order in which their indices occurred. Later tweaks may use this
--   list to decide what to do.
--
--
-- __Example 1__
--
-- Assume the optic has three foci, let's denote them by @a, b, c :: x@, with
-- indices @1, 2, 3 :: Integer@ respectively. Also assume that the @is -> x -> m
-- [(x, l)]@ argument returns lists of 2, 3, and 5 elements on @a@, @b@, and
-- @c@, respectively. Let's call those elements @a1, a2@ and @b1, b2, b3@ and
-- @c1, c2, c3, c4, c5@.
--
-- If the @[ix] -> [[ix]]@ argument is @map (:[])@, you will try every
-- modification on a separate transaction, since
--
-- > map (:[]) [1, 2, 3] = [[1], [2], [3]]  .
--
-- Thus, there'll be 2+3+5=10 modified transactions in our examples. Namely, for
-- each element of the list
--
-- > [a1, a2, b1, b2, b3, c1, c2, c3, c4, c5]
--
-- you'll get one modified transaction that includes that value in place of the
-- original focus.
--
-- __Example 2__
--
-- In the setting of the first example, if you want to try combining all
-- possible modifications of one focus with all possible modifications of all
-- other foci, choose @tail . subsequences@ for the @[ix] -> [[ix]] argument. We
-- have
--
-- > tail (subsequences [1, 2, 3])
-- >   == [ [1], [2], [3],
-- >        [1, 2], [1, 3], [2, 3],
-- >        [1, 2, 3]
-- >      ]
--
-- This will correspond to the following 71 modified transactions, represented
-- by the list of modified foci they contain:
--
-- > [ -- one modified focus (the 10 cases from Example 1)
-- >   [a1],
-- >   [a2],
-- >   ...
-- >   [c4],
-- >   [c5],
-- >
-- >   -- two modifications of different foci (2*3 + 2*5 + 3*5 = 31 cases)
-- >   [a1, b1],
-- >   [a1, b2],
-- >   ...
-- >   [b3, c4],
-- >   [b3, c5],
-- >
-- >   -- three modified foci, one from each focus (2*3*5 = 30 cases)
-- >   [a1, b1, c1],
-- >   [a1, b1, c2],
-- >   ...
-- >   [a1, b3, c4],
-- >   [a1, b3, c5]
-- > ]
--
-- So you see that tweaks constructed like this can branch quite wildly. Use
-- with caution!
combineModsTweak ::
  (Eq is, Is k A_Traversal, Members '[Tweak, NonDet] effs) =>
  ([is] -> [[is]]) ->
  Optic' k (WithIx is) TxSkel x ->
  (is -> x -> Sem effs [(x, l)]) ->
  Sem effs [l]
combineModsTweak groupings optic changes = do
  (indexes, foci) <- iviewTweak (ipartsOf optic)
  msum $
    map
      ( \grouping -> do
          let mChangedFoci =
                zipWith
                  ( \i a ->
                      if i `elem` grouping
                        then map (second Right) <$> changes i a
                        else return [(a, Left ())]
                  )
                  indexes
                  foci
          changedFoci <- sequence mChangedFoci
          msum $
            map
              ( \combination -> do
                  setTweak (partsOf optic) $ map fst combination
                  return $ mapMaybe (rightToMaybe . snd) combination
              )
              (allCombinations changedFoci)
      )
      (groupings indexes)
  where
    allCombinations :: [[a]] -> [[a]]
    allCombinations [] = [[]]
    allCombinations (first : rest) = [x : xs | x <- first, xs <- allCombinations rest]

-- | 'overMaybeTweak' requires a modification that can fail (targeting 'Maybe').
-- Sometimes, it can prove more convenient to explicitly state which property
-- the foci shoud satisfy to be eligible for a modification that cannot fail
-- instead. 'selectP' provides a prism to make such a selection.  The intended
-- use case is @overTweak (optic % selectP prop) mod@ where @optic@ gives the
-- candidate foci, @prop@ is the predicate to be satisfied by the foci, and
-- @mod@ is the modification to be applied to the selected foci.
selectP :: (a -> Bool) -> Prism' a a
selectP prop = prism' id (\a -> if prop a then Just a else Nothing)
