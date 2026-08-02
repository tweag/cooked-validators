-- | This module provides tweaks able to remove elements from collections
-- focused in a 'TxSkel'.
module Cooked.Tweak.Remove
  ( -- * Removing elements from lists
    removeIfTweak,
    removeAtPosTweak,

    -- * Removing elements from maps
    removeAtTweak,

    -- * Removing elements from sets
    removeInTweak,
  )
where

import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Query
import Cooked.Tweak.Update
import Data.List (partition)
import Data.Map (Map)
import Data.Set (Set)
import Optics.Core
import Polysemy

-- | Removes elements satisfying a predicate from a list focused in a 'TxSkel',
-- returning the elements that were removed.
removeIfTweak ::
  ( Member Tweak effs,
    Is k A_Lens
  ) =>
  Optic' k is TxSkel [a] ->
  (a -> Bool) ->
  Sem effs [a]
removeIfTweak (castOptic @A_Lens -> optic) removePred = do
  as <- viewTweak optic
  let (removed, kept) = partition removePred as
  setTweak optic kept
  return removed

-- | Removes an element at a specific position in a list focused in a
-- 'TxSkel', returning the removed element, if any.
removeAtPosTweak ::
  ( Member Tweak effs,
    Is k A_Lens
  ) =>
  Optic' k is TxSkel [a] ->
  Int ->
  Sem effs (Maybe a)
removeAtPosTweak (castOptic @A_Lens -> optic) i = do
  as <- viewTweak optic
  let (before, after) = splitAt i as
  case after of
    [] -> return Nothing
    x : xs -> do
      setTweak optic (before ++ xs)
      return $ Just x

-- | Removes an element at a specific key in a map focused in a 'TxSkel',
-- returning the removed value, if any.
removeAtTweak ::
  ( Member Tweak effs,
    Is k A_Lens,
    Ord a
  ) =>
  Optic' k is TxSkel (Map a b) ->
  a ->
  Sem effs (Maybe b)
removeAtTweak (castOptic @A_Lens -> optic) a = do
  mb <- viewTweak (optic % at a)
  setTweak (optic % at a) Nothing
  return mb

-- | Removes an element in a set focused in a 'TxSkel', returning the removed
-- value, if any.
removeInTweak ::
  ( Member Tweak effs,
    Is k A_Lens,
    Ord a
  ) =>
  Optic' k is TxSkel (Set a) ->
  a ->
  Sem effs (Maybe a)
removeInTweak (castOptic @A_Lens -> optic) a = do
  mb <- viewTweak (optic % at a)
  setTweak (optic % at a) Nothing
  return $ a <$ mb
