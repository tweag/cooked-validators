-- | This module exposes tweaks to insert elements within various structures
-- focused in a 'TxSkel'.
module Cooked.Tweak.Insert
  ( -- * Generic insert functions
    insertUsingTweak,
    appendAfterTweak,
    appendBeforeTweak,

    -- * Insert elements in lists
    insertThereTweak,
    insertFirstTweak,
    insertLastTweak,

    -- * Insert elements in sets
    insertInTweak,

    -- * Insert elements in maps
    insertAtTweak,
  )
where

import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Guard
import Cooked.Tweak.Update
import Data.Map (Map)
import Data.Set (Set)
import Optics.Core
import Polysemy
import Polysemy.NonDet

-- | Inserts an element in a structure using a custom function
insertUsingTweak ::
  ( Member Tweak effs,
    Is k A_Setter
  ) =>
  Optic' k is TxSkel (f a) ->
  (a -> f a -> f a) ->
  a ->
  Sem effs ()
insertUsingTweak optic op el = overTweak optic (op el)

-- | Appending an element in a Semigroup after the existing element
appendAfterTweak ::
  ( Member Tweak effs,
    Is k A_Setter,
    Semigroup a
  ) =>
  Optic' k is TxSkel a ->
  a ->
  Sem effs ()
appendAfterTweak optic a = overTweak optic (<> a)

-- | Appending an element in a Semigroup after the existing element
appendBeforeTweak ::
  ( Member Tweak effs,
    Is k A_Setter,
    Semigroup a
  ) =>
  Optic' k is TxSkel a ->
  a ->
  Sem effs ()
appendBeforeTweak optic a = overTweak optic (a <>)

-- | Inserts an element at a specific position in a list focused in a
-- 'TxSkel'. If the index is beyond or equal to the list length, inserts it at
-- the end of the list.
insertThereTweak ::
  ( Member Tweak effs,
    Is k A_Setter
  ) =>
  Optic' k is TxSkel [a] ->
  Int ->
  a ->
  Sem effs ()
insertThereTweak optic j =
  insertUsingTweak optic (aux j)
  where
    aux _ el [] = [el]
    aux i el (x : xs) | i == 0 = x : el : xs
    aux i el (x : xs) = x : aux i el xs

-- | Inserts an element at the first position in a list focused in a 'TxSkel'.
insertFirstTweak ::
  ( Member Tweak effs,
    Is k A_Setter
  ) =>
  Optic' k is TxSkel [a] ->
  a ->
  Sem effs ()
insertFirstTweak optic = insertThereTweak optic 0

-- | Inserts an element at the end of a list focused in a 'TxSkel'.
insertLastTweak ::
  ( Member Tweak effs,
    Is k A_Setter
  ) =>
  Optic' k is TxSkel [a] ->
  a ->
  Sem effs ()
insertLastTweak optic = insertUsingTweak optic (\el -> (++ [el]))

-- | Inserts an element in a set focused in a 'TxSkel'. Fails if the element is
-- already present in the set.
insertInTweak ::
  ( Members '[Tweak, NonDet] effs,
    Is k A_Traversal,
    Ord a
  ) =>
  Optic' k is TxSkel (Set a) ->
  a ->
  Sem effs ()
insertInTweak (castOptic @A_Traversal -> optic) a = do
  guardTweak $ optic % at a % _Nothing
  setTweak (optic % contains a) True

-- | Inserts an element in a map focused in a 'TxSkel'. Fails if the key is
-- already present in the map.
insertAtTweak ::
  ( Members '[Tweak, NonDet] effs,
    Is k A_Traversal,
    Ord k
  ) =>
  Optic' k is TxSkel (Map k v) ->
  k ->
  v ->
  Sem effs ()
insertAtTweak (castOptic @A_Traversal -> optic) k v = do
  guardTweak $ optic % at k % _Nothing
  setTweak (optic % at k) (Just v)
