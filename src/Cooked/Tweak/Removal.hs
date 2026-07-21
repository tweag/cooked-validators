module Cooked.Tweak.Removal
  ( removeIfTweak,
    removeAtTweak,
  )
where

import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.List (partition)
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

-- | Removes an element at the specific index in a list focused in a 'TxSkel',
-- returning @Just@ the removed element if any, @Nothing@ otherwise.
removeAtTweak ::
  ( Member Tweak effs,
    Is k A_Lens
  ) =>
  Optic' k is TxSkel [a] ->
  Int ->
  Sem effs (Maybe a)
removeAtTweak (castOptic @A_Lens -> optic) i = do
  as <- viewTweak optic
  let (before, after) = splitAt i as
  case after of
    [] -> return Nothing
    x : xs -> do
      setTweak optic (before ++ xs)
      return $ Just x
