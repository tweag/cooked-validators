-- | This module exposes an attack consisting in reordering the outputs of a
-- transaction, in an attempt to uncover vulnerabilities on smart contract
-- relying on the outputs order.
module Cooked.Attack.OutputsReordering
  ( -- * Outputs reordering params
    OutputsReorderingParams (..),

    -- * Outputs reordering label
    OutputsReorderingLabel (..),

    -- * Outputs reordering attack
    outputsReorderingAttack,
  )
where

import Control.Monad
import Cooked.Pretty.Class
import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Insert
import Cooked.Tweak.Query
import Cooked.Tweak.Update
import Data.List (permutations)
import Polysemy (Members, Sem)
import Polysemy.NonDet

-- | A label added to a 'TxSkel' on which a tweak reordering some outputs has
-- been applied.
data OutputsReorderingLabel = OutputsReorderingLabel
  deriving (Show, Eq, Ord)

instance PrettyCooked OutputsReorderingLabel where
  prettyCooked _ = "Outputs reordering"

-- | Parameters of the outputs reordering attack
data OutputsReorderingParams
  = -- | Swaps two elements in the list
    Swap Int Int
  | -- | Moves one element from a given index to a given index in the list
    Move Int Int
  | -- | Shuffle the list (generate all permutations, except the identity)
    Shuffle
  | -- | Do whatever you want with the outputs, manually, including removing
    -- some of them, or fully changing the list.
    ManualReordering (forall a. [a] -> [[a]])

-- | Reorders the outputs following a given policy (parameters) to try and
-- uncover vulnerabilities for smart contract depending on the outputs
-- order. This can also be used to uncover some cases of double
-- satisfaction. This removes the permutations that turn out to be identical
-- than the initial outputs list.
outputsReorderingAttack ::
  (Members '[Tweak, NonDet] effs) =>
  OutputsReorderingParams ->
  Sem effs ()
outputsReorderingAttack params = do
  outputs <- viewTweak txSkelOutputsL
  let iMax = length outputs
  msum $
    fmap (setTweak txSkelOutputsL) $
      filter (/= outputs) $
        case params of
          Swap i j
            | valid i iMax && valid j iMax && i /= j ->
                let (ai, aj) = (outputs !! i, outputs !! j)
                 in [replaceAt i aj $ replaceAt j ai outputs]
          Move i j
            | valid i iMax && valid j iMax && i /= j ->
                let ai = outputs !! i
                 in [insertAt (if i < j then j - 1 else j) ai $ removeAt i outputs]
          Shuffle -> permutations outputs
          ManualReordering f -> f outputs
          _ -> []
  insertInTweak txSkelLabelsL $ TxSkelLabel OutputsReorderingLabel
  where
    valid :: Int -> Int -> Bool
    valid i iMax = i >= 0 && i < iMax

    modifyAt :: Int -> ([a] -> [a]) -> [a] -> [a]
    modifyAt 0 f l = f l
    modifyAt _ _ [] = []
    modifyAt n f (x : xs) = x : modifyAt (n - 1) f xs

    replaceAt n a = modifyAt n $ \case [] -> []; (_ : xs) -> a : xs

    removeAt n = modifyAt n $ \case [] -> []; (_ : xs) -> xs

    insertAt n a = modifyAt n (a :)
