-- | 'Tweak's working on the outputs of a 'TxSkel'
module Cooked.Tweak.Outputs
  ( ensureOutputTweak,
    addOutputTweak,
    removeOutputTweak,
    tamperDatumTweak,
    TamperDatumLbl (..),
    malformDatumTweak,
    MalformDatumLbl (..),
  )
where

import Control.Monad
import Cooked.Output
import Cooked.Pretty.Common
import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Labels
import Data.List (partition)
import Data.Maybe
import Data.Typeable
import Optics.Core
import PlutusLedgerApi.V3 qualified as Api

-- | Ensures that a certain output is produced by a transaction. The return
-- value will be @Just@ the added output, when applicable.
ensureOutputTweak :: (MonadTweak m) => TxSkelOut -> m (Maybe TxSkelOut)
ensureOutputTweak txSkelOut = do
  presentOutputs <- viewTweak txSkelOutsL
  if txSkelOut `elem` presentOutputs
    then return Nothing
    else do
      addOutputTweak txSkelOut
      return $ Just txSkelOut

-- | Adds a transaction output, at the end of the current list of outputs, thus
-- retaining the initial outputs order.
addOutputTweak :: (MonadTweak m) => TxSkelOut -> m ()
addOutputTweak txSkelOut = overTweak txSkelOutsL (++ [txSkelOut])

-- | Removes transaction outputs according to some predicate. The returned list
-- contains all the removed outputs.
removeOutputTweak :: (MonadTweak m) => (TxSkelOut -> Bool) -> m [TxSkelOut]
removeOutputTweak removePred = do
  presentOutputs <- viewTweak txSkelOutsL
  let (removed, kept) = partition removePred presentOutputs
  setTweak txSkelOutsL kept
  return removed

-- | A label added to a 'TxSkel' on which the 'tamperDatumTweak' has been
-- successfully applied
data TamperDatumLbl = TamperDatumLbl deriving (Show, Eq, Ord)

instance PrettyCooked TamperDatumLbl where
  prettyCookedOpt _ _ = "TamperDatum"

-- | A tweak that tries to change the datum on outputs carrying datums of a
-- certain type with a prescribed tampering function. The tampering function
-- ignores datums of other types and those for which it returns @Nothing@.
--
-- The tweak returns a list of the modified datums, as they were *before* the
-- modification was applied to them.
tamperDatumTweak :: forall a m. (MonadTweak m, Api.FromData a, Typeable a) => (a -> Maybe a) -> m [a]
tamperDatumTweak change = do
  beforeModification <- overMaybeTweak (txSkelOutsL % traversed % txSkelOutputDatumTypeAT) change
  guard . not . null $ beforeModification
  addLabelTweak TamperDatumLbl
  return beforeModification

-- | A tweak that tries to change the datum on outputs carrying datums of a
-- certain type with a prescribed tampering function. There are two main
-- differences with 'tamperDatumTweak'. First, the tampering function returns
-- 'Api.BuiltinData', allowing it to do pretty much anything with the
-- datums. Second, for every output datum there are zero or more options for how
-- to modify it, and all combinations of these modifications are tried.
--
-- That is, if there are @n@ output datums, for which there are @k_1,...,k_n@
-- possible modifications, this tweak will try
--
-- >   k_1 + ... + k_n
-- > + k_1 * k_2 + ... + k_{n-1} * k_n
-- > + k_1 * k_2 * k_3 + ... + k_{n-2} * k_{n-1} * k_n
-- > + ...
-- > + k_1 * k_2 * ... * k_{n-1} * k_n
-- > == (k_1 + 1) * ... * (k_n + 1) - 1
--
-- modified transactions.
malformDatumTweak :: forall a m. (MonadTweak m, Typeable a) => (a -> [Api.BuiltinData]) -> m ()
malformDatumTweak change = do
  outputs <- viewAllTweak (txSkelOutsL % traversed)
  let modifiedOutputs = map (\output -> output : changeOutput output) outputs
      -- We remove the first combination because it consists of all the heads
      -- and therefore it is the combination consisting of no changes at all.
      modifiedOutputGroups = tail $ allCombinations modifiedOutputs
  msum $ map (setTweak txSkelOutsL) modifiedOutputGroups
  addLabelTweak MalformDatumLbl
  where
    changeOutput :: TxSkelOut -> [TxSkelOut]
    changeOutput (Pays out) =
      do
        let dat = view outputDatumL out
        typedDat <- maybeToList $ txSkelOutTypedDatum @a dat
        modifiedDat <- change typedDat
        return $ Pays $ setDatum out $ case dat of
          TxSkelOutNoDatum -> TxSkelOutNoDatum
          TxSkelOutSomeDatum _ shape -> TxSkelOutSomeDatum (DatumContent modifiedDat) shape

-- | A label added to a 'TxSkel' on which the 'malformDatumTweak' has been
-- successfully applied
data MalformDatumLbl = MalformDatumLbl deriving (Show, Eq, Ord)

instance PrettyCooked MalformDatumLbl where
  prettyCookedOpt _ _ = "MalformDatum"

-- | Given a list of lists @l@, we call “combination” of @l@ a list @c@ such
-- that - @length c == length l@, and - for all @0 <= i < length c@, @elem (c !!
-- i) (l !! i)@.
--
-- 'allCombinations', as the name suggests, returns all the possible
-- combinations of a given list of lists. For instance:
--
-- @allCombinations [[1,2,3], [4,5], [6]] == [[1,4,6], [1,5,6], [2,4,6], [2,5,6], [3,4,6], [3,5,6]]@
--
-- It is guaranteed that combinations are returned in such an order that a
-- combination @c1@ comes before a combination @c2@ in the result list if and
-- only if for some prefix list @p@, some elements @a1@ and @a2@ and for some
-- rest lists @r1@ and @r2@:
-- > c1 == p ++ (a1 : r1)
-- > c2 == p ++ (a2 : r2)
-- and @a1@ comes before @a2@ in the list @l !! length p@. In particular, the
-- first element of the result list is the combination consisting of all the
-- first elements of the input lists.
allCombinations :: [[a]] -> [[a]]
allCombinations [] = [[]]
allCombinations [[]] = [] -- included in the next one
allCombinations (first : rest) = [x : xs | x <- first, xs <- allCombinations rest]
