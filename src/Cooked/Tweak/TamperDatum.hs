module Cooked.Tweak.TamperDatum
  ( tamperDatumTweak,
    TamperDatumLbl (..),
    malformDatumTweak,
    MalformDatumLbl (..),
  )
where

import Control.Monad
import Cooked.Output
import Cooked.Pretty.Class
import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Labels
import Optics.Core
import PlutusTx qualified as Pl
import Type.Reflection

-- | A tweak that tries to change the datum on outputs carrying datums of a
-- certain type with a prescribed tampering function.
--
-- The tweak returns a list of the modified datums, as they were *before* the
-- modification was applied to them.
tamperDatumTweak ::
  forall a m.
  ( MonadTweak m,
    Show a,
    PrettyCooked a,
    Pl.ToData a,
    Pl.FromData a,
    Typeable a
  ) =>
  -- | Use this function to return 'Just' the changed datum, if you want to
  -- perform a change, and 'Nothing', if you want to leave it as-is. All datums
  -- on outputs not paying to a validator of type @a@ are never touched.
  (a -> Maybe a) ->
  m [a]
tamperDatumTweak change = do
  beforeModification <-
    overMaybeTweak
      ( txSkelOutsL
          % traversed
          % txSkelOutputDatumTypeAT @a
      )
      change
  guard . not . null $ beforeModification
  addLabelTweak TamperDatumLbl
  return beforeModification

data TamperDatumLbl = TamperDatumLbl deriving (Show, Eq, Ord)

-- | A tweak that tries to change the datum on outputs carrying datums of a
-- certain type with a prescribed tampering function. There are two main
-- differences with 'tamperDatumTweak'. First, the tampering function returns 'BuiltinData', allowing it to
-- do pretty much anything with the datums. Second, for every output datum there are zero or more options
-- for how to modify it, and all combinations of these modifications are tried.
--
-- That is, if there are 'n' output datums, for which there are 'k_1,...,k_n' possible modifications,
-- this tweak will try
--
-- >   k_1 + ... + k_n
-- > + k_1 * k_2 + ... + k_{n-1} * k_n
-- > + k_1 * k_2 * k_3 + ... + k_{n-2} * k_{n-1} * k_n
-- > + ...
-- > + k_1 * k_2 * ... * k_{n-1} * k_n
-- > == (k_1 + 1) * ... * (k_n + 1) - 1
--
-- modified transactions.
malformDatumTweak ::
  forall a m.
  ( MonadTweak m,
    Pl.ToData a,
    Pl.FromData a,
    Typeable a
  ) =>
  (a -> [Pl.BuiltinData]) ->
  m ()
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
      let datums = changeTxSkelOutDatum $ view outputDatumL out
       in map
            ( \datum ->
                Pays $
                  ConcreteOutput
                    (out ^. outputOwnerL)
                    (out ^. outputStakingCredentialL)
                    (out ^. outputValueL)
                    datum
                    (out ^. outputReferenceScriptL)
            )
            datums

    changeTxSkelOutDatum :: TxSkelOutDatum -> [TxSkelOutDatum]
    changeTxSkelOutDatum TxSkelOutNoDatum = []
    changeTxSkelOutDatum (TxSkelOutDatum datum) = map TxSkelOutDatum $ changeOnCorrectType datum
    changeTxSkelOutDatum (TxSkelOutDatumHash datum) = map TxSkelOutDatumHash $ changeOnCorrectType datum
    changeTxSkelOutDatum (TxSkelOutInlineDatum datum) = map TxSkelOutInlineDatum $ changeOnCorrectType datum

    changeOnCorrectType :: (Typeable b) => b -> [Pl.BuiltinData]
    changeOnCorrectType datum = case typeOf datum `eqTypeRep` (typeRep @a) of
      Just HRefl -> change datum
      Nothing -> []

data MalformDatumLbl = MalformDatumLbl deriving (Show, Eq, Ord)

-- | Given a list of lists @l@, we call “combination” of @l@ a list @c@ such that
-- - @length c == length l@, and
-- - for all @0 <= i < length c@, @elem (c !! i) (l !! i)@.
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
