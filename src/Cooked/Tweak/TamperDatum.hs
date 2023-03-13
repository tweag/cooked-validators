{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
import qualified PlutusTx as Pl
import Type.Reflection

-- | A tweak that tries to change the datum on outputs that go to scripts with a
-- prescribed tampering function, that only applies to datums of a certain type.
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

-- | FIXME
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
      modifiedOutputGroups = tail $ allCombinations modifiedOutputs
  msum $ map (setTweak txSkelOutsL) modifiedOutputGroups
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

    changeOnCorrectType :: Typeable b => b -> [Pl.BuiltinData]
    changeOnCorrectType datum = case typeOf datum `eqTypeRep` (typeRep @a) of
      Just HRefl -> change datum
      Nothing -> []

data MalformDatumLbl = MalformDatumLbl deriving (Show, Eq, Ord)

-- | FIXME: document
--
-- The first element of the result is the list consisting of all the first
-- elements of the inputs.
--
-- @allCombinations [[1,2,3], [4,5], [6]] == [[1,4,6], [1,5,6], [2,4,6], [2,5,6], [3,4,6], [3,5,6]]@
allCombinations :: [[a]] -> [[a]]
allCombinations [] = [[]]
allCombinations [[]] = [] -- included in the next one
allCombinations (first : rest) = [x : xs | x <- first, xs <- allCombinations rest]
