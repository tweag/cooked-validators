{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cooked.Attack.Common where

import Control.Arrow
import Cooked.MockChain.Monad
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.UtxoPredicate
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Optics
import Data.Bifunctor hiding (first, second)
import Data.Default
import Data.Maybe
import qualified Ledger as L
import qualified Ledger.Typed.Scripts as L
import Optics.Core
import qualified PlutusTx as Pl

-- * The type of attacks

-- | The type of attacks that operate on a single transaction. The idea is that
-- in every 'MockChainSt'ate, there are number of possible modifications to a
-- transaction described by the given 'TxSkel'. Return @[]@ if the modification
-- does not apply. Use these attacks with the modalities from
-- 'MonadModalMockChain'.
type Attack = MockChainSt -> TxSkel -> [TxSkel]

-- * Constructing 'Attack's that return at most one modified transaction

-- | The simplest way to make an attack from an optic: Try to apply a given
-- function of type @a -> Maybe a@ to all foci of an optic, modifying all foci
-- that return @Just@. This attack returns a singleton list of the modified
-- 'TxSkel' if at least one modification was successful, i.e. if at least one of
-- the foci evaluates to @Just@; otherwise it returns @[]@.
mkAttack ::
  Is k A_Traversal =>
  -- | Optic focussing potentially interesting points to modify.
  Optic' k is TxSkel a ->
  -- | The modification to apply; return @Nothing@ if you want to leave the
  -- given focus as it is.
  (a -> Maybe a) ->
  Attack
mkAttack optic f =
  mkAccumLAttack
    optic
    ( \flag x -> case f x of
        Just y -> (y, True)
        Nothing -> (x, flag)
    )
    False
    (\_ skel flag -> [skel | flag])

-- | Traverse all foci of the given optic from the left to the right, while
-- counting the foci to which the modification successfully applies, and modify
-- only those modifiable foci whose index satisfies a given predicate. (This is
-- useful for example if you have many identical outputs on a transaction but
-- you only want to modify a few of them.) This attack returns a singleton list
-- of the modified 'TxSkel' if at least one modification was successful, i.e. if
-- at least one focus was modified; otherwise it returns @[]@.
mkSelectAttack ::
  Is k A_Traversal =>
  -- | Optic focussing potentially interesting points to modify.
  Optic' k is TxSkel a ->
  -- | The modification to apply; return @Nothing@ if you want to leave the
  -- given focus as it is.
  (a -> Maybe a) ->
  -- | Maybe the modification applies to (i.e. returns @Just@ on) more than one
  -- focus. Use this function to select the foci to modify by the order in which
  -- they are traversed. If you set this function to @const True@, you should
  -- probably use 'mkAttack', because that the semantic of that function.
  (Integer -> Bool) ->
  Attack
mkSelectAttack optic f select =
  mkAccumLAttack
    optic
    ( \(flag, index) x -> case f x of
        Just y ->
          if select index
            then (y, (True, index + 1))
            else (x, (flag, index + 1))
        Nothing -> (x, (flag, index))
    )
    (False, 0)
    (\_ skel (flag, _) -> [skel | flag])

-- * Constructing 'Attack's that return zero or more modified transactions

-- | This attack generates potentially very many modified TxSkels, or
-- potentially very few. For each focus, a list of possible modifications, each
-- with an associated label, is computed. The 'SplitStrategy' argument
-- determines how to combine modifications and labels.
--
-- By way of example, consider
--
-- > mkSplittingAttack strategy optic f g st skel
--
-- where @optic@ has three foci @x,y,z@ in @skel@. Assume that for each of these
-- foci, the lists of possible modifications together with their labels are
-- given by
--
-- > f st x = [(x1, 'a'), (x2, 'b')]
--
-- > f st y = [(y1, 'c'), (y2, 'd'), (y3, 'e')]
--
-- > f st z = []
--
-- The strategy 'OneChange' passes to @g@ all possible modifications of the
-- intial 'TxSkel' obtained by applying exactly one of the modifications. In the
-- example, this would mean 2 + 3 + 0 = 5 modified 'TxSkel's, namely the ones
-- corresponding to the following lists of (un)modified foci, together with
-- their (singleton) lists of labels:
--
-- > [ ([x1, y,  z], "a"),
-- >   ([x2, y,  z], "b"),
-- >   ([x,  y1, z], "c"),
-- >   ([x,  y2, z], "d"),
-- >   ([x,  y3, z], "e")
-- > ]
--
-- The strategy 'AllCombinations' passes to @g@ all modifications that change at
-- least focus. In the example, there are (2+1) * (3+1) * (0+1) = 12
-- combinations of applied and non-applied modifications, but one of them is the
-- one that leaves everything unchanged, which leaves us with the following 11:
--
-- > [ ([x,  y1, z], "c"),
-- >   ([x,  y2, z], "d"),
-- >   ([x,  y3, z], "e"),
-- >   ([x1, y,  z], "a"),
-- >   ([x1, y1, z], "ac"),
-- >   ([x1, y2, z], "ad"),
-- >   ([x1, y3, z], "ae"),
-- >   ([x2, y,  z], "b"),
-- >   ([x2, y1, z], "bc"),
-- >   ([x2, y2, z], "bd"),
-- >   ([x2, y3, z], "be")
-- > ]
mkSplittingAttack ::
  forall k is a b.
  Is k A_Traversal =>
  SplitStrategy ->
  -- | Optic focussing potentially interesting points to modify
  Optic' k is TxSkel a ->
  -- | Function that returns possible modifications of the current focus,
  -- together with some piece of extra information (labels)
  (MockChainSt -> a -> [(a, b)]) ->
  -- | function to look at a modified 'TxSkel' and the list of labels of all
  -- modifications applied to it, in order to apply optional further
  -- modifications. The order of labels is not specified (yet?)
  ([b] -> TxSkel -> [TxSkel]) ->
  Attack
mkSplittingAttack strategy optic f g mcst skel = modifiedSkels
  where
    modifiedSkels :: [TxSkel]
    modifiedSkels =
      concatMap
        ( \(foci, mods) ->
            g mods $
              set (partsOf optic) foci skel
        )
        $ splitter fociWithOptions

    -- The list 'fociWithOptions' has one element for each focus. This element
    -- is a pair of the unmodified focus and a list of potential modifications
    -- to that focus, together with the label associated to that modified focus.
    fociWithOptions :: [(a, [(a, b)])]
    fociWithOptions = map (\x -> (x, f mcst x)) $ view (partsOf optic) skel

    -- We expect the invariant
    --
    -- > all (\r -> length (fst r) == length xs) (splitter xs)
    --
    -- for 'splitter': The elements of the returned list are pairs of
    --
    -- - a list of (potentially) modified foci and
    --
    -- - a list of labels to be applied to the 'TxSkel' with the modifed foci.
    splitter :: [(a, [(a, b)])] -> [([a], [b])]
    splitter = case strategy of
      OneChange -> oneChange
      AllCombinations -> allCombinations

oneChange :: [(a, [(a, b)])] -> [([a], [b])]
oneChange = inner []
  where
    inner :: [a] -> [(a, [(a, b)])] -> [([a], [b])]
    inner l ((x, ms) : r) = map (\(x', h) -> (l ++ x' : map fst r, [h])) ms ++ inner (l ++ [x]) r
    inner _ _ = []

allCombinations :: [(a, [(a, b)])] -> [([a], [b])]
allCombinations = tailSafe . allCombinations'
  where
    allCombinations' :: [(a, [(a, b)])] -> [([a], [b])]
    allCombinations' [] = []
    allCombinations' [(a, os)] = ([a], []) : map (bimap (: []) (: [])) os
    allCombinations' ((a, os) : r) =
      let r' = allCombinations' r
       in map (first (a :)) r'
            ++ concatMap (\(a', m) -> map (bimap (a' :) (m :)) r') os

    tailSafe :: [t] -> [t]
    tailSafe [] = []
    tailSafe (_ : xs) = xs

data SplitStrategy = OneChange | AllCombinations

-- | A very general attack: Traverse all foci of the optic from the left to the
-- right, collecting an accumulator while also (optionally) modifying the
-- 'TxSkel'. At the end, look at the modified 'TxSkel' and the accumulator to
-- decide whether the traversal was a success, or whether we should return @[]@.
mkAccumLAttack ::
  Is k A_Traversal =>
  -- | Optic focussing potentially interesting points to modify
  Optic' k is TxSkel a ->
  -- | function that describes the modification of the accumulator and the
  -- current focus.
  (acc -> a -> (a, acc)) ->
  -- | initial accumulator
  acc ->
  -- | function to decide whether the traversal modified the 'TxSkel' in the
  -- desired way. This will typically look like this:
  -- > \state skel acc -> if someTest state skel acc
  -- >                    then computeFinalModifications state skel acc
  -- >                    else []
  (MockChainSt -> TxSkel -> acc -> [TxSkel]) ->
  Attack
mkAccumLAttack optic f initAcc test mcst skel =
  let (skel', acc) = mapAccumLOf optic f initAcc skel
   in test mcst skel' acc

-- * Helpers to interact with 'MockChainSt'

-- | Like 'utxosSuchThat', but with the 'MockChainSt'ate as an explicit argument
utxosSuchThatMcst ::
  (Pl.FromData a) =>
  MockChainSt ->
  L.Address ->
  UtxoPredicate a ->
  [(SpendableOut, Maybe a)]
utxosSuchThatMcst mcst addr select =
  case runMockChainRaw def mcst (utxosSuchThat addr select) of
    Left _ -> []
    Right (utxos, _) -> utxos

-- | Like 'scriptUtxosSuchThat', but with the 'MockChainSt'ate as an explicit
-- argument
scriptUtxosSuchThatMcst ::
  (Pl.FromData (L.DatumType a)) =>
  MockChainSt ->
  L.TypedValidator a ->
  (L.DatumType a -> L.Value -> Bool) ->
  [(SpendableOut, L.DatumType a)]
scriptUtxosSuchThatMcst mcst val select =
  map (second fromJust) $
    utxosSuchThatMcst
      mcst
      (L.validatorAddress val)
      (maybe (const False) select)

-- * General helpers

-- | Add a label to a 'TxSkel'. If there is already a pre-existing label, the
-- given label will be added, forming a pair @(newlabel, oldlabel)@.
addLabel :: LabelConstrs x => x -> TxSkel -> TxSkel
addLabel newlabel =
  over
    txLabelL
    ( \case
        TxLabel Nothing -> TxLabel $ Just newlabel
        TxLabel (Just oldlabel) -> TxLabel $ Just (newlabel, oldlabel)
    )
