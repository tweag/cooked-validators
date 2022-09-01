{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cooked.Attack.Common where

import Control.Arrow
import Control.Monad
import Cooked.MockChain.Monad
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.UtxoPredicate
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Optics
import Data.Default
import Data.Maybe
import qualified Ledger as L
import qualified Ledger.Typed.Scripts as L
import Optics.Core
import qualified PlutusTx as Pl

-- * The type of attacks

newtype Attack a = Attack (MockChainSt -> TxSkel -> Maybe (TxSkel, a))

data UntypedAttack where
  UntypedAttack :: Attack a -> UntypedAttack

instance Functor Attack where
  fmap f (Attack g) = Attack $ \mcst skel -> g mcst skel >>= Just . second f

instance Applicative Attack where
  pure x = Attack $ \_ skel -> Just (skel, x)
  (<*>) = ap

instance Monad Attack where
  Attack g >>= h =
    Attack $ \mcst skel -> case g mcst skel of
      Nothing -> Nothing
      Just (skel', x) -> case h x of
        Attack f -> f mcst skel'

-- * Some frequently used methods to combine attacks

-- | The never-applicable attack.
failingAttack :: Attack a
failingAttack = Attack $ \_ _ -> Nothing

-- | The attack that always applies and leaves the transaction unchanged.
doNothingAttack :: Attack ()
doNothingAttack = Attack (\_ skel -> Just (skel, ()))

-- | Test a boolean condition, leaving the 'TxSkel' unmodified if the condition
-- holds, returning @Nothing@ otherwise.
guardAttack :: Bool -> Attack ()
guardAttack True = doNothingAttack
guardAttack False = failingAttack

-- | Add a label to a 'TxSkel'. If there is already a pre-existing label, the
-- given label will be added, forming a pair @(newlabel, oldlabel)@.
addLabelAttack :: LabelConstrs x => x -> Attack ()
addLabelAttack newlabel = Attack $ \_mcst skel ->
  Just
    ( over
        txLabelL
        ( \case
            TxLabel Nothing -> TxLabel $ Just newlabel
            TxLabel (Just oldlabel) -> TxLabel $ Just (newlabel, oldlabel)
        )
        skel,
      ()
    )

-- | Turn a potentially failing attack into an attack that always returns
-- @Just@. In cases where the original attack would have not been applicable
-- (i.e. returned @Nothing@), return the pair @(unmodified 'TxSkel', Nothing)@.
skipFailure :: Attack a -> Attack (Maybe a)
skipFailure (Attack f) = Attack $
  \mcst skel -> Just $ maybe (skel, Nothing) (second Just) (f mcst skel)

-- | Apply all the attacks in the given list, one after the other, and succeed
-- if at least one of the attacks succeeded. Returns the value returned by the
-- last successful of the given attacks. This will be equivalent to
-- 'failingAttack' for an empty input list of attacks.
atLeastOneSuccessAttack ::
  [Attack a] -> Attack a
atLeastOneSuccessAttack attacks = do
  returnValues <- mapM skipFailure attacks
  maybe failingAttack return (lastJust returnValues)
  where
    -- return the last entry in the list that is a @Just@, or @Nothing@, if all
    -- entries are @Nothing@.
    lastJust :: [Maybe a] -> Maybe a
    lastJust [] = Nothing
    lastJust (Just x : xs) = case lastJust xs of
      Just y -> Just y
      Nothing -> Just x
    lastJust (Nothing : xs) = lastJust xs

-- -- * Constructing 'Attack's that return at most one modified transaction

-- -- | The simplest way to make an attack from an optic: Try to apply a given
-- -- function of type @a -> Maybe a@ to all foci of an optic, modifying all foci
-- -- that return @Just@. This attack returns a singleton list of the modified
-- -- 'TxSkel' if at least one modification was successful, i.e. if at least one of
-- -- the foci evaluates to @Just@; otherwise it returns @[]@.
-- mkAttack ::
--   Is k A_Traversal =>
--   -- | Optic focussing potentially interesting points to modify.
--   Optic' k is TxSkel a ->
--   -- | The modification to apply; return @Nothing@ if you want to leave the
--   -- given focus as it is.
--   (a -> Maybe a) ->
--   Attack []
-- mkAttack optic f =
--   mkAccumLAttack
--     optic
--     ( \flag x -> case f x of
--         Just y -> (y, True)
--         Nothing -> (x, flag)
--     )
--     False
--     (\_ skel flag -> [skel | flag])

-- | Traverse all foci of the given optic from the left to the right, while
-- counting the foci to which the modification successfully applies, and modify
-- only those modifiable foci whose index satisfies a given predicate. (This is
-- useful for example if you have many identical outputs on a transaction but
-- you only want to modify a few of them.) Returns a list of the modified foci,
-- as they were before the modification, and in the order in which they occurred
-- on the original transaction.
--
-- If no foci were modified, this attack fails.
mkSelectAttack ::
  (Is k A_Traversal) =>
  -- | Optic focussing potentially interesting points to modify.
  Optic' k is TxSkel a ->
  -- | The modification to apply; return @Nothing@ if you want to leave the
  -- given focus as it is.
  (MockChainSt -> a -> Maybe a) ->
  -- | Maybe more than one of the foci is modifiable (i.e. the modification
  -- returns @Just@). Use this predicate to selectively apply the modification
  -- only to some of the modifiable foci: This attack conts the modifiable foci
  -- (in the order of the traversal, starting with 0) and only applies the
  -- modification to the @i@-th modifiable focus if @i@ satisfies the predicate.
  (Integer -> Bool) ->
  Attack [a]
mkSelectAttack optic change select = do
  modified <-
    mkAccumLAttack
      optic
      ( \mcst (index, acc) oldFocus ->
          case change mcst oldFocus of
            Just newFocus ->
              if select index
                then (newFocus, (index + 1, oldFocus : acc))
                else (oldFocus, (index + 1, acc))
            Nothing -> (oldFocus, (index, acc))
      )
      (0, [])
  guardAttack (not $ null modified)
  return $ reverse $ snd modified

-- -- * Constructing 'Attack's that return zero or more modified transactions

-- -- | This attack generates potentially very many modified TxSkels, or
-- -- potentially very few. For each focus, a list of possible modifications, each
-- -- with an associated label, is computed. The 'SplitStrategy' argument
-- -- determines how to combine modifications and labels.
-- --
-- -- By way of example, consider
-- --
-- -- > mkSplittingAttack strategy optic f g st skel
-- --
-- -- where @optic@ has three foci @x,y,z@ in @skel@. Assume that for each of these
-- -- foci, the lists of possible modifications together with their labels are
-- -- given by
-- --
-- -- > f st x = [(x1, 'a'), (x2, 'b')]
-- --
-- -- > f st y = [(y1, 'c'), (y2, 'd'), (y3, 'e')]
-- --
-- -- > f st z = []
-- --
-- -- The strategy 'OneChange' passes to @g@ all possible modifications of the
-- -- intial 'TxSkel' obtained by applying exactly one of the modifications. In the
-- -- example, this would mean 2 + 3 + 0 = 5 modified 'TxSkel's, namely the ones
-- -- corresponding to the following lists of (un)modified foci, together with
-- -- their (singleton) lists of labels:
-- --
-- -- > [ ([x1, y,  z], "a"),
-- -- >   ([x2, y,  z], "b"),
-- -- >   ([x,  y1, z], "c"),
-- -- >   ([x,  y2, z], "d"),
-- -- >   ([x,  y3, z], "e")
-- -- > ]
-- --
-- -- The strategy 'AllCombinations' passes to @g@ all modifications that change at
-- -- least focus. In the example, there are (2+1) * (3+1) * (0+1) = 12
-- -- combinations of applied and non-applied modifications, but one of them is the
-- -- one that leaves everything unchanged, which leaves us with the following 11:
-- --
-- -- > [ ([x,  y1, z], "c"),
-- -- >   ([x,  y2, z], "d"),
-- -- >   ([x,  y3, z], "e"),
-- -- >   ([x1, y,  z], "a"),
-- -- >   ([x1, y1, z], "ac"),
-- -- >   ([x1, y2, z], "ad"),
-- -- >   ([x1, y3, z], "ae"),
-- -- >   ([x2, y,  z], "b"),
-- -- >   ([x2, y1, z], "bc"),
-- -- >   ([x2, y2, z], "bd"),
-- -- >   ([x2, y3, z], "be")
-- -- > ]
-- mkSplittingAttack ::
--   forall a b k is.
--   Is k A_Traversal =>
--   SplitStrategy ->
--   -- | Optic focussing potentially interesting points to modify
--   Optic' k is TxSkel a ->
--   -- | Function that returns possible modifications of the current focus,
--   -- together with some piece of extra information (labels)
--   (MockChainSt -> a -> [(a, b)]) ->
--   -- | function to look at a modified 'TxSkel' and the list of labels of all
--   -- modifications applied to it, in order to apply optional further
--   -- modifications. The order of labels is not specified (yet?)
--   ([b] -> TxSkel -> [TxSkel]) ->
--   Attack
-- mkSplittingAttack strategy optic f g mcst skel = modifiedSkels
--   where
--     modifiedSkels :: [TxSkel]
--     modifiedSkels =
--       concatMap
--         ( \(foci, mods) ->
--             g mods $
--               set (partsOf optic) foci skel
--         )
--         $ splitter fociWithOptions

--     -- The list 'fociWithOptions' has one element for each focus. This element
--     -- is a pair of the unmodified focus and a list of potential modifications
--     -- to that focus, together with the label associated to that modified focus.
--     fociWithOptions :: [(a, [(a, b)])]
--     fociWithOptions = map (\x -> (x, f mcst x)) $ view (partsOf optic) skel

--     -- We expect the invariant
--     --
--     -- > all (\r -> length (fst r) == length xs) (splitter xs)
--     --
--     -- for 'splitter': The elements of the returned list are pairs of
--     --
--     -- - a list of (potentially) modified foci and
--     --
--     -- - a list of labels to be applied to the 'TxSkel' with the modifed foci.
--     splitter :: [(a, [(a, b)])] -> [([a], [b])]
--     splitter = case strategy of
--       OneChange -> oneChange
--       AllCombinations -> allCombinations

-- oneChange :: [(a, [(a, b)])] -> [([a], [b])]
-- oneChange = inner []
--   where
--     inner :: [a] -> [(a, [(a, b)])] -> [([a], [b])]
--     inner l ((x, ms) : r) = map (\(x', h) -> (l ++ x' : map fst r, [h])) ms ++ inner (l ++ [x]) r
--     inner _ _ = []

-- allCombinations :: [(a, [(a, b)])] -> [([a], [b])]
-- allCombinations = tailSafe . allCombinations'
--   where
--     allCombinations' :: [(a, [(a, b)])] -> [([a], [b])]
--     allCombinations' [] = []
--     allCombinations' [(a, os)] = ([a], []) : map (bimap (: []) (: [])) os
--     allCombinations' ((a, os) : r) =
--       let r' = allCombinations' r
--        in map (first (a :)) r'
--             ++ concatMap (\(a', m) -> map (bimap (a' :) (m :)) r') os

--     tailSafe :: [t] -> [t]
--     tailSafe [] = []
--     tailSafe (_ : xs) = xs

data SplitStrategy = OneChange | AllCombinations

-- | A very general attack: Traverse all foci of the optic from the left to the
-- right, collecting an accumulator while also modifying the foci.
--
-- This attack never fails, i.e. always returns 'Just' the modified 'TxSkel',
-- even if no modifications were made.
mkAccumLAttack ::
  Is k A_Traversal =>
  -- | Optic focussing potentially interesting points to modify
  Optic' k is TxSkel a ->
  -- | function that describes the modification of the accumulator and the
  -- current focus.
  (MockChainSt -> acc -> a -> (a, acc)) ->
  -- | initial accumulator
  acc ->
  Attack acc
mkAccumLAttack optic f initAcc = Attack $ \mcst skel ->
  Just $ mapAccumLOf optic (f mcst) initAcc skel

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
