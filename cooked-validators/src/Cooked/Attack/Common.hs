{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Cooked.Attack.Common where

import Cooked.MockChain.Monad.Direct
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Optics
import Optics.Core

-- * The type of attacks

-- | The type of attacks that operate on a single transaction. The idea is that
-- in every 'MockChainSt'ate, there are number of possible modifications to a
-- transaction described by the given 'TxSkel'. Return @[]@ if the modification
-- does not apply. Use these attacks with the modalities from
-- 'MonadModalMockChain'.
type Attack = MockChainSt -> TxSkel -> [TxSkel]

-- * Constructing attacks that return at most one modified transaction

-- | The simplest way to make an attack from an optic: Try to apply a given
-- function of type @a -> Maybe a@ to all foci of an optic, modifying all foci
-- that return @Just@. This attack returns @Just@ if at least one modification
-- was successful, i.e. if at least one of the foci evaluates to @Just@;
-- otherwise it returns @Nothing@.
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

-- | A more fine-grained attack: Traverse all foci of the optic from the left to
-- the right, while counting the foci to which the modification successfully
-- applies, and modify only those modifiable foci whose index satisfies a given
-- predicate. This is useful if you have many indentical outputs but you only
-- want to modify a few of them.
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

-- | A very general attack: Traverse all foci of the optic from the left to the
-- right, collecting an accumulator while also (optionally) modifying the
-- 'TxSkel'. At the end, look at the modified 'TxSkel' and the accumulator to
-- decide whether the traversal was a success, or whether we should return
-- @Nothing@.
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
  -- >                    then Just (someFinalModification state skel acc)
  -- >                    else Nothing
  (MockChainSt -> TxSkel -> acc -> [TxSkel]) ->
  Attack
mkAccumLAttack optic f initAcc test mcst skel =
  let (skel', acc) = mapAccumLOf optic f initAcc skel
   in test mcst skel' acc

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
