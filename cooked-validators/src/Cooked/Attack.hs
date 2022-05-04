{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.Attack where

import Cooked.MockChain
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Optics
import Data.Maybe
import qualified Ledger as L hiding (validatorHash)
import qualified Ledger.Typed.Scripts as L
import Optics.Core
import qualified PlutusTx as Pl
import qualified PlutusTx.Numeric as Pl (negate)
import Type.Reflection

-- * The type of attacks, and functions to turn optics into attacks

-- | The type of attacks that operate on a single transaction. The idea is to
-- try to modify a transaction, or return @Nothing@ if the modification does not
-- apply; use this with the 'somewhere' and 'everywhere' modalities from
-- "Cooked.MockChain.Monad".
type Attack = TxSkel -> Maybe TxSkel

-- | The simplest way to make an attack from an optic: Try to apply the given
-- function to all of the optic's foci. Fail, returning @Nothing@, if nothing is
-- in focus or if all foci are evaluated to @Nothing@.
mkAttack :: Is k A_Traversal => Optic' k is TxSkel a -> (a -> Maybe a) -> Attack
mkAttack optic f =
  mkAccumLAttack
    optic
    ( \flag x -> case f x of
        Just y -> (y, True)
        Nothing -> (x, flag)
    )
    False
    (\skel flag -> if flag then Just skel else Nothing)

-- | A more fine-grained attack: Traverse all foci of the optic from the left to
-- the right, collecting an accumulator while also (optionally) modifying the
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
  -- > \skel acc -> if someTest skel acc
  -- >               then Just (someFinalModification skel)
  -- >               else Nothing
  (TxSkel -> acc -> Maybe TxSkel) ->
  Attack
mkAccumLAttack optic f initAcc test skel =
  let (skel', acc) = mapAccumLOf optic f initAcc skel
   in test skel' acc

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

-- * Token duplication attack

-- | A token duplication attack which modifies every 'Mints'-constraint of a
-- 'TxSkel' that satisfies some conditions. This adds a 'DupTokenLbl' to the
-- labels of the transaction using 'addLabel'.
dupTokenAttack ::
  -- | A function describing how the amount of minted tokens of an asset class
  -- should be changed by the attack. The given function @f@ should probably satisfy
  -- > f ac i == Just j -> i < j
  -- for all @ac@ and @i@, i.e. it should increase in the minted amount.
  -- If it does *not* increase the minted amount, or if @f ac i == Nothing@, the
  -- minted amount will be left unchanged.
  (L.AssetClass -> Integer -> Maybe Integer) ->
  -- | The wallet of the attacker. Any extra tokens that were minted by the
  -- transaction are paid to this wallet.
  Wallet ->
  Attack
dupTokenAttack change attacker skel =
  addLabel DupTokenLbl . paySurplusTo attacker
    <$> mkAttack (mintsConstraintsT % valueL) increaseValue skel
  where
    increaseValue :: L.Value -> Maybe L.Value
    increaseValue v =
      case someJust
        ( \(ac, i) -> case change ac i of
            Nothing -> Nothing
            Just j -> if i < j then Just (ac, j) else Nothing
        )
        (view flattenValueI v) of
        Just l -> Just $ review flattenValueI l
        Nothing -> Nothing

    paySurplusTo :: Wallet -> TxSkel -> TxSkel
    paySurplusTo w s = over outConstraintsL (paysPK (walletPKHash w) surplus :) s
      where
        surplus = txSkelInValue s <> Pl.negate (nonAdaValue $ txSkelOutValue s)

    -- Try to use the given function to modify the elements of the given list. If
    -- at least one modification is successful, return 'Just' the modified list,
    -- otherwise fail returning 'Nothing'.
    someJust :: (a -> Maybe a) -> [a] -> Maybe [a]
    someJust _ [] = Nothing
    someJust f (x : xs) = case f x of
      Just y -> Just $ y : map (\a -> fromMaybe a (f a)) xs
      Nothing -> (x :) <$> someJust f xs

data DupTokenLbl = DupTokenLbl
  deriving (Eq, Show)

-- * Datum hijacking attack

-- | A datum hijacking attack, simplified: This attack tries to substitute a
-- different recipient on 'PaysScript' constraints, but leaves the datum as it
-- is. That is, it tests for careless uses of something like 'txInfoOutputs' in
-- places where something like 'getContinuingOutputs' should be used. If this
-- attack goes through, however, a "proper" datum hijacking attack that modifies
-- the datum in a way that the (relevant part of) the
-- 'toBuiltinData'-translation stays the same will also work. A
-- 'DatumHijackingLbl' is added to the labels of the 'TxSkel' using 'addLabel'.
datumHijackingAttack ::
  forall a.
  ( Typeable a,
    Pl.UnsafeFromData (L.DatumType a),
    Pl.UnsafeFromData (L.RedeemerType a)
  ) =>
  -- | Function to select outputs to steal, depending on the intended recipient,
  -- the datum, and the value.
  (L.TypedValidator a -> L.DatumType a -> L.Value -> Bool) ->
  -- | If the selection predicate matches more than one output, select the n-th
  -- output by passing @Just n@, or all by passing @Nothing@.
  Maybe Integer ->
  Attack
datumHijackingAttack change nth skel =
  addLabel DatumHijackingLbl
    <$> mkAccumLAttack paysScriptConstraintsT changeRecipient (0, False) checkReturn skel
  where
    checkReturn :: TxSkel -> (Integer, Bool) -> Maybe TxSkel
    checkReturn _ (_, False) = Nothing
    checkReturn s (_, True) = Just s

    changeRecipient :: (Integer, Bool) -> PaysScriptConstraint -> (PaysScriptConstraint, (Integer, Bool))
    changeRecipient (i, flag) c@(PaysScriptConstraint val dat money) =
      -- checks whether val _is of the same type as_ the hijacking target,
      -- they're obviously different scripts.
      case val ~*~? datumHijackingTarget @a of
        Just HRefl ->
          if change val dat money && i == fromMaybe i nth
            then (PaysScriptConstraint @a datumHijackingTarget dat money, (i + 1, True))
            else (c, (i, flag))
        Nothing -> (c, (i, flag))

data DatumHijackingLbl = DatumHijackingLbl
  deriving (Show, Eq)

-- | The trivial validator that always succeds; this is a sufficient target for
-- the datum hijacking attack since we only want to show feasibility of the
-- attack.
datumHijackingTarget :: L.TypedValidator a
datumHijackingTarget = unsafeTypedValidatorFromUPLC (Pl.getPlc $$(Pl.compile [||tgt||]))
  where
    tgt :: Pl.BuiltinData -> Pl.BuiltinData -> Pl.BuiltinData -> ()
    tgt _ _ _ = ()
