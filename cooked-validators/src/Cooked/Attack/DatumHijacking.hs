{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.Attack.DatumHijacking where

import Control.Monad
import Cooked.Attack.Common
import Cooked.MockChain.RawUPLC
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Optics
import Data.List
import qualified Ledger as L
import qualified Ledger.Typed.Scripts as L
import Optics.Core
import qualified PlutusTx as Pl

removeOutConstraintsAttack ::
  (OutConstraint -> Bool) ->
  Attack [OutConstraint]
removeOutConstraintsAttack removePred = Attack $
  \_mcst skel ->
    [ let ocs = view outConstraintsL skel
          (removed, kept) = partition removePred ocs
       in ( set outConstraintsL kept skel,
            removed
          )
    ]

addOutConstraintAttack ::
  OutConstraint ->
  Attack ()
addOutConstraintAttack oc = Attack $
  \_mcst skel -> [(over outConstraintsL (++ [oc]) skel, ())]

-- | Redirect 'PaysScript's from one validator to another validator of the same
-- type. Returns the list of outputs it redirected (as they were before the
-- modification), in the order in which they occurred on the original
-- transaction.
--
-- If no output is redirected, this attack fails.
--
-- Something like 'paysScriptCoinstraintTypeP' might be useful to construct the
-- optics used by this attack.
redirectScriptOutputAttack ::
  Is k A_Traversal =>
  Optic' k is TxSkel (L.TypedValidator a, L.DatumType a, L.Value) ->
  -- | Return @Just@ the new validator, or @Nothing@ if you want to leave this
  -- output unchanged.
  (L.TypedValidator a -> L.DatumType a -> L.Value -> Maybe (L.TypedValidator a)) ->
  -- | The redirection described by the previous argument might apply to more
  -- than one of the script outputs of the transaction. Use this predicate to
  -- select which of the redirectable script outputs to actually redirect. We
  -- count the redirectable script outputs from the left to the right, starting
  -- with zero.
  (Integer -> Bool) ->
  Attack [(L.TypedValidator a, L.DatumType a, L.Value)]
redirectScriptOutputAttack optic change =
  mkSelectAttack
    optic
    ( \_mcst (oldVal, dat, money) ->
        case change oldVal dat money of
          Nothing -> Nothing
          Just newVal -> Just (newVal, dat, money)
    )

-- | A datum hijacking attack, simplified: This attack tries to substitute a
-- different recipient on 'PaysScript' constraints, but leaves the datum as it
-- is. That is, it tests for careless uses of something like 'txInfoOutputs' in
-- places where something like 'getContinuingOutputs' should be used. If this
-- attack goes through, however, a "proper" datum hijacking attack that modifies
-- the datum in a way that the (relevant part of) the
-- 'toBuiltinData'-translation stays the same will also work.
--
-- A 'DatumHijackingLbl' with the hash of the "thief" validator is added to the
-- labels of the 'TxSkel' using 'addLabel'.
--
-- This attack returns the list of outputs it redirected, in the order in which
-- they occurred on the original transaction. If no output is redirected, this
-- attack fails.
datumHijackingAttack ::
  forall a.
  PaysScriptConstrs a =>
  -- | Predicate to select outputs to steal, depending on the intended
  -- recipient, the datum, and the value.
  (L.TypedValidator a -> L.DatumType a -> L.Value -> Bool) ->
  -- | The selection predicate may match more than one output, restrict to the
  -- i-th of the output(s) (counting from the left, starting at zero) chosen by
  -- the selection predicate with this predicate.
  (Integer -> Bool) ->
  Attack [(L.TypedValidator a, L.DatumType a, L.Value)]
datumHijackingAttack change select =
  let thief = datumHijackingTarget @a
   in do
        redirected <-
          redirectScriptOutputAttack
            (paysScriptConstraintsT % paysScriptConstraintTypeP @a)
            (\val dat money -> if change val dat money then Just thief else Nothing)
            select
        addLabelAttack $ DatumHijackingLbl $ L.validatorAddress thief
        return redirected

newtype DatumHijackingLbl = DatumHijackingLbl L.Address
  deriving (Show, Eq)

-- | The trivial validator that always succeds; this is a sufficient target for
-- the datum hijacking attack since we only want to show feasibility of the
-- attack.
datumHijackingTarget :: L.TypedValidator a
datumHijackingTarget = unsafeTypedValidatorFromUPLC (Pl.getPlc $$(Pl.compile [||tgt||]))
  where
    tgt :: Pl.BuiltinData -> Pl.BuiltinData -> Pl.BuiltinData -> ()
    tgt _ _ _ = ()
