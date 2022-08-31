{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.Attack.DatumHijacking where

import Cooked.Attack.Common
import Cooked.MockChain.RawUPLC
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Optics
import Data.List
import Data.Maybe
import qualified Ledger as L
import qualified Ledger.Typed.Scripts as L
import Optics.Core
import qualified PlutusTx as Pl
import Type.Reflection

removeOutConstraintsAttack ::
  (OutConstraint -> Bool) ->
  Attack [OutConstraint]
removeOutConstraintsAttack removePred = Attack $
  \_mcst skel ->
    Just $
      let ocs = view outConstraintsL skel
          (removed, kept) = partition removePred ocs
       in ( set outConstraintsL kept skel,
            removed
          )

addOutConstraintAttack ::
  OutConstraint ->
  Attack ()
addOutConstraintAttack oc = Attack $
  \_mcst skel -> Just (over outConstraintsL (++ [oc]) skel, ())

-- | A datum hijacking attack, simplified: This attack tries to substitute a
-- different recipient on 'PaysScript' constraints, but leaves the datum as it
-- is. That is, it tests for careless uses of something like 'txInfoOutputs' in
-- places where something like 'getContinuingOutputs' should be used. If this
-- attack goes through, however, a "proper" datum hijacking attack that modifies
-- the datum in a way that the (relevant part of) the
-- 'toBuiltinData'-translation stays the same will also work. A
-- 'DatumHijackingLbl' with the hash of the "thief" validator is added to the
-- labels of the 'TxSkel' using 'addLabel'.
datumHijackingAttack ::
  forall a.
  PaysScriptConstrs a =>
  -- | Predicate to select outputs to steal, depending on the intended
  -- recipient, the datum, and the value.
  (L.TypedValidator a -> L.DatumType a -> L.Value -> Bool) ->
  -- | If the selection predicate matches more than one output, select the n-th
  -- output(s) with this predicate.
  (Integer -> Bool) ->
  Attack ()
datumHijackingAttack change select =
  let thief = datumHijackingTarget @a
   in do
        removedOuts <-
          removeOutConstraintsAttack
            ( \case
                PaysScript val dat money ->
                  case val ~*~? thief of
                    Just HRefl -> change val dat money
                    Nothing -> False
                _ -> False
            )
        atLeastOneSuccessAttack
          ( zipWith
              ( \psc i ->
                  if select i
                    then -- We know that @removedOuts@ contains only
                    -- 'PaysScript' constraints to a validator of type @a@, so
                    -- the following call to @fromJust@ can never fail.

                      let (_, dat, money) = fromJust (preview (paysScriptConstraintP % paysScriptConstraintTypeP @a) psc)
                       in addOutConstraintAttack $ PaysScript thief dat money
                    else failingAttack
              )
              removedOuts
              [0 ..]
          )
        addLabelAttack $ DatumHijackingLbl $ L.validatorAddress thief

newtype DatumHijakingLbl = DatumHijackingLbl L.Address
  deriving (Show, Eq)

-- | The trivial validator that always succeds; this is a sufficient target for
-- the datum hijacking attack since we only want to show feasibility of the
-- attack.
datumHijackingTarget :: L.TypedValidator a
datumHijackingTarget = unsafeTypedValidatorFromUPLC (Pl.getPlc $$(Pl.compile [||tgt||]))
  where
    tgt :: Pl.BuiltinData -> Pl.BuiltinData -> Pl.BuiltinData -> ()
    tgt _ _ _ = ()
