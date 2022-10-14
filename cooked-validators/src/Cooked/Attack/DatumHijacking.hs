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

import Cooked.Attack.Tweak
import Cooked.MockChain.RawUPLC
import Cooked.Tx.Constraints
import qualified Ledger as L
import qualified Ledger.Typed.Scripts as L
import Optics.Core
import qualified Plutus.V1.Ledger.Credential as L
import qualified PlutusTx as Pl

-- | Redirect 'PaysScript's from one validator to another validator of the same
-- type. Returns the list of outputs it redirected (as they were before the
-- modification), in the order in which they occurred on the original
-- transaction.
--
-- If no output is redirected, this tweak fails.
--
-- Something like 'paysScriptCoinstraintTypeP' might be useful to construct the
-- optics used by this tweak.
redirectScriptOutputTweak ::
  Is k A_Traversal =>
  Optic' k is TxSkel (L.TypedValidator a, Maybe L.StakingCredential, L.DatumType a, L.Value) ->
  -- | Return @Just@ the new validator, or @Nothing@ if you want to leave this
  -- output unchanged.
  (L.TypedValidator a -> Maybe L.StakingCredential -> L.DatumType a -> L.Value -> Maybe (L.TypedValidator a)) ->
  -- | The redirection described by the previous argument might apply to more
  -- than one of the script outputs of the transaction. Use this predicate to
  -- select which of the redirectable script outputs to actually redirect. We
  -- count the redirectable script outputs from the left to the right, starting
  -- with zero.
  (Integer -> Bool) ->
  Tweak [(L.TypedValidator a, Maybe L.StakingCredential, L.DatumType a, L.Value)]
redirectScriptOutputTweak optic change =
  mkSelectTweak
    optic
    ( \_mcst (oldVal, mStakingCred, dat, money) ->
        case change oldVal mStakingCred dat money of
          Nothing -> Nothing
          Just newVal -> Just (newVal, mStakingCred, dat, money)
    )

-- | A datum hijacking attack, simplified: This attack tries to substitute a
-- different recipient on 'PaysScript' constraints, but leaves the datum as it
-- is. That is, it tests for careless uses of something like 'txInfoOutputs' in
-- places where something like 'getContinuingOutputs' should be used. If this
-- attack goes through, however, a "proper" datum hijacking attack that modifies
-- the datum in a way that (the relevant part of) the
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
  Tweak [(L.TypedValidator a, Maybe L.StakingCredential, L.DatumType a, L.Value)]
datumHijackingAttack change select =
  let thief = datumHijackingTarget @a
   in do
        redirected <-
          redirectScriptOutputTweak
            (paysScriptConstraintsT % paysScriptConstraintTypeP @a)
            (\val _mStakingCred dat money -> if change val dat money then Just thief else Nothing)
            select
        addLabelTweak $ DatumHijackingLbl $ L.validatorAddress thief
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
