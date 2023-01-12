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
import Cooked.Attack.Tweak
import Cooked.MockChain.RawUPLC
import Cooked.Tx.Constraints.Optics
import Cooked.Tx.Constraints.Type
import qualified Ledger as L
import qualified Ledger.Typed.Scripts as L
import Optics.Core
import qualified PlutusTx as Pl
import Type.Reflection

-- | Redirect script outputs from one validator to another validator of the same
-- type. Returns the list of outputs it redirected (as they were before the
-- modification), in the order in which they occurred on the original
-- transaction.
--
-- Something like @txSkelOutsL % traversed % txSkelOutputToTypedValidatorP@
-- might be useful to construct the optics used by this tweak.
redirectScriptOutputTweak ::
  ( MonadTweak m,
    Is k A_Traversal,
    Show (L.DatumType a),
    Pl.ToData (L.DatumType a)
  ) =>
  Optic' k is TxSkel (ConcreteOutput (L.TypedValidator a) (TxSkelOutDatum (L.DatumType a)) L.Value) ->
  -- | Return @Just@ the new validator, or @Nothing@ if you want to leave this
  -- output unchanged.
  (ConcreteOutput (L.TypedValidator a) (TxSkelOutDatum (L.DatumType a)) L.Value -> Maybe (L.TypedValidator a)) ->
  -- | The redirection described by the previous argument might apply to more
  -- than one of the script outputs of the transaction. Use this predicate to
  -- select which of the redirectable script outputs to actually redirect. We
  -- count the redirectable script outputs from the left to the right, starting
  -- with zero.
  (Integer -> Bool) ->
  m [ConcreteOutput (L.TypedValidator a) (TxSkelOutDatum (L.DatumType a)) L.Value]
redirectScriptOutputTweak optic change =
  overMaybeTweakSelecting
    optic
    ( \output -> case change output of
        Nothing -> Nothing
        Just newValidator -> Just $ output & outputOwnerL .~ newValidator
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
  forall a m.
  ( MonadTweak m,
    Show (L.DatumType a),
    Pl.ToData (L.DatumType a),
    Typeable (L.DatumType a),
    Typeable a
  ) =>
  -- | Predicate to select outputs to steal, depending on the intended
  -- recipient, the datum, and the value.
  (ConcreteOutput (L.TypedValidator a) (TxSkelOutDatum (L.DatumType a)) L.Value -> Bool) ->
  -- | The selection predicate may match more than one output, restrict to the
  -- i-th of the output(s) (counting from the left, starting at zero) chosen by
  -- the selection predicate with this predicate.
  (Integer -> Bool) ->
  m [ConcreteOutput (L.TypedValidator a) (TxSkelOutDatum (L.DatumType a)) L.Value]
datumHijackingAttack change select = do
  redirected <-
    redirectScriptOutputTweak
      (txSkelOutsL % traversed % txSkelOutputToTypedValidatorP @a)
      (\output -> if change output then Just thief else Nothing)
      select
  guard . not $ null redirected
  addLabelTweak $ DatumHijackingLbl $ L.validatorAddress thief
  return redirected
  where
    thief = datumHijackingTarget @a

newtype DatumHijackingLbl = DatumHijackingLbl L.Address
  deriving (Show, Eq, Ord)

-- | The trivial validator that always succeds; this is a sufficient target for
-- the datum hijacking attack since we only want to show feasibility of the
-- attack.
datumHijackingTarget :: L.TypedValidator a
datumHijackingTarget = unsafeTypedValidatorFromUPLC (Pl.getPlc $$(Pl.compile [||tgt||]))
  where
    tgt :: Pl.BuiltinData -> Pl.BuiltinData -> Pl.BuiltinData -> ()
    tgt _ _ _ = ()
