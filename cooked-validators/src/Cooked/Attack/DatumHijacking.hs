{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.Attack.DatumHijacking where

import Cooked.Attack.Common
import Cooked.MockChain.RawUPLC
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Optics
import qualified Ledger as L
import qualified Ledger.Typed.Scripts as L
import qualified PlutusTx as Pl
import Type.Reflection

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
  ( Typeable a,
    Pl.UnsafeFromData (L.DatumType a),
    Pl.UnsafeFromData (L.RedeemerType a)
  ) =>
  -- | Predicate to select outputs to steal, depending on the intended
  -- recipient, the datum, and the value.
  (L.TypedValidator a -> L.DatumType a -> L.Value -> Bool) ->
  -- | If the selection predicate matches more than one output, select the n-th
  -- output(s) with this predicate.
  (Integer -> Bool) ->
  Attack
datumHijackingAttack change select mcst skel =
  let thief = datumHijackingTarget @a

      changeRecipient :: PaysScriptConstraint -> Maybe PaysScriptConstraint
      changeRecipient (PaysScriptConstraint val sc dat money) =
        -- checks whether val _is of the same type as_ the thief, they're obviously different scripts.
        case val ~*~? thief of
          Just HRefl ->
            if change val dat money
              then Just $ PaysScriptConstraint thief sc dat money
              else Nothing
          Nothing -> Nothing
   in addLabel (DatumHijackingLbl $ L.validatorAddress thief)
        <$> mkSelectAttack paysScriptConstraintsT changeRecipient select mcst skel

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
