{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.Attack.DatumHijacking
  ( redirectScriptOutputTweak,
    datumHijackingAttack,
    DatumHijackingLbl (..),
    datumHijackingTarget,
  )
where

import Control.Monad
import Cooked.Output
import Cooked.Pretty.Class
import Cooked.RawUPLC
import Cooked.Skeleton
import Cooked.Tweak
import Optics.Core
import qualified Plutus.Script.Utils.Typed as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import qualified PlutusTx as Pl
import Type.Reflection

-- | Redirect script outputs from one validator to another validator of the same
-- type. Returns the list of outputs it redirected (as they were before the
-- modification), in the order in which they occurred on the original
-- transaction.
--
-- Something like @txSkelOutsL % traversed % txSkelOutOwnerTypeP @(Pl.TypedValidator a)@
-- might be useful to construct the optics used by this tweak.
redirectScriptOutputTweak ::
  ( MonadTweak m,
    Is k A_Traversal,
    Show (Pl.DatumType a),
    Pl.ToData (Pl.DatumType a)
  ) =>
  Optic' k is TxSkel (ConcreteOutput (Pl.TypedValidator a) TxSkelOutDatum Pl.Value (Pl.Versioned Pl.Script)) ->
  -- | Return @Just@ the new validator, or @Nothing@ if you want to leave this
  -- output unchanged.
  (ConcreteOutput (Pl.TypedValidator a) TxSkelOutDatum Pl.Value (Pl.Versioned Pl.Script) -> Maybe (Pl.TypedValidator a)) ->
  -- | The redirection described by the previous argument might apply to more
  -- than one of the script outputs of the transaction. Use this predicate to
  -- select which of the redirectable script outputs to actually redirect. We
  -- count the redirectable script outputs from the left to the right, starting
  -- with zero.
  (Integer -> Bool) ->
  m [ConcreteOutput (Pl.TypedValidator a) TxSkelOutDatum Pl.Value (Pl.Versioned Pl.Script)]
redirectScriptOutputTweak optic change =
  overMaybeSelectingTweak
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
    Show (Pl.DatumType a),
    PrettyCooked (Pl.DatumType a),
    Pl.ToData (Pl.DatumType a),
    Typeable (Pl.DatumType a),
    Typeable a
  ) =>
  -- | Predicate to select outputs to steal, depending on the intended
  -- recipient, the datum, and the value.
  (ConcreteOutput (Pl.TypedValidator a) TxSkelOutDatum Pl.Value (Pl.Versioned Pl.Script) -> Bool) ->
  -- | The selection predicate may match more than one output. Use this
  -- predicate to restrict to the i-th of the outputs (counting from the left,
  -- starting at zero) chosen by the selection predicate with this predicate.
  (Integer -> Bool) ->
  m [ConcreteOutput (Pl.TypedValidator a) TxSkelOutDatum Pl.Value (Pl.Versioned Pl.Script)]
datumHijackingAttack change select = do
  redirected <-
    redirectScriptOutputTweak
      (txSkelOutsL % traversed % txSkelOutOwnerTypeP @(Pl.TypedValidator a))
      (\output -> if change output then Just thief else Nothing)
      select
  guard . not $ null redirected
  addLabelTweak $ DatumHijackingLbl $ Pl.validatorAddress thief
  return redirected
  where
    thief = datumHijackingTarget @a

newtype DatumHijackingLbl = DatumHijackingLbl Pl.Address
  deriving (Show, Eq, Ord)

-- | The trivial validator that always succeds; this is a sufficient target for
-- the datum hijacking attack since we only want to show feasibility of the
-- attack.
datumHijackingTarget :: Pl.TypedValidator a
datumHijackingTarget = unsafeTypedValidatorFromUPLC (Pl.getPlc $$(Pl.compile [||tgt||]))
  where
    tgt :: Pl.BuiltinData -> Pl.BuiltinData -> Pl.BuiltinData -> ()
    tgt _ _ _ = ()
