{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.Output where

import Control.Monad
import qualified Ledger.Ada as Pl
import qualified Ledger.Scripts as Pl hiding (validatorHash)
import qualified Ledger.Typed.Scripts as Pl
import qualified Ledger.Value as Pl
import Optics.Core
import qualified Plutus.V2.Ledger.Api as Pl hiding (TxOut)
import qualified Plutus.V2.Ledger.Tx as Pl

-- For Template Haskell reasons, the most intersting thing in this module (the
-- definition of 'TxSkel') is at the very bottom of this file
--
-- We'll use TH to generate optics for the types in this file, and the naming
-- convention will be that the optic's names will be the corresponding field's
-- names, followed by
--
-- - an 'L' for lenses
--
-- - an 'AT' for affine traversals

-- * 'IsOutput': UTxOs that can be used as transaction outputs

-- | A generalisation of 'Pl.TxOut': With the two type families, we can lift
-- some information about who owns the output (a public key, a script...?) and
-- the datum (do we have an inline datum, a datum hash, nothing...?) to the type
-- level.
class Show o => IsOutput o where
  -- The owner type can be, in particular, a 'TypedValidator a' or a
  -- 'PubkeyHash'
  type OwnerType o
  type DatumType o
  type ValueType o
  outputOwnerL :: Lens' o (OwnerType o)
  outputStakingCredentialL :: Lens' o (Maybe Pl.StakingCredential)
  outputDatumL :: Lens' o (DatumType o)
  outputValueL :: Lens' o (ValueType o)
  outputAddress :: o -> Pl.Address
  outputOutputDatum :: o -> Pl.OutputDatum
  outputValue :: o -> Pl.Value

-- | Return the output as it is seen by a validator. In particular the
-- correctness of this specification will depend on the 'IsOutput' instance, so
-- make sure you get these instances (in particular the functions 'outputAddress',
-- 'outputOutputDatum', and 'outputValue') right!
outputTxOut :: IsOutput o => o -> Pl.TxOut
outputTxOut o =
  Pl.TxOut
    (outputAddress o)
    (outputValue o)
    (outputOutputDatum o)
    Nothing -- TODO for when we introduce reference scripts

-- ** 'Pl.TxOut's are outputs

instance IsOutput Pl.TxOut where
  type OwnerType Pl.TxOut = Pl.Credential
  type DatumType Pl.TxOut = Pl.OutputDatum
  type ValueType Pl.TxOut = Pl.Value
  outputOwnerL =
    lensVL Pl.outAddress
      % lens
        Pl.addressCredential
        (\addr cred -> addr {Pl.addressCredential = cred})
  outputDatumL = lensVL Pl.outDatum
  outputStakingCredentialL =
    lens
      (Pl.addressStakingCredential . Pl.txOutAddress)
      ( \out mStCred ->
          out {Pl.txOutAddress = (Pl.txOutAddress out) {Pl.addressStakingCredential = mStCred}}
      )
  outputValueL = lensVL Pl.outValue
  outputAddress = Pl.txOutAddress
  outputOutputDatum = Pl.txOutDatum
  outputValue = Pl.txOutValue

-- ** A concrete type for outputs

class ToCredential a where
  toCredential :: a -> Pl.Credential

instance ToCredential Pl.Credential where
  toCredential = id

instance ToCredential (Pl.TypedValidator a) where
  toCredential = Pl.ScriptCredential . Pl.validatorHash

instance ToCredential Pl.PubKeyHash where
  toCredential = Pl.PubKeyCredential

class ToOutputDatum a where
  toOutputDatum :: a -> Pl.OutputDatum

instance ToOutputDatum Pl.OutputDatum where
  toOutputDatum = id

instance ToOutputDatum Pl.Datum where
  toOutputDatum = Pl.OutputDatum

instance ToOutputDatum () where
  toOutputDatum = const Pl.NoOutputDatum

instance ToOutputDatum Pl.DatumHash where
  toOutputDatum = Pl.OutputDatumHash

instance Pl.ToData a => ToOutputDatum (ResolvedOrInlineDatum a) where
  toOutputDatum (ResolvedOrInlineDatum x) = Pl.OutputDatum . Pl.Datum . Pl.toBuiltinData $ x

class ToValue a where
  toValue :: a -> Pl.Value

instance ToValue Pl.Value where
  toValue = id

instance ToValue Pl.Ada where
  toValue = Pl.toValue

data ConcreteOutput ownerType datumType valueType where
  ConcreteOutput ::
    { concreteOutputOwner :: ownerType,
      concreteOutputStakingCredential :: Maybe Pl.StakingCredential,
      concreteOutputValue :: valueType,
      concreteOutputDatum :: datumType
      -- concreteOutputReferenceScript :: Maybe Pl.ScriptHash -- TODO for when we introduce reference scripts
    } ->
    ConcreteOutput ownerType datumType valueType

deriving instance (Show ownerType, Show datumType, Show valueType) => Show (ConcreteOutput ownerType datumType valueType)

deriving instance (Eq ownerType, Eq datumType, Eq valueType) => Eq (ConcreteOutput ownerType datumType valueType)

instance
  ( Show ownerType,
    ToCredential ownerType,
    Show datumType,
    ToOutputDatum datumType,
    Show valueType,
    ToValue valueType
  ) =>
  IsOutput (ConcreteOutput ownerType datumType valueType)
  where
  type OwnerType (ConcreteOutput ownerType datumType valueType) = ownerType
  type DatumType (ConcreteOutput ownerType datumType valueType) = datumType
  type ValueType (ConcreteOutput ownerType datumType valueType) = valueType
  outputOwnerL = lens concreteOutputOwner (\out owner -> out {concreteOutputOwner = owner})
  outputStakingCredentialL = lens concreteOutputStakingCredential (\out mStCred -> out {concreteOutputStakingCredential = mStCred})
  outputDatumL = lens concreteOutputDatum (\out datum -> out {concreteOutputDatum = datum})
  outputValueL = lens concreteOutputValue (\out value -> out {concreteOutputValue = value})
  outputAddress out =
    Pl.Address
      (toCredential $ concreteOutputOwner out)
      (concreteOutputStakingCredential out)
  outputOutputDatum = toOutputDatum . concreteOutputDatum
  outputValue = toValue . concreteOutputValue

-- ** A few special concrete outputs

-- | A public key output without a datum
type PKOutput = ConcreteOutput Pl.PubKeyHash () Pl.Value

-- | A public key output that only has Ada and no datum
type PKAdaOnlyOutput = ConcreteOutput Pl.PubKeyHash () Pl.Ada

-- | A public key output where we don't know anything about the datum: It is a
-- general 'Pl.OutputDatum'
type PKOutputMaybeDatum = ConcreteOutput Pl.PubKeyHash Pl.OutputDatum Pl.Value

-- | An output that belongs to a typed validator and has an inline datum of the
-- appropriate type.
type ScriptOutputWithInlineDatum a = ConcreteOutput (Pl.TypedValidator a) (Pl.DatumType a) Pl.Value

-- TODO et cetera

-- ** Functions to translate between different output types

-- | Test if there is no datum on an output. If there is no datum, return an
-- output with the same 'OwnerType', but with @()@ as its 'DatumType'.
isOutputWithoutDatum ::
  IsOutput output =>
  output ->
  Maybe (ConcreteOutput (OwnerType output) () (ValueType output))
isOutputWithoutDatum out = case outputOutputDatum out of
  Pl.NoOutputDatum ->
    Just $
      ConcreteOutput
        (out ^. outputOwnerL)
        (out ^. outputStakingCredentialL)
        (out ^. outputValueL)
        ()
  _ -> Nothing

-- ** Functions to translate between different output types

-- | Test if the output carries some inlined datum (lose the type information
-- about the datum in favour of Plutus' 'Datum' type).
isOutputWithInlineDatumUntyped ::
  IsOutput output =>
  output ->
  Maybe (ConcreteOutput (OwnerType output) Pl.Datum (ValueType output))
isOutputWithInlineDatumUntyped out =
  case outputOutputDatum out of
    Pl.OutputDatum datum ->
      Just $
        ConcreteOutput
          (out ^. outputOwnerL)
          (out ^. outputStakingCredentialL)
          (out ^. outputValueL)
          datum
    _ -> Nothing

-- | Test if the output carries some inlined datum.
isOutputWithInlineDatum ::
  IsOutput output =>
  output ->
  Maybe output
isOutputWithInlineDatum out =
  case outputOutputDatum out of
    Pl.OutputDatum _ -> Just out
    _ -> Nothing

-- | Test if the output carries some datum hash.
isOutputWithDatumHash ::
  IsOutput output =>
  output ->
  Maybe (ConcreteOutput (OwnerType output) Pl.DatumHash (ValueType output))
isOutputWithDatumHash out =
  case outputOutputDatum out of
    Pl.OutputDatumHash hash ->
      Just $
        ConcreteOutput
          (out ^. outputOwnerL)
          (Pl.addressStakingCredential . outputAddress $ out)
          (out ^. outputValueL)
          hash
    _ -> Nothing

-- | Test if the value carried by an output verifies a given predicate.
isOutputWithValueSuchThat ::
  IsOutput output =>
  (ValueType output -> Bool) ->
  output ->
  Maybe output
isOutputWithValueSuchThat predicate out
  | predicate (out ^. outputValueL) = Just out
  | otherwise = Nothing

-- | Test if the datum carried by an output verifies a given predicate.
isOutputWithDatumSuchThat ::
  IsOutput output =>
  (DatumType output -> Bool) ->
  output ->
  Maybe output
isOutputWithDatumSuchThat predicate out
  | predicate (out ^. outputDatumL) = Just out
  | otherwise = Nothing

-- | Wrapper type to make clear that the datum is a resolved or inline datum
newtype ResolvedOrInlineDatum a = ResolvedOrInlineDatum a deriving (Show, Eq)

-- | Test if an output has an inline datum of a certain type. In most
-- applications, it will make sense to use this with 'filteredUtxosWithDatums',
-- and not with 'filteredUtxos'.
isOutputWithDatumOfType ::
  forall a output.
  ( Pl.FromData a,
    IsOutput output
  ) =>
  output ->
  Maybe (ConcreteOutput (OwnerType output) (ResolvedOrInlineDatum a) (ValueType output))
isOutputWithDatumOfType out = case outputOutputDatum out of
  Pl.OutputDatumHash _ -> Nothing
  Pl.OutputDatum (Pl.Datum datum) ->
    ConcreteOutput
      (out ^. outputOwnerL)
      (out ^. outputStakingCredentialL)
      (out ^. outputValueL)
      . ResolvedOrInlineDatum
      <$> Pl.fromBuiltinData datum
  Pl.NoOutputDatum -> Nothing

-- | Test if the owner an output is a specific script. If it is, return an
-- output with the validator type as its 'OwnerType'.
isScriptOutputFrom ::
  IsOutput output =>
  Pl.TypedValidator a ->
  output ->
  Maybe (ConcreteOutput (Pl.TypedValidator a) (DatumType output) (ValueType output))
isScriptOutputFrom validator out =
  case outputAddress out of
    Pl.Address (Pl.ScriptCredential scriptHash) mStCred ->
      if scriptHash == Pl.validatorHash validator
        then
          Just $
            ConcreteOutput
              validator
              mStCred
              (out ^. outputValueL)
              (out ^. outputDatumL)
        else Nothing
    _ -> Nothing

-- | like 'isScriptOutputFrom', but also makes sure that the output has an
-- inline datum of the correct type. In most applications, it will make sense to
-- use this with 'filteredUtxosWithDatums', and not with 'filteredUtxos'.
isScriptOutputFrom' ::
  forall a output.
  ( IsOutput output,
    Pl.FromData (Pl.DatumType a),
    ToOutputDatum (DatumType output),
    Show (DatumType output),
    ToValue (ValueType output),
    Show (ValueType output)
  ) =>
  Pl.TypedValidator a ->
  output ->
  Maybe (ConcreteOutput (Pl.TypedValidator a) (ResolvedOrInlineDatum (Pl.DatumType a)) (ValueType output))
isScriptOutputFrom' validator =
  -- what's all this madness witht the type annotations?
  isScriptOutputFrom @output @a validator
    >=> isOutputWithDatumOfType @(Pl.DatumType a) @(ConcreteOutput (Pl.TypedValidator a) (DatumType output) (ValueType output))

-- | Test if the owner an output is a specific public key. If it is, return an
-- output of the same 'DatumType', but with 'Pl.PubKeyHash' as its 'OwnerType'.
isPKOutputFrom ::
  IsOutput output =>
  Pl.PubKeyHash ->
  output ->
  Maybe (ConcreteOutput Pl.PubKeyHash (DatumType output) (ValueType output))
isPKOutputFrom pkh out = case outputAddress out of
  Pl.Address (Pl.PubKeyCredential pkh') _mStCred ->
    if pkh == pkh'
      then
        Just $
          ConcreteOutput
            pkh
            (out ^. outputStakingCredentialL)
            (out ^. outputValueL)
            (out ^. outputDatumL)
      else Nothing
  _ -> Nothing

-- | Test if the value on an output contains only Ada, and adapt the return type
-- accordingly if it is so.
isOnlyAdaOutput ::
  IsOutput output =>
  output ->
  Maybe (ConcreteOutput (OwnerType output) (DatumType output) Pl.Ada)
isOnlyAdaOutput out =
  if Pl.isAdaOnlyValue (outputValue out)
    then
      Just $
        ConcreteOutput
          (out ^. outputOwnerL)
          (out ^. outputStakingCredentialL)
          (Pl.fromValue $ outputValue out)
          (out ^. outputDatumL)
    else Nothing

-- TODO et cetera
