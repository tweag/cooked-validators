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
import Optics.Core
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Plutus.Script.Utils.Scripts as Pl hiding (validatorHash)
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Pl
import qualified Plutus.Script.Utils.Value as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import qualified Plutus.V2.Ledger.Tx as Pl

-- | A generalisation of 'Pl.TxOut': With the four type families, we can lift
-- some information about who owns the output (a public key, a script...?) and
-- the datum (do we have an inline datum, a datum hash, nothing...?) to the type
-- level.
class IsAbstractOutput o where
  -- The owner type can be, in particular, a 'TypedValidator a' or a
  -- 'PubkeyHash'
  type OwnerType o
  type DatumType o
  type ValueType o
  type ReferenceScriptType o
  outputOwnerL :: Lens' o (OwnerType o)
  outputStakingCredentialL :: Lens' o (Maybe Pl.StakingCredential)
  outputDatumL :: Lens' o (DatumType o)
  outputValueL :: Lens' o (ValueType o)
  outputReferenceScriptL :: Lens' o (Maybe (ReferenceScriptType o))

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

class ToValue a where
  toValue :: a -> Pl.Value

instance ToValue Pl.Value where
  toValue = id

instance ToValue Pl.Ada where
  toValue = Pl.toValue

class ToScript a where
  toScript :: a -> Pl.Versioned Pl.Script

instance ToScript (Pl.Versioned Pl.Script) where
  toScript = id

instance ToScript (Pl.Versioned Pl.Validator) where
  toScript (Pl.Versioned (Pl.Validator script) version) = Pl.Versioned script version

instance ToScript (Pl.TypedValidator a) where
  toScript = toScript . Pl.vValidatorScript

class ToScriptHash a where
  toScriptHash :: a -> Pl.ScriptHash

instance ToScriptHash Pl.ScriptHash where
  toScriptHash = id

instance ToScriptHash (Pl.Versioned Pl.Script) where
  toScriptHash = Pl.scriptHash

instance ToScriptHash (Pl.Versioned Pl.Validator) where
  toScriptHash = toScriptHash . toScript

instance ToScriptHash Pl.ValidatorHash where
  toScriptHash (Pl.ValidatorHash h) = Pl.ScriptHash h

instance ToScriptHash (Pl.TypedValidator a) where
  toScriptHash = toScriptHash . Pl.validatorHash

-- | An output that can be translated into its script-perspective (as seen on the 'TxInfo') representation
type IsOnchainOutput o =
  ( IsAbstractOutput o,
    ToCredential (OwnerType o),
    ToOutputDatum (DatumType o),
    ToValue (ValueType o),
    ToScriptHash (ReferenceScriptType o)
  )

outputAddress :: (IsAbstractOutput o, ToCredential (OwnerType o)) => o -> Pl.Address
outputAddress out = Pl.Address (toCredential (out ^. outputOwnerL)) (out ^. outputStakingCredentialL)

outputOutputDatum :: (IsAbstractOutput o, ToOutputDatum (DatumType o)) => o -> Pl.OutputDatum
outputOutputDatum = toOutputDatum . (^. outputDatumL)

outputValue :: (IsAbstractOutput o, ToValue (ValueType o)) => o -> Pl.Value
outputValue = toValue . (^. outputValueL)

outputReferenceScript :: (IsAbstractOutput o, ToScriptHash (ReferenceScriptType o)) => o -> Maybe Pl.ScriptHash
outputReferenceScript = (toScriptHash <$>) . (^. outputReferenceScriptL)

-- | Return the output as it is seen by a validator. In particular the
-- correctness of this specification will depend on the 'IsOnchainOutput' instance, so
-- make sure you get these instances (in particular the functions 'outputAddress',
-- 'outputOutputDatum', and 'outputValue') right!
outputTxOut :: IsOnchainOutput o => o -> Pl.TxOut
outputTxOut o =
  Pl.TxOut
    (outputAddress o)
    (outputValue o)
    (outputOutputDatum o)
    (outputReferenceScript o)

-- ** 'Pl.TxOut's are outputs

instance IsAbstractOutput Pl.TxOut where
  type OwnerType Pl.TxOut = Pl.Credential
  type DatumType Pl.TxOut = Pl.OutputDatum
  type ValueType Pl.TxOut = Pl.Value
  type ReferenceScriptType Pl.TxOut = Pl.ScriptHash
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
  outputReferenceScriptL = lensVL Pl.outReferenceScript

-- ** A concrete type for outputs

data ConcreteOutput ownerType datumType valueType referenceScriptType where
  ConcreteOutput ::
    { concreteOutputOwner :: ownerType,
      concreteOutputStakingCredential :: Maybe Pl.StakingCredential,
      concreteOutputValue :: valueType,
      concreteOutputDatum :: datumType,
      concreteOutputReferenceScript :: Maybe referenceScriptType
    } ->
    ConcreteOutput ownerType datumType valueType referenceScriptType

deriving instance (Show ownerType, Show datumType, Show valueType, Show referenceScriptType) => Show (ConcreteOutput ownerType datumType valueType referenceScriptType)

deriving instance (Eq ownerType, Eq datumType, Eq valueType, Eq referenceScriptType) => Eq (ConcreteOutput ownerType datumType valueType referenceScriptType)

instance IsAbstractOutput (ConcreteOutput ownerType datumType valueType referenceScriptType) where
  type OwnerType (ConcreteOutput ownerType datumType valueType referenceScriptType) = ownerType
  type DatumType (ConcreteOutput ownerType datumType valueType referenceScriptType) = datumType
  type ValueType (ConcreteOutput ownerType datumType valueType referenceScriptType) = valueType
  type ReferenceScriptType (ConcreteOutput ownerType datumType valueType referenceScriptType) = referenceScriptType
  outputOwnerL = lens concreteOutputOwner (\out owner -> out {concreteOutputOwner = owner})
  outputStakingCredentialL = lens concreteOutputStakingCredential (\out mStCred -> out {concreteOutputStakingCredential = mStCred})
  outputDatumL = lens concreteOutputDatum (\out datum -> out {concreteOutputDatum = datum})
  outputValueL = lens concreteOutputValue (\out value -> out {concreteOutputValue = value})
  outputReferenceScriptL = lens concreteOutputReferenceScript (\out mRefScript -> out {concreteOutputReferenceScript = mRefScript})

-- ** Functions to translate between different output types

toOutputWithReferenceScriptHash ::
  (IsAbstractOutput output, ToScriptHash (ReferenceScriptType output)) =>
  output ->
  ConcreteOutput (OwnerType output) (DatumType output) (ValueType output) Pl.ScriptHash
toOutputWithReferenceScriptHash out =
  ConcreteOutput
    (out ^. outputOwnerL)
    (out ^. outputStakingCredentialL)
    (out ^. outputValueL)
    (out ^. outputDatumL)
    (toScriptHash <$> out ^. outputReferenceScriptL)

-- | Test if there is no datum on an output. If there is no datum, return an
-- output with the same 'OwnerType', but with @()@ as its 'DatumType'.
isOutputWithoutDatum ::
  IsOnchainOutput output =>
  output ->
  Maybe (ConcreteOutput (OwnerType output) () (ValueType output) (ReferenceScriptType output))
isOutputWithoutDatum out = case outputOutputDatum out of
  Pl.NoOutputDatum ->
    Just $
      ConcreteOutput
        (out ^. outputOwnerL)
        (out ^. outputStakingCredentialL)
        (out ^. outputValueL)
        ()
        (out ^. outputReferenceScriptL)
  _ -> Nothing

-- ** Functions to translate between different output types

-- | Test if the output carries some inlined datum (lose the type information
-- about the datum in favour of Plutus' 'Datum' type).
isOutputWithInlineDatumUntyped ::
  IsOnchainOutput output =>
  output ->
  Maybe (ConcreteOutput (OwnerType output) Pl.Datum (ValueType output) (ReferenceScriptType output))
isOutputWithInlineDatumUntyped out =
  case outputOutputDatum out of
    Pl.OutputDatum datum ->
      Just $
        ConcreteOutput
          (out ^. outputOwnerL)
          (out ^. outputStakingCredentialL)
          (out ^. outputValueL)
          datum
          (out ^. outputReferenceScriptL)
    _ -> Nothing

-- | Test if the output carries some inlined datum.
isOutputWithInlineDatum ::
  IsOnchainOutput output =>
  output ->
  Maybe output
isOutputWithInlineDatum out =
  case outputOutputDatum out of
    Pl.OutputDatum _ -> Just out
    _ -> Nothing

-- | Test if the output carries some datum hash.
isOutputWithDatumHash ::
  IsOnchainOutput output =>
  output ->
  Maybe (ConcreteOutput (OwnerType output) Pl.DatumHash (ValueType output) (ReferenceScriptType output))
isOutputWithDatumHash out =
  case outputOutputDatum out of
    Pl.OutputDatumHash hash ->
      Just $
        ConcreteOutput
          (out ^. outputOwnerL)
          (Pl.addressStakingCredential . outputAddress $ out)
          (out ^. outputValueL)
          hash
          (out ^. outputReferenceScriptL)
    _ -> Nothing

-- | Test if the value carried by an output verifies a given predicate.
isOutputWithValueSuchThat ::
  IsAbstractOutput output =>
  (ValueType output -> Bool) ->
  output ->
  Maybe output
isOutputWithValueSuchThat predicate out
  | predicate (out ^. outputValueL) = Just out
  | otherwise = Nothing

-- | Test if the datum carried by an output verifies a given predicate.
isOutputWithDatumSuchThat ::
  IsAbstractOutput output =>
  (DatumType output -> Bool) ->
  output ->
  Maybe output
isOutputWithDatumSuchThat predicate out
  | predicate (out ^. outputDatumL) = Just out
  | otherwise = Nothing

-- | Test if the owner an output is a specific script. If it is, return an
-- output with the validator type as its 'OwnerType'.
isScriptOutputFrom ::
  IsOnchainOutput output =>
  Pl.TypedValidator a ->
  output ->
  Maybe (ConcreteOutput (Pl.TypedValidator a) (DatumType output) (ValueType output) (ReferenceScriptType output))
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
              (out ^. outputReferenceScriptL)
        else Nothing
    _ -> Nothing

-- | Test if the owner an output is a specific public key. If it is, return an
-- output of the same 'DatumType', but with 'Pl.PubKeyHash' as its 'OwnerType'.
isPKOutputFrom ::
  IsOnchainOutput output =>
  Pl.PubKeyHash ->
  output ->
  Maybe (ConcreteOutput Pl.PubKeyHash (DatumType output) (ValueType output) (ReferenceScriptType output))
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
            (out ^. outputReferenceScriptL)
      else Nothing
  _ -> Nothing

-- | Test if the value on an output contains only Ada, and adapt the return type
-- accordingly if it is so.
isOnlyAdaOutput ::
  IsOnchainOutput output =>
  output ->
  Maybe (ConcreteOutput (OwnerType output) (DatumType output) Pl.Ada (ReferenceScriptType output))
isOnlyAdaOutput out =
  if Pl.isAdaOnlyValue (outputValue out)
    then
      Just $
        ConcreteOutput
          (out ^. outputOwnerL)
          (out ^. outputStakingCredentialL)
          (Pl.fromValue $ outputValue out)
          (out ^. outputDatumL)
          (out ^. outputReferenceScriptL)
    else Nothing
