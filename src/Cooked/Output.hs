-- | This module provide abstractions around the notion of outputs. The idea is
-- to use types to carry additional information on which data is carried by
-- various kinds of concrete outputs used in cooked specifically or in Plutus as
-- a whole, such as @TxSkelOut@ or @TxOut@.
module Cooked.Output
  ( IsAbstractOutput,
    OwnerType,
    DatumType,
    ValueType,
    ReferenceScriptType,
    outputOwnerL,
    outputStakingCredentialL,
    outputDatumL,
    outputValueL,
    outputReferenceScriptL,
    ToCredential (..),
    ToOutputDatum (..),
    ToValue (..),
    ToScript (..),
    ToScriptHash (..),
    IsTxInfoOutput,
    outputAddress,
    outputOutputDatum,
    outputValue,
    outputReferenceScriptHash,
    outputTxOut,
    ConcreteOutput (ConcreteOutput),
    toOutputWithReferenceScriptHash,
    isOutputWithoutDatum,
    isOutputWithInlineDatum,
    isOutputWithDatumHash,
    isOutputWithInlineDatumOfType,
    isScriptOutputFrom,
    isPKOutputFrom,
    isOnlyAdaOutput,
  )
where

import Optics.Core
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Typed qualified as Script hiding (validatorHash)
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V2.Tx qualified as Api
import PlutusLedgerApi.V3 qualified as Api

-- | A generalisation of 'Api.TxOut': With the four type families, we can lift
-- some information about
--
-- - who owns the output (a public key, a script...?)
--
-- - what kind of datum is there (do we have an inline datum, a datum hash,
--   nothing...?)
--
-- - what kind of value does the output hold (pure Ada ...?)
--
-- - what information do we have on the reference script (only a hash, a
--   complete script, a typed validator...?)
--
-- to the type level.
class IsAbstractOutput o where
  type OwnerType o
  type DatumType o
  type ValueType o
  type ReferenceScriptType o
  outputOwnerL :: Lens' o (OwnerType o)
  outputStakingCredentialL :: Lens' o (Maybe Api.StakingCredential)
  outputDatumL :: Lens' o (DatumType o)
  outputValueL :: Lens' o (ValueType o)
  outputReferenceScriptL :: Lens' o (Maybe (ReferenceScriptType o))

class ToCredential a where
  toCredential :: a -> Api.Credential

instance ToCredential Api.Credential where
  toCredential = id

instance ToCredential (Script.TypedValidator a) where
  toCredential = Api.ScriptCredential . toScriptHash . Script.tvValidatorHash

instance ToCredential Api.PubKeyHash where
  toCredential = Api.PubKeyCredential

class ToOutputDatum a where
  toOutputDatum :: a -> Api.OutputDatum

instance ToOutputDatum Api.OutputDatum where
  toOutputDatum = id

instance ToOutputDatum Api.Datum where
  toOutputDatum = Api.OutputDatum

instance ToOutputDatum () where
  toOutputDatum = const Api.NoOutputDatum

instance ToOutputDatum Api.DatumHash where
  toOutputDatum = Api.OutputDatumHash

instance ToOutputDatum Api.BuiltinData where
  toOutputDatum = toOutputDatum . Api.Datum

class ToValue a where
  toValue :: a -> Api.Value

instance ToValue Api.Value where
  toValue = id

instance ToValue Script.Ada where
  toValue = Script.toValue

class ToScript a where
  toScript :: a -> Script.Versioned Script.Script

instance ToScript (Script.Versioned Script.Script) where
  toScript = id

instance ToScript (Script.Versioned Script.Validator) where
  toScript (Script.Versioned (Script.Validator script) version) = Script.Versioned script version

instance ToScript (Script.TypedValidator a) where
  toScript = toScript . Script.vValidatorScript

class ToScriptHash a where
  toScriptHash :: a -> Api.ScriptHash

instance ToScriptHash Api.ScriptHash where
  toScriptHash = id

instance ToScriptHash (Script.Versioned Script.Script) where
  toScriptHash = Script.scriptHash

instance ToScriptHash (Script.Versioned Script.Validator) where
  toScriptHash = toScriptHash . toScript

instance ToScriptHash Script.ValidatorHash where
  toScriptHash (Script.ValidatorHash h) = Script.ScriptHash h

instance ToScriptHash (Script.TypedValidator a) where
  toScriptHash = toScriptHash . Script.tvValidator

-- | An output that can be translated into its script-perspective (as seen on
-- the 'TxInfo') representation
type IsTxInfoOutput o =
  ( IsAbstractOutput o,
    ToCredential (OwnerType o),
    ToOutputDatum (DatumType o),
    ToValue (ValueType o),
    ToScriptHash (ReferenceScriptType o)
  )

outputAddress :: (IsAbstractOutput o, ToCredential (OwnerType o)) => o -> Api.Address
outputAddress out = Api.Address (toCredential (out ^. outputOwnerL)) (out ^. outputStakingCredentialL)

outputOutputDatum :: (IsAbstractOutput o, ToOutputDatum (DatumType o)) => o -> Api.OutputDatum
outputOutputDatum = toOutputDatum . (^. outputDatumL)

outputValue :: (IsAbstractOutput o, ToValue (ValueType o)) => o -> Api.Value
outputValue = toValue . (^. outputValueL)

outputReferenceScriptHash :: (IsAbstractOutput o, ToScriptHash (ReferenceScriptType o)) => o -> Maybe Api.ScriptHash
outputReferenceScriptHash = (toScriptHash <$>) . (^. outputReferenceScriptL)

-- | Return the output as it is seen by a validator on the 'TxInfo'.
outputTxOut :: (IsTxInfoOutput o) => o -> Api.TxOut
outputTxOut o =
  Api.TxOut
    (outputAddress o)
    (outputValue o)
    (outputOutputDatum o)
    (outputReferenceScriptHash o)

-- * 'Api.TxOut's are outputs

instance IsAbstractOutput Api.TxOut where
  type OwnerType Api.TxOut = Api.Credential
  type DatumType Api.TxOut = Api.OutputDatum
  type ValueType Api.TxOut = Api.Value
  type ReferenceScriptType Api.TxOut = Api.ScriptHash
  outputOwnerL =
    lensVL Api.outAddress
      % lens
        Api.addressCredential
        (\addr cred -> addr {Api.addressCredential = cred})
  outputDatumL = lensVL Api.outDatum
  outputStakingCredentialL =
    lens
      (Api.addressStakingCredential . Api.txOutAddress)
      ( \out mStCred ->
          out {Api.txOutAddress = (Api.txOutAddress out) {Api.addressStakingCredential = mStCred}}
      )
  outputValueL = lensVL Api.outValue
  outputReferenceScriptL = lensVL Api.outReferenceScript

-- * A concrete type for outputs

-- | A type constructed to be the most general instance of 'IsAbstractOutput'.
data ConcreteOutput ownerType datumType valueType referenceScriptType where
  ConcreteOutput ::
    { concreteOutputOwner :: ownerType,
      concreteOutputStakingCredential :: Maybe Api.StakingCredential,
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

-- * Functions to translate between different output types

-- ** Filtering on the datum

-- | Test if there is no datum on an output.
isOutputWithoutDatum ::
  (IsTxInfoOutput output) =>
  output ->
  Maybe (ConcreteOutput (OwnerType output) () (ValueType output) (ReferenceScriptType output))
isOutputWithoutDatum out = case outputOutputDatum out of
  Api.NoOutputDatum ->
    Just $
      ConcreteOutput
        (out ^. outputOwnerL)
        (out ^. outputStakingCredentialL)
        (out ^. outputValueL)
        ()
        (out ^. outputReferenceScriptL)
  _ -> Nothing

-- | Test if the output carries some inlined datum that can be parsed from
-- builtin data on to something of a specific type.
isOutputWithInlineDatumOfType ::
  (Api.FromData a, IsTxInfoOutput output) =>
  output ->
  Maybe (ConcreteOutput (OwnerType output) a (ValueType output) (ReferenceScriptType output))
isOutputWithInlineDatumOfType out =
  case outputOutputDatum out of
    Api.OutputDatum (Api.Datum datum) ->
      ConcreteOutput
        (out ^. outputOwnerL)
        (out ^. outputStakingCredentialL)
        (out ^. outputValueL)
        <$> Api.fromBuiltinData datum
        <*> Just (out ^. outputReferenceScriptL)
    _ -> Nothing

-- | Test if the output carries some inlined datum.
isOutputWithInlineDatum ::
  (IsTxInfoOutput output) =>
  output ->
  Maybe (ConcreteOutput (OwnerType output) Api.Datum (ValueType output) (ReferenceScriptType output))
isOutputWithInlineDatum out =
  case outputOutputDatum out of
    Api.OutputDatum datum@(Api.Datum _) ->
      Just $
        ConcreteOutput
          (out ^. outputOwnerL)
          (out ^. outputStakingCredentialL)
          (out ^. outputValueL)
          datum
          (out ^. outputReferenceScriptL)
    _ -> Nothing

-- | Test if the output carries some datum hash.
isOutputWithDatumHash ::
  (IsTxInfoOutput output) =>
  output ->
  Maybe (ConcreteOutput (OwnerType output) Api.DatumHash (ValueType output) (ReferenceScriptType output))
isOutputWithDatumHash out =
  case outputOutputDatum out of
    Api.OutputDatumHash hash ->
      Just $
        ConcreteOutput
          (out ^. outputOwnerL)
          (Api.addressStakingCredential . outputAddress $ out)
          (out ^. outputValueL)
          hash
          (out ^. outputReferenceScriptL)
    _ -> Nothing

-- ** Filtering on the owner

-- | Test if the owner of an output is a specific typed validator. If it is,
-- return an output with the validator type as its 'OwnerType'.
isScriptOutputFrom ::
  (IsTxInfoOutput output) =>
  Script.TypedValidator a ->
  output ->
  Maybe (ConcreteOutput (Script.TypedValidator a) (DatumType output) (ValueType output) (ReferenceScriptType output))
isScriptOutputFrom validator out =
  case outputAddress out of
    Api.Address (Api.ScriptCredential scriptHash) mStCred ->
      if scriptHash == toScriptHash validator
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

-- | Test if the owner of an output is a specific public key. If it is, return
-- an output of the same 'DatumType', but with 'Api.PubKeyHash' as its
-- 'OwnerType'.
isPKOutputFrom ::
  (IsTxInfoOutput output) =>
  Api.PubKeyHash ->
  output ->
  Maybe (ConcreteOutput Api.PubKeyHash (DatumType output) (ValueType output) (ReferenceScriptType output))
isPKOutputFrom pkh out = case outputAddress out of
  Api.Address (Api.PubKeyCredential pkh') _mStCred ->
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

-- ** Filtering on the value

-- | Test if the value on an output contains only Ada.
isOnlyAdaOutput ::
  (IsTxInfoOutput output) =>
  output ->
  Maybe (ConcreteOutput (OwnerType output) (DatumType output) Script.Ada (ReferenceScriptType output))
isOnlyAdaOutput out =
  if Script.isAdaOnlyValue (outputValue out)
    then
      Just $
        ConcreteOutput
          (out ^. outputOwnerL)
          (out ^. outputStakingCredentialL)
          (Script.fromValue $ outputValue out)
          (out ^. outputDatumL)
          (out ^. outputReferenceScriptL)
    else Nothing

-- ** Filtering on the reference script

-- | Convert the reference script type on the output to 'Api.ScriptHash'.
toOutputWithReferenceScriptHash ::
  (IsAbstractOutput output, ToScriptHash (ReferenceScriptType output)) =>
  output ->
  ConcreteOutput (OwnerType output) (DatumType output) (ValueType output) Api.ScriptHash
toOutputWithReferenceScriptHash out =
  ConcreteOutput
    (out ^. outputOwnerL)
    (out ^. outputStakingCredentialL)
    (out ^. outputValueL)
    (out ^. outputDatumL)
    (toScriptHash <$> out ^. outputReferenceScriptL)
