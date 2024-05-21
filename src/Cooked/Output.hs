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
    fromAbstractOutput,
  )
where

import Cooked.Classes
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
outputTxOut o = Api.TxOut (outputAddress o) (outputValue o) (outputOutputDatum o) (outputReferenceScriptHash o)

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
      concreteOutputDatum :: datumType,
      concreteOutputValue :: valueType,
      concreteOutputReferenceScript :: Maybe referenceScriptType
    } ->
    ConcreteOutput ownerType datumType valueType referenceScriptType

deriving instance (Show ownerType, Show datumType, Show valueType, Show referenceScriptType) => Show (ConcreteOutput ownerType datumType valueType referenceScriptType)

deriving instance (Eq ownerType, Eq datumType, Eq valueType, Eq referenceScriptType) => Eq (ConcreteOutput ownerType datumType valueType referenceScriptType)

instance IsAbstractOutput (ConcreteOutput ownerType datumType valueType referenceScriptType) where
  type OwnerType (ConcreteOutput ownerType _ _ _) = ownerType
  type DatumType (ConcreteOutput _ datumType _ _) = datumType
  type ValueType (ConcreteOutput _ _ valueType _) = valueType
  type ReferenceScriptType (ConcreteOutput _ _ _ referenceScriptType) = referenceScriptType
  outputOwnerL = lens concreteOutputOwner (\out owner -> out {concreteOutputOwner = owner})
  outputStakingCredentialL = lens concreteOutputStakingCredential (\out mStCred -> out {concreteOutputStakingCredential = mStCred})
  outputDatumL = lens concreteOutputDatum (\out datum -> out {concreteOutputDatum = datum})
  outputValueL = lens concreteOutputValue (\out value -> out {concreteOutputValue = value})
  outputReferenceScriptL = lens concreteOutputReferenceScript (\out mRefScript -> out {concreteOutputReferenceScript = mRefScript})

-- | Creates a generic concrete instance of any kind of abstract output
fromAbstractOutput :: (IsAbstractOutput out) => out -> ConcreteOutput (OwnerType out) (DatumType out) (ValueType out) (ReferenceScriptType out)
fromAbstractOutput o = ConcreteOutput (o ^. outputOwnerL) (o ^. outputStakingCredentialL) (o ^. outputDatumL) (o ^. outputValueL) (o ^. outputReferenceScriptL)

-- * Functions to translate between different output types

-- ** Filtering on the datum

-- | Test if there is no datum on an output.
isOutputWithoutDatum :: (IsTxInfoOutput out) => out -> Maybe (ConcreteOutput (OwnerType out) () (ValueType out) (ReferenceScriptType out))
isOutputWithoutDatum out | Api.NoOutputDatum <- outputOutputDatum out = Just $ (fromAbstractOutput out) {concreteOutputDatum = ()}
isOutputWithoutDatum _ = Nothing

-- | Test if the output carries some inlined datum.
isOutputWithInlineDatum :: (IsTxInfoOutput out) => out -> Maybe (ConcreteOutput (OwnerType out) Api.Datum (ValueType out) (ReferenceScriptType out))
isOutputWithInlineDatum out | Api.OutputDatum datum@(Api.Datum _) <- outputOutputDatum out = Just $ (fromAbstractOutput out) {concreteOutputDatum = datum}
isOutputWithInlineDatum _ = Nothing

-- | Test if the output carries some inlined datum that can be parsed from
-- builtin data on to something of a specific type.
isOutputWithInlineDatumOfType :: (Api.FromData a, IsTxInfoOutput out) => out -> Maybe (ConcreteOutput (OwnerType out) a (ValueType out) (ReferenceScriptType out))
isOutputWithInlineDatumOfType out | Api.OutputDatum (Api.Datum datum) <- outputOutputDatum out = (\x -> (fromAbstractOutput out) {concreteOutputDatum = x}) <$> Api.fromBuiltinData datum
isOutputWithInlineDatumOfType _ = Nothing

-- | Test if the output carries some datum hash.
isOutputWithDatumHash :: (IsTxInfoOutput out) => out -> Maybe (ConcreteOutput (OwnerType out) Api.DatumHash (ValueType out) (ReferenceScriptType out))
isOutputWithDatumHash out | Api.OutputDatumHash hash <- outputOutputDatum out = Just $ (fromAbstractOutput out) {concreteOutputDatum = hash}
isOutputWithDatumHash _ = Nothing

-- ** Filtering on the owner

-- | Test if the owner of an output is a specific typed validator. If it is,
-- return an output with the validator type as its 'OwnerType'.
isScriptOutputFrom :: (IsTxInfoOutput out) => Script.TypedValidator a -> out -> Maybe (ConcreteOutput (Script.TypedValidator a) (DatumType out) (ValueType out) (ReferenceScriptType out))
isScriptOutputFrom validator out | Api.Address (Api.ScriptCredential scriptHash) _ <- outputAddress out, scriptHash == toScriptHash validator = Just $ (fromAbstractOutput out) {concreteOutputOwner = validator}
isScriptOutputFrom _ _ = Nothing

-- | Test if the owner of an output is a specific public key. If it is, return
-- an output of the same 'DatumType', but with 'Api.PubKeyHash' as its
-- 'OwnerType'.
isPKOutputFrom :: (IsTxInfoOutput out) => Api.PubKeyHash -> out -> Maybe (ConcreteOutput Api.PubKeyHash (DatumType out) (ValueType out) (ReferenceScriptType out))
isPKOutputFrom pkh out | Api.Address (Api.PubKeyCredential pkh') _ <- outputAddress out, pkh == pkh' = Just $ (fromAbstractOutput out) {concreteOutputOwner = pkh}
isPKOutputFrom _ _ = Nothing

-- ** Filtering on the value

-- | Test if the value on an output contains only Ada.
isOnlyAdaOutput :: (IsTxInfoOutput out) => out -> Maybe (ConcreteOutput (OwnerType out) (DatumType out) Script.Ada (ReferenceScriptType out))
isOnlyAdaOutput out | Script.isAdaOnlyValue (outputValue out) = Just $ (fromAbstractOutput out) {concreteOutputValue = Script.fromValue $ outputValue out}
isOnlyAdaOutput _ = Nothing

-- ** Filtering on the reference script

-- | Convert the reference script type on the output to 'Api.ScriptHash'.
toOutputWithReferenceScriptHash :: (IsTxInfoOutput out) => out -> ConcreteOutput (OwnerType out) (DatumType out) (ValueType out) Api.ScriptHash
toOutputWithReferenceScriptHash out = (fromAbstractOutput out) {concreteOutputReferenceScript = toScriptHash <$> out ^. outputReferenceScriptL}
