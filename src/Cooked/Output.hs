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
    IsTxInfoOutput,
    outputAddress,
    outputOutputDatum,
    outputValue,
    outputReferenceScriptHash,
    outputTxOut,
    ConcreteOutput (..),
    toOutputWithReferenceScriptHash,
    isOutputWithoutDatum,
    isOutputWithInlineDatum,
    isOutputWithDatumHash,
    isOutputWithInlineDatumOfType,
    isScriptOutputFrom,
    isPKOutputFrom,
    isOnlyAdaOutput,
    fromAbstractOutput,
    isReferenceScriptOutputFrom,
    isStakingCredentialOutputFrom,
    isEmptyStakingCredentialOutput,
    isPKOutput,
    isScriptOutput,
    setDatum,
    setOwner,
    setReferenceScript,
    setValue,
    setStakingCredential,
  )
where

import Cooked.Conversion.ToCredential
import Cooked.Conversion.ToOutputDatum
import Cooked.Conversion.ToValue
import Optics.Core
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
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
    Script.ToScriptHash (ReferenceScriptType o)
  )

outputAddress :: (IsAbstractOutput o, ToCredential (OwnerType o)) => o -> Api.Address
outputAddress out = Api.Address (toCredential (out ^. outputOwnerL)) (out ^. outputStakingCredentialL)

outputOutputDatum :: (IsAbstractOutput o, ToOutputDatum (DatumType o)) => o -> Api.OutputDatum
outputOutputDatum = toOutputDatum . (^. outputDatumL)

outputValue :: (IsAbstractOutput o, ToValue (ValueType o)) => o -> Api.Value
outputValue = toValue . (^. outputValueL)

outputReferenceScriptHash :: (IsAbstractOutput o, Script.ToScriptHash (ReferenceScriptType o)) => o -> Maybe Api.ScriptHash
outputReferenceScriptHash = (Script.toScriptHash <$>) . (^. outputReferenceScriptL)

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

-- * Setters for outputs

setDatum :: (IsAbstractOutput out) => out -> dat -> ConcreteOutput (OwnerType out) dat (ValueType out) (ReferenceScriptType out)
setDatum o dat = (fromAbstractOutput o) {concreteOutputDatum = dat}

setOwner :: (IsAbstractOutput out) => out -> owner -> ConcreteOutput owner (DatumType out) (ValueType out) (ReferenceScriptType out)
setOwner o own = (fromAbstractOutput o) {concreteOutputOwner = own}

setStakingCredential :: (IsAbstractOutput out) => out -> Api.StakingCredential -> ConcreteOutput (OwnerType out) (DatumType out) (ValueType out) (ReferenceScriptType out)
setStakingCredential o cred = (fromAbstractOutput o) {concreteOutputStakingCredential = Just cred}

setValue :: (IsAbstractOutput out) => out -> val -> ConcreteOutput (OwnerType out) (DatumType out) val (ReferenceScriptType out)
setValue o val = (fromAbstractOutput o) {concreteOutputValue = val}

setReferenceScript :: (IsAbstractOutput out) => out -> ref -> ConcreteOutput (OwnerType out) (DatumType out) (ValueType out) ref
setReferenceScript o refScript = (fromAbstractOutput o) {concreteOutputReferenceScript = Just refScript}

-- * Functions to translate between different output types

-- ** Filtering on the datum

-- | Test if there is no datum on an output.
isOutputWithoutDatum :: (IsTxInfoOutput out) => out -> Maybe (ConcreteOutput (OwnerType out) () (ValueType out) (ReferenceScriptType out))
isOutputWithoutDatum out | Api.NoOutputDatum <- outputOutputDatum out = Just $ setDatum out ()
isOutputWithoutDatum _ = Nothing

-- | Test if the output carries some inlined datum.
isOutputWithInlineDatum :: (IsTxInfoOutput out) => out -> Maybe (ConcreteOutput (OwnerType out) Api.Datum (ValueType out) (ReferenceScriptType out))
isOutputWithInlineDatum out | Api.OutputDatum datum@(Api.Datum _) <- outputOutputDatum out = Just $ setDatum out datum
isOutputWithInlineDatum _ = Nothing

-- | Test if the output carries some inlined datum that can be parsed from
-- builtin data on to something of a specific type.
isOutputWithInlineDatumOfType :: (Api.FromData a, IsTxInfoOutput out) => out -> Maybe (ConcreteOutput (OwnerType out) a (ValueType out) (ReferenceScriptType out))
isOutputWithInlineDatumOfType out | Api.OutputDatum (Api.Datum datum) <- outputOutputDatum out = setDatum out <$> Api.fromBuiltinData datum
isOutputWithInlineDatumOfType _ = Nothing

-- | Test if the output carries some datum hash.
isOutputWithDatumHash :: (IsTxInfoOutput out) => out -> Maybe (ConcreteOutput (OwnerType out) Api.DatumHash (ValueType out) (ReferenceScriptType out))
isOutputWithDatumHash out | Api.OutputDatumHash hash <- outputOutputDatum out = Just $ setDatum out hash
isOutputWithDatumHash _ = Nothing

-- ** Filtering on the owner

-- | Test if the owner of an output is a script
isScriptOutput :: (IsTxInfoOutput out) => out -> Maybe (ConcreteOutput Api.ScriptHash (DatumType out) (ValueType out) (ReferenceScriptType out))
isScriptOutput out | Api.Address (Api.ScriptCredential scriptHash) _ <- outputAddress out = Just $ setOwner out scriptHash
isScriptOutput _ = Nothing

-- | Test if the owner of an output is a specific script
isScriptOutputFrom :: (IsTxInfoOutput out, Script.ToScriptHash s) => s -> out -> Maybe (ConcreteOutput s (DatumType out) (ValueType out) (ReferenceScriptType out))
isScriptOutputFrom validator out = do
  x <- isScriptOutput out
  if Script.toScriptHash validator == x ^. outputOwnerL
    then Just $ setOwner x validator
    else Nothing

-- Test if the owner of an output is a public key
isPKOutput :: (IsTxInfoOutput out) => out -> Maybe (ConcreteOutput Api.PubKeyHash (DatumType out) (ValueType out) (ReferenceScriptType out))
isPKOutput out | Api.Address (Api.PubKeyCredential pkh) _ <- outputAddress out = Just $ setOwner out pkh
isPKOutput _ = Nothing

-- | Test if the owner of an output is a specific public key
isPKOutputFrom :: (IsTxInfoOutput out) => Api.PubKeyHash -> out -> Maybe (ConcreteOutput Api.PubKeyHash (DatumType out) (ValueType out) (ReferenceScriptType out))
isPKOutputFrom pkh out = do
  x <- isPKOutput out
  if pkh == x ^. outputOwnerL
    then Just x
    else Nothing

-- ** Filtering on the staking credential

-- | Test if the given output possesses a certain staking credential
isStakingCredentialOutputFrom :: (IsTxInfoOutput out, ToCredential cred) => cred -> out -> Maybe (ConcreteOutput (OwnerType out) (DatumType out) (ValueType out) (ReferenceScriptType out))
isStakingCredentialOutputFrom cred out | Just (Api.StakingHash cred') <- out ^. outputStakingCredentialL, toCredential cred == cred' = Just $ fromAbstractOutput out
isStakingCredentialOutputFrom _ _ = Nothing

-- | Test if the give output does not possess any staking credential
isEmptyStakingCredentialOutput :: (IsTxInfoOutput out) => out -> Maybe (ConcreteOutput (OwnerType out) (DatumType out) (ValueType out) (ReferenceScriptType out))
isEmptyStakingCredentialOutput out | Nothing <- out ^. outputStakingCredentialL = Just $ fromAbstractOutput out
isEmptyStakingCredentialOutput _ = Nothing

-- ** Filtering on the value

-- | Test if the value on an output contains only Ada.
isOnlyAdaOutput :: (IsTxInfoOutput out) => out -> Maybe (ConcreteOutput (OwnerType out) (DatumType out) Script.Ada (ReferenceScriptType out))
isOnlyAdaOutput out | Script.isAdaOnlyValue (outputValue out) = Just $ setValue out $ Script.fromValue $ outputValue out
isOnlyAdaOutput _ = Nothing

-- ** Filtering on the reference script

-- | Convert the reference script type on the output to 'Api.ScriptHash'.
toOutputWithReferenceScriptHash :: (IsTxInfoOutput out) => out -> ConcreteOutput (OwnerType out) (DatumType out) (ValueType out) Api.ScriptHash
toOutputWithReferenceScriptHash out = (fromAbstractOutput out) {concreteOutputReferenceScript = Script.toScriptHash <$> out ^. outputReferenceScriptL}

-- | Test if the reference script in an output is a specific script
isReferenceScriptOutputFrom :: (IsTxInfoOutput out, Script.ToScriptHash s) => s -> out -> Maybe (ConcreteOutput (OwnerType out) (DatumType out) (ValueType out) Api.ScriptHash)
isReferenceScriptOutputFrom script out | Just x <- out ^. outputReferenceScriptL, Script.toScriptHash x == Script.toScriptHash script = Just $ toOutputWithReferenceScriptHash out
isReferenceScriptOutputFrom _ _ = Nothing
