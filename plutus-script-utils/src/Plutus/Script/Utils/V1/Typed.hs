{-# LANGUAGE UndecidableInstances #-}

module Plutus.Script.Utils.V1.Typed
  ( ValidatorType,
    validatorToTypedValidator,
    alwaysSucceedTypedValidator,
    alwaysFailTypedValidator,
    mkTypedValidator,
    mkTypedValidatorParam,
    ConnectionError (..),
    WrongOutTypeError (..),
    checkValidatorAddress,
    checkRedeemer,
    checkDatum,
    TypedScriptTxOut (..),
    TypedScriptTxOutRef (..),
    makeTypedScriptTxOut,
    typeScriptTxOut,
    typeScriptTxOutRef,
    ValidatorTypes (..),
    TypedValidator
      ( TypedValidator,
        tvValidator,
        tvValidatorHash,
        tvForwardingMintingPolicy,
        tvForwardingMintingPolicyHash,
        tvLanguage
      ),
    generalise,
    Any,
    toCardanoAddressAny,
  )
where

import Cardano.Api.Shelley qualified as C.Api
import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Void (Void)
import GHC.Generics (Generic)
import Plutus.Script.Utils.Address (ToAddress (toAddress), ToCardanoAddress (toCardanoAddress), ToCredential (toCredential))
import Plutus.Script.Utils.Data (datumHash)
import Plutus.Script.Utils.Scripts
  ( Language (PlutusV1),
    MintingPolicy,
    MintingPolicyHash,
    Script,
    ToMintingPolicyHash (toMintingPolicyHash),
    ToScript (toScript),
    ToScriptHash (toScriptHash),
    ToValidator (toValidator),
    ToValidatorHash (toValidatorHash),
    ToVersioned (toVersioned),
    Validator,
    ValidatorHash (ValidatorHash, getValidatorHash),
    Versioned (Versioned),
  )
import Plutus.Script.Utils.V1.Generators
  ( alwaysFailValidator,
    alwaysSucceedValidator,
    mkForwardingMintingPolicy,
  )
import Plutus.Script.Utils.V1.Scripts (UntypedValidator)
import PlutusCore.Default (DefaultUni)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1
  ( Address (Address, addressCredential),
    BuiltinData,
    Credential (PubKeyCredential, ScriptCredential),
    Datum (Datum),
    DatumHash,
    FromData (fromBuiltinData),
    Redeemer (Redeemer),
    ScriptContext,
    ScriptHash (ScriptHash),
    ToData (toBuiltinData),
    TxOut (TxOut),
    TxOutRef,
    Value,
    builtinDataToData,
  )
import PlutusTx (CompiledCode, Lift, liftCode, unsafeApplyCode)
import Prettyprinter (Pretty (pretty), viaShow, (<+>))

data Any
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | A class that associates a type standing for a connection type with two types, the type of the
-- redeemer and the data script for that connection type.
class ValidatorTypes (a :: Type) where
  -- | The type of the redeemers of this connection type.
  type RedeemerType a :: Type

  -- | Default redeemer type is ()
  type RedeemerType a = ()

  -- | The type of the data of this connection type.
  type DatumType a :: Type

  -- | Default datum type is ()
  type DatumType a = ()

instance ValidatorTypes ()

instance ValidatorTypes Void where
  type RedeemerType Void = Void
  type DatumType Void = Void

instance ValidatorTypes Any where
  type RedeemerType Any = BuiltinData
  type DatumType Any = BuiltinData

-- | A validator, its hash, its forwarding minting policy and hash bundled into
-- a structure tagged by a phantom connection type.
data TypedValidator (a :: Type) = TypedValidator
  { -- | The validator script comprised in the typed validator
    tvValidator :: Validator,
    -- | The hash of the validator script
    tvValidatorHash :: ValidatorHash,
    -- | The forwarding minting policy associated with the validator hash, which
    -- ensures the validator is invoked in a transaction.
    tvForwardingMintingPolicy :: MintingPolicy,
    -- | The hash of the forwarding minting policy associated to this validator
    tvForwardingMintingPolicyHash :: MintingPolicyHash,
    -- | The language of the validator and forwarding minting policy
    tvLanguage :: Language
  }
  deriving stock (Show, Eq, Generic)

instance ToScript (TypedValidator a) where
  toScript = toScript . tvValidator

instance ToValidator (TypedValidator a) where
  toValidator = toValidator . tvValidator

instance ToVersioned Script (TypedValidator a) where
  toVersioned = (toScript <$>) . toVersioned @Validator

instance ToVersioned Validator (TypedValidator a) where
  toVersioned (TypedValidator {tvLanguage, tvValidator}) = Versioned tvValidator tvLanguage

instance ToScriptHash (TypedValidator a) where
  toScriptHash = toScriptHash . toVersioned @Script

instance ToValidatorHash (TypedValidator a) where
  toValidatorHash = toValidatorHash . toVersioned @Validator

instance ToCredential (TypedValidator a) where
  toCredential = toCredential . toVersioned @Script

instance ToAddress (TypedValidator a) where
  toAddress = toAddress . toVersioned @Script

instance ToCardanoAddress (TypedValidator a) where
  toCardanoAddress networkId = toCardanoAddress networkId . toVersioned @Script

toCardanoAddressAny :: C.Api.NetworkId -> TypedValidator a -> C.Api.AddressAny
toCardanoAddressAny nid tv =
  case toCardanoAddress nid tv of
    C.Api.AddressInEra C.Api.ShelleyAddressInEra {} addr -> C.Api.AddressShelley addr
    C.Api.AddressInEra C.Api.ByronAddressInAnyEra {} addr -> C.Api.AddressByron addr

-- | Generalise the typed validator to one that works with the 'Data' type.  we
-- can do this safely because the on-chain validators are untyped, so they
-- always take 'BuiltinData' arguments. The validator script stays the same, so
-- the conversion from 'BuiltinData' to 'a' still takes place, even if it's not
-- reflected in the type signature anymore.
generalise :: TypedValidator a -> TypedValidator Any
generalise = coerce

-- | The type of validators for the given connection type.
type ValidatorType (a :: Type) = DatumType a -> RedeemerType a -> ScriptContext -> Bool

validatorToTypedValidator :: Validator -> TypedValidator a
validatorToTypedValidator val =
  TypedValidator val hsh fmp fmph lang
  where
    lang = PlutusV1
    hsh = toValidatorHash (Versioned val lang)
    fmp = mkForwardingMintingPolicy hsh
    fmph = toMintingPolicyHash (Versioned fmp lang)

alwaysSucceedTypedValidator :: TypedValidator a
alwaysSucceedTypedValidator = validatorToTypedValidator alwaysSucceedValidator

alwaysFailTypedValidator :: TypedValidator a
alwaysFailTypedValidator = validatorToTypedValidator alwaysFailValidator

-- | Make a 'TypedValidator' from the 'CompiledCode' of a validator script and its wrapper.
mkTypedValidator ::
  -- | Validator script (compiled)
  CompiledCode (ValidatorType a) ->
  -- | A wrapper for the compiled validator
  CompiledCode (ValidatorType a -> UntypedValidator) ->
  TypedValidator a
mkTypedValidator vc wrapper =
  validatorToTypedValidator $ toValidator $ wrapper `unsafeApplyCode` vc

-- | Make a 'TypedValidator' from the 'CompiledCode' of a parameterized validator script and its wrapper.
mkTypedValidatorParam ::
  forall a param.
  (Lift DefaultUni param) =>
  -- | Validator script (compiled)
  CompiledCode (param -> ValidatorType a) ->
  -- | A wrapper for the compiled validator
  CompiledCode (ValidatorType a -> UntypedValidator) ->
  -- | The extra paramater for the validator script
  param ->
  TypedValidator a
mkTypedValidatorParam vc wrapper param =
  mkTypedValidator (vc `unsafeApplyCode` liftCode plcVersion100 param) wrapper

data WrongOutTypeError
  = ExpectedScriptGotPubkey
  | ExpectedPubkeyGotScript
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | An error we can get while trying to type an existing transaction part.
data ConnectionError
  = WrongValidatorHash ValidatorHash ValidatorHash
  | WrongCredentialType WrongOutTypeError
  | WrongValidatorType String
  | WrongRedeemerType BuiltinData
  | WrongDatumType BuiltinData
  | NoDatum TxOutRef DatumHash
  | UnknownRef TxOutRef
  deriving stock (Show, Eq, Ord, Generic)

instance Pretty ConnectionError where
  pretty (WrongValidatorHash h1 h2) = "Wrong validator address. Expected:" <+> pretty h1 <+> "Actual:" <+> pretty h2
  pretty (WrongCredentialType t) = "Wrong credential type:" <+> viaShow t
  pretty (WrongValidatorType t) = "Wrong validator type:" <+> pretty t
  pretty (WrongRedeemerType d) = "Wrong redeemer type" <+> pretty (builtinDataToData d)
  pretty (WrongDatumType d) = "Wrong datum type" <+> pretty (builtinDataToData d)
  pretty (NoDatum t d) = "No datum with hash " <+> pretty d <+> "for tx output" <+> pretty t
  pretty (UnknownRef d) = "Unknown reference" <+> pretty d

-- | Checks that the given validator hash is consistent with the actual validator.
checkValidatorAddress ::
  (MonadError ConnectionError m) => TypedValidator a -> Address -> m ()
checkValidatorAddress (TypedValidator {tvValidatorHash}) (Address (ScriptCredential (ScriptHash sHash)) _) =
  unless (sHash == getValidatorHash tvValidatorHash) $
    throwError $
      WrongValidatorHash tvValidatorHash (ValidatorHash sHash)
checkValidatorAddress _ _ = throwError $ WrongCredentialType ExpectedScriptGotPubkey

-- | Checks that the given redeemer script has the right type.
checkRedeemer ::
  forall inn m.
  (FromData (RedeemerType inn), MonadError ConnectionError m) =>
  TypedValidator inn ->
  Redeemer ->
  m (RedeemerType inn)
checkRedeemer _ (Redeemer d) =
  case fromBuiltinData d of
    Just v -> pure v
    Nothing -> throwError $ WrongRedeemerType d

-- | Checks that the given datum has the right type.
checkDatum ::
  forall a m.
  (FromData (DatumType a), MonadError ConnectionError m) =>
  TypedValidator a ->
  Datum ->
  m (DatumType a)
checkDatum _ (Datum d) =
  case fromBuiltinData d of
    Just v -> pure v
    Nothing -> throwError $ WrongDatumType d

type IsDataDatum a = (FromData (DatumType a), ToData (DatumType a))

-- | A 'TxOut' tagged by a phantom type: the connection type of the output.
data TypedScriptTxOut a = (IsDataDatum a) =>
  TypedScriptTxOut
  { tyTxOutTxOut :: TxOut,
    tyTxOutData :: DatumType a
  }

deriving instance (Eq (DatumType a)) => Eq (TypedScriptTxOut a)

-- | Create a 'TypedScriptTxOut' from a correctly-typed data script, an address, and a value.
makeTypedScriptTxOut ::
  (IsDataDatum out) =>
  TypedValidator out ->
  DatumType out ->
  Value ->
  TypedScriptTxOut out
makeTypedScriptTxOut (TypedValidator {tvValidatorHash}) dat val =
  TypedScriptTxOut
    ( TxOut
        (Address (ScriptCredential (toScriptHash tvValidatorHash)) Nothing)
        val
        (Just (datumHash $ Datum $ toBuiltinData dat))
    )
    dat

-- | A 'TxOutRef' tagged by a phantom type: the connection type of the output.
data TypedScriptTxOutRef a = TypedScriptTxOutRef
  { tyTxOutRefRef :: TxOutRef,
    tyTxOutRefOut :: TypedScriptTxOut a
  }

deriving instance (Eq (DatumType a)) => Eq (TypedScriptTxOutRef a)

-- | Create a 'TypedScriptTxOut' from an existing 'TxOut' by checking the types of its parts.
typeScriptTxOut ::
  (IsDataDatum out, MonadError ConnectionError m) =>
  TypedValidator out ->
  TxOutRef ->
  TxOut ->
  Datum ->
  m (TypedScriptTxOut out)
typeScriptTxOut tv txOutRef txOut@(TxOut addr _ dHash) datum =
  case addressCredential addr of
    PubKeyCredential _ ->
      throwError $ WrongCredentialType ExpectedScriptGotPubkey
    ScriptCredential _vh ->
      case dHash of
        Just dh
          | datumHash datum == dh ->
              checkValidatorAddress tv addr
                >> TypedScriptTxOut txOut <$> checkDatum tv datum
        _ -> throwError $ NoDatum txOutRef (datumHash datum)

-- | Create a 'TypedScriptTxOut' from an existing 'TxOut' by checking the types of its parts.
typeScriptTxOutRef ::
  (IsDataDatum out, MonadError ConnectionError m) =>
  TypedValidator out ->
  TxOutRef ->
  TxOut ->
  Datum ->
  m (TypedScriptTxOutRef out)
typeScriptTxOutRef tv txOutRef txOut datum =
  TypedScriptTxOutRef txOutRef <$> typeScriptTxOut tv txOutRef txOut datum
