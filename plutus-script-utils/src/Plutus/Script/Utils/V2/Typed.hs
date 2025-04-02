{-# LANGUAGE UndecidableInstances #-}

module Plutus.Script.Utils.V2.Typed
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
  )
where

import Control.Monad (unless)
import Control.Monad.Except (MonadError, throwError)
import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Plutus.Script.Utils.Address (toAddress)
import Plutus.Script.Utils.Data (datumHash)
import Plutus.Script.Utils.Scripts
  ( Language (PlutusV2),
    Validator,
    Versioned (Versioned),
    toMintingPolicyHash,
    toValidator,
    toValidatorHash,
  )
import Plutus.Script.Utils.Typed
  ( TypedValidator (TypedValidator),
    UntypedValidator,
    ValidatorTypes (DatumType, RedeemerType),
  )
import Plutus.Script.Utils.V2.Generators (alwaysFailValidator, alwaysSucceedValidator, mkForwardingMintingPolicy)
import Plutus.Script.Utils.V2.Scripts ()
import PlutusCore.Core (plcVersion100)
import PlutusCore.Default (DefaultUni)
import PlutusLedgerApi.V2
  ( Address (addressCredential),
    BuiltinData,
    Credential (PubKeyCredential, ScriptCredential),
    Datum (Datum),
    DatumHash,
    FromData,
    OutputDatum (OutputDatum, OutputDatumHash),
    Redeemer (Redeemer),
    ScriptContext,
    ToData (toBuiltinData),
    TxOut (TxOut),
    TxOutRef,
    Value,
    builtinDataToData,
  )
import PlutusTx (CompiledCode, FromData (fromBuiltinData), Lift, liftCode, unsafeApplyCode)
import Prettyprinter (Pretty (pretty), viaShow, (<+>))

-- | The type of validators for the given connection type.
type ValidatorType (a :: Type) = DatumType a -> RedeemerType a -> ScriptContext -> Bool

validatorToTypedValidator :: Validator -> TypedValidator a
validatorToTypedValidator val =
  TypedValidator
    (Versioned val PlutusV2)
    hsh
    (Versioned mps PlutusV2)
    (toMintingPolicyHash mps)
  where
    hsh = toValidatorHash val
    mps = mkForwardingMintingPolicy hsh

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
  = WrongValidatorAddress Address Address
  | WrongOutType WrongOutTypeError
  | WrongValidatorType String
  | WrongRedeemerType BuiltinData
  | WrongDatumType BuiltinData
  | NoDatum TxOutRef DatumHash
  | UnknownRef TxOutRef
  deriving stock (Show, Eq, Ord, Generic)

instance Pretty ConnectionError where
  pretty (WrongValidatorAddress a1 a2) = "Wrong validator address. Expected:" <+> pretty a1 <+> "Actual:" <+> pretty a2
  pretty (WrongOutType t) = "Wrong out type:" <+> viaShow t
  pretty (WrongValidatorType t) = "Wrong validator type:" <+> pretty t
  pretty (WrongRedeemerType d) = "Wrong redeemer type" <+> pretty (builtinDataToData d)
  pretty (WrongDatumType d) = "Wrong datum type" <+> pretty (builtinDataToData d)
  pretty (NoDatum t d) = "No datum with hash " <+> pretty d <+> "for tx output" <+> pretty t
  pretty (UnknownRef d) = "Unknown reference" <+> pretty d

-- | Checks that the given validator hash is consistent with the actual validator.
checkValidatorAddress ::
  forall a m. (MonadError ConnectionError m) => TypedValidator a -> Address -> m ()
checkValidatorAddress ct actualAddr = do
  let expectedAddr = toAddress ct
  unless (expectedAddr == actualAddr) $ throwError $ WrongValidatorAddress expectedAddr actualAddr

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

-- | A 'TxOut' tagged by a phantom type: and the connection type of the output.
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
makeTypedScriptTxOut tv dat val =
  TypedScriptTxOut
    ( TxOut
        (toAddress tv)
        val
        (OutputDatumHash $ datumHash $ Datum $ toBuiltinData dat)
        Nothing
    )
    dat

-- | A 'TxOutRef' tagged by a phantom type: and the connection type of the output.
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
typeScriptTxOut tv txOutRef txOut@(TxOut addr _ dat _) datum =
  case addressCredential addr of
    PubKeyCredential _ ->
      throwError $ WrongOutType ExpectedScriptGotPubkey
    ScriptCredential _vh ->
      case dat of
        OutputDatum d
          | datumHash datum == datumHash d ->
              checkValidatorAddress tv addr
                >> TypedScriptTxOut txOut <$> checkDatum tv datum
        OutputDatumHash dh
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
