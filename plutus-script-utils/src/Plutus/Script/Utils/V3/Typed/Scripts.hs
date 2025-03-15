{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Plutus.Script.Utils.V3.Typed.Scripts
  ( module Plutus.Script.Utils.V3.Typed.Scripts.MultiPurpose,
    Validator,
    MintingPolicy,
    StakeValidator,
    TypedScriptTxOut (..),
    TypedScriptTxOutRef (..),
    typeScriptTxOut,
    typeScriptTxOutRef,
    ConnectionError (..),
  )
where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Plutus.Script.Utils.Scripts (MintingPolicy, Script, StakeValidator, Validator, datumHash, toScriptHash, toVersioned)
import Plutus.Script.Utils.V3.Typed.Scripts.MultiPurpose
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V3
  ( Address (..),
    BuiltinData,
    Credential (ScriptCredential),
    Datum,
    DatumHash,
    FromData,
    OutputDatum (OutputDatum, OutputDatumHash),
    ToData (..),
    TxOut (..),
    TxOutRef,
  )
import Prettyprinter (Pretty (pretty), viaShow, (<+>))

data WrongOutTypeError
  = ExpectedScriptGotPubkey
  | ExpectedPubkeyGotScript
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | An error we can get while trying to type an existing transaction part.
data ConnectionError
  = WrongValidatorAddress PV1.ScriptHash PV1.ScriptHash
  | WrongOutType WrongOutTypeError
  | WrongValidatorType String
  | WrongRedeemerType BuiltinData
  | WrongDatumType BuiltinData
  | NoDatum TxOutRef DatumHash
  | WrongDatumHash DatumHash DatumHash
  | UnknownRef TxOutRef
  deriving stock (Show, Eq, Ord, Generic)

instance Pretty ConnectionError where
  pretty (WrongValidatorAddress a1 a2) = "Wrong validator address. Expected:" <+> pretty a1 <+> "Actual:" <+> pretty a2
  pretty (WrongOutType t) = "Wrong out type:" <+> viaShow t
  pretty (WrongValidatorType t) = "Wrong validator type:" <+> pretty t
  pretty (WrongRedeemerType d) = "Wrong redeemer type" <+> pretty (PV1.builtinDataToData d)
  pretty (WrongDatumType d) = "Wrong datum type" <+> pretty (PV1.builtinDataToData d)
  pretty (NoDatum t d) = "No datum with hash " <+> pretty d <+> "for tx output" <+> pretty t
  pretty (WrongDatumHash dh1 dh2) = "Wrong datum hash, " <> "expected " <> pretty dh1 <> ", got " <> pretty dh2
  pretty (UnknownRef d) = "Unknown reference" <+> pretty d

-- | Checks that the given validator hash is consistent with the actual validator.
checkValidatorHash :: (MonadError ConnectionError m) => MultiPurposeScript a -> PV1.ScriptHash -> m ()
checkValidatorHash script expectedHash = do
  let actualHash = toScriptHash $ toVersioned @Script script
  unless (expectedHash == actualHash) $ throwError $ WrongValidatorAddress expectedHash actualHash

-- | Checks that the given datum has the right type.
checkDatum ::
  (PV1.FromData (SpendingDatumType a), MonadError ConnectionError m) =>
  MultiPurposeScript a ->
  Datum ->
  m (SpendingDatumType a)
checkDatum _ (PV1.Datum d) = maybe (throwError $ WrongDatumType d) return $ PV1.fromBuiltinData d

-- | A 'TxOut' tagged by a phantom type: and the connection type of the output.
data TypedScriptTxOut a = (FromData (SpendingDatumType a), ToData (SpendingDatumType a)) =>
  TypedScriptTxOut
  { tyTxOutTxOut :: TxOut,
    tyTxOutData :: SpendingDatumType a
  }

instance (Eq (SpendingDatumType a)) => Eq (TypedScriptTxOut a) where
  l == r =
    tyTxOutTxOut l == tyTxOutTxOut r
      && tyTxOutData l == tyTxOutData r

-- | A 'TxOutRef' tagged by a phantom type: and the connection type of the output.
data TypedScriptTxOutRef a = TypedScriptTxOutRef
  { tyTxOutRefRef :: TxOutRef,
    tyTxOutRefOut :: TypedScriptTxOut a
  }

instance (Eq (SpendingDatumType a)) => Eq (TypedScriptTxOutRef a) where
  l == r =
    tyTxOutRefRef l == tyTxOutRefRef r
      && tyTxOutRefOut l == tyTxOutRefOut r

-- | Create a 'TypedScriptTxOut' from an existing 'TxOut' by checking the types of its parts.
typeScriptTxOut ::
  (FromData (SpendingDatumType out), ToData (SpendingDatumType out), MonadError ConnectionError m) =>
  MultiPurposeScript out ->
  TxOutRef ->
  TxOut ->
  Datum ->
  m (TypedScriptTxOut out)
typeScriptTxOut script txOutRef txOut@TxOut {txOutAddress = Address (ScriptCredential sHash) _, txOutDatum} datum = do
  let mDatumHash = case txOutDatum of
        OutputDatum d -> Just $ datumHash d
        OutputDatumHash hash -> Just hash
        _ -> Nothing
      inputDatumHash = datumHash datum
  case mDatumHash of
    Just dHash | dHash == inputDatumHash -> do
      checkValidatorHash script sHash
      dsVal <- checkDatum script datum
      pure $ TypedScriptTxOut txOut dsVal
    Just dHash -> throwError $ WrongDatumHash inputDatumHash dHash
    Nothing -> throwError $ NoDatum txOutRef inputDatumHash
typeScriptTxOut _ _ _ _ = throwError $ WrongOutType ExpectedScriptGotPubkey

-- | Create a 'TypedScriptTxOut' from an existing 'TxOut' by checking the types of its parts.
typeScriptTxOutRef ::
  ( FromData (SpendingDatumType out),
    ToData (SpendingDatumType out),
    MonadError ConnectionError m
  ) =>
  MultiPurposeScript out ->
  TxOutRef ->
  TxOut ->
  Datum ->
  m (TypedScriptTxOutRef out)
typeScriptTxOutRef tv txOutRef txOut datum = do
  tyOut <- typeScriptTxOut tv txOutRef txOut datum
  pure $ TypedScriptTxOutRef txOutRef tyOut
