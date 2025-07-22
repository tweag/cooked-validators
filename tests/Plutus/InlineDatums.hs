{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.InlineDatums where

import Plutus.Script.Utils.V3 qualified as Script
import PlutusCore.Version
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx
import PlutusTx.AssocMap qualified as Map
import PlutusTx.List
import PlutusTx.Prelude
import Prelude qualified as HS

data SimpleContractDatum = FirstPaymentDatum | SecondPaymentDatum deriving (HS.Show, HS.Eq)

instance Eq SimpleContractDatum where
  FirstPaymentDatum == FirstPaymentDatum = True
  SecondPaymentDatum == SecondPaymentDatum = True
  _ == _ = False

unstableMakeIsData ''SimpleContractDatum

data SimpleContract

instance Script.MultiPurposeScriptTypes SimpleContract where
  type SpendingDatumType SimpleContract = SimpleContractDatum

{-# INLINEABLE inputDatumSpendingPurpose #-}
inputDatumSpendingPurpose :: Bool -> Script.SpendingPurposeType SimpleContract
inputDatumSpendingPurpose requireInlineDatum oRef _ _ Api.TxInfo {txInfoInputs} =
  case find ((oRef ==) . Api.txInInfoOutRef) txInfoInputs of
    Just (Api.TxInInfo _ Api.TxOut {Api.txOutDatum = inDatum}) | requireInlineDatum -> case inDatum of
      Api.OutputDatum _ -> True
      Api.OutputDatumHash _ -> trace "I want an inline datum, but I got a hash" False
      Api.NoOutputDatum -> trace "I want an inline datum, but I got neither a datum nor a hash" False
    Just (Api.TxInInfo _ Api.TxOut {Api.txOutDatum = inDatum}) -> case inDatum of
      Api.OutputDatumHash _ -> True
      Api.OutputDatum _ -> trace "I want a datum hash, but I got an inline datum" False
      Api.NoOutputDatum -> trace "I want a datum hash, but I got neither a datum nor a hash" False
    _ -> False

compiledInputDatumSpendingPurpose :: CompiledCode (Bool -> BuiltinData -> BuiltinUnit)
compiledInputDatumSpendingPurpose = $$(compile [||script||])
  where
    script b = Script.mkMultiPurposeScript $ Script.falseTypedMultiPurposeScript `Script.withSpendingPurpose` inputDatumSpendingPurpose b

requireInlineDatumInInputValidator :: Script.Versioned Script.Validator
requireInlineDatumInInputValidator =
  Script.toVersioned
    $ Script.MultiPurposeScript @SimpleContract
    $ Script.toScript
    $ compiledInputDatumSpendingPurpose
    `unsafeApplyCode` liftCode plcVersion110 True

requireHashedDatumInInputValidator :: Script.Versioned Script.Validator
requireHashedDatumInInputValidator =
  Script.toVersioned
    $ Script.MultiPurposeScript @SimpleContract
    $ Script.toScript
    $ compiledInputDatumSpendingPurpose
    `unsafeApplyCode` liftCode plcVersion110 False

data OutputDatumKind = OnlyHash | Datum | Inline

makeLift ''OutputDatumKind

-- | This defines three validators: @outputDatumValidator OnlyHash@ is a
-- validator that only returns true if there's a continuing transaction output
-- that has a datum hash that's not included in the 'txInfoData', inline datum,
-- @outputDatumSpendingPurpose Datum@ requires an output datum with a hash that's in
-- the 'txInfoData', and @outputDatumSpendingPurpose Inline@ only returns true if the
-- output has an inline datum.
{-# INLINEABLE outputDatumSpendingPurpose #-}
outputDatumSpendingPurpose :: OutputDatumKind -> Script.SpendingPurposeType SimpleContract
outputDatumSpendingPurpose datumKind oRef _ _ Api.TxInfo {txInfoInputs, txInfoOutputs, txInfoData} =
  case find ((oRef ==) . Api.txInInfoOutRef) txInfoInputs of
    Just (Api.TxInInfo _ Api.TxOut {txOutAddress})
      | [Api.TxOut {txOutDatum}] <- filter ((txOutAddress ==) . Api.txOutAddress) txInfoOutputs ->
          case (datumKind, txOutDatum) of
            (OnlyHash, Api.OutputDatumHash h) -> not $ Map.member h txInfoData
            (Datum, Api.OutputDatumHash h) -> Map.member h txInfoData
            (Inline, Api.OutputDatum _) -> True
            _ -> False
    _ -> False

compiledOutputDatumSpendingPurpose :: CompiledCode (OutputDatumKind -> BuiltinData -> BuiltinUnit)
compiledOutputDatumSpendingPurpose = $$(compile [||script||])
  where
    script b = Script.mkMultiPurposeScript $ Script.falseTypedMultiPurposeScript `Script.withSpendingPurpose` outputDatumSpendingPurpose b

requireInlineDatumInOutputValidator :: Script.Versioned Script.Validator
requireInlineDatumInOutputValidator =
  Script.toVersioned
    $ Script.MultiPurposeScript @()
    $ Script.toScript
    $ compiledOutputDatumSpendingPurpose
    `unsafeApplyCode` liftCode plcVersion110 Inline

requireHashedDatumInOutputValidator :: Script.Versioned Script.Validator
requireHashedDatumInOutputValidator =
  Script.toVersioned
    $ Script.MultiPurposeScript @()
    $ Script.toScript
    $ compiledOutputDatumSpendingPurpose
    `unsafeApplyCode` liftCode plcVersion110 Datum

requireOnlyHashedDatumInOutputValidator :: Script.Versioned Script.Validator
requireOnlyHashedDatumInOutputValidator =
  Script.toVersioned
    $ Script.MultiPurposeScript @()
    $ Script.toScript
    $ compiledOutputDatumSpendingPurpose
    `unsafeApplyCode` liftCode plcVersion110 OnlyHash
