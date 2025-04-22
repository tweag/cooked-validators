{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Attack.DatumHijacking where

import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusLedgerApi.V3.Contexts qualified as Api
import PlutusTx
import PlutusTx.Prelude
import Prelude qualified as HS

-- * Mock contract for the datum hijacking attack

-- This is a very simple contract: The first transaction locks some
-- Ada to the validator, using the datum 'FirstLock', the second
-- transaction then re-locks the same amount to the same validator,
-- using the datum 'SecondLock'. The datum hijacking attack should
-- target the second transaction, and substitute a different
-- recipient.

data LockDatum = FirstLock | SecondLock deriving (HS.Show, HS.Eq)

instance Eq LockDatum where
  {-# INLINEABLE (==) #-}
  FirstLock == FirstLock = True
  SecondLock == SecondLock = True
  _ == _ = False

makeLift ''LockDatum
unstableMakeIsData ''LockDatum

data DHContract

instance Script.MultiPurposeScriptTypes DHContract where
  type SpendingDatumType DHContract = LockDatum

lockValue :: Api.Value
lockValue = Script.lovelace 12345678

-- | Try to extract a datum from an output.
{-# INLINEABLE outputDatum #-}
outputDatum :: Api.TxInfo -> Api.TxOut -> Maybe LockDatum
outputDatum txi o = case Api.txOutDatum o of
  Api.NoOutputDatum -> Nothing
  Api.OutputDatumHash h -> do
    Api.Datum d <- Api.findDatum h txi
    Api.fromBuiltinData d
  Api.OutputDatum (Api.Datum d) -> Api.fromBuiltinData d

{-# INLINEABLE mockValidatorSpendingPurpose #-}
mockValidatorSpendingPurpose :: (Api.TxInfo -> [Api.TxOut]) -> Script.SpendingPurposeType DHContract
mockValidatorSpendingPurpose getOutputs _ (Just FirstLock) _ txi =
  case getOutputs txi of
    o : _ ->
      traceIfFalse "not in 'SecondLock'-state after re-locking" (outputDatum txi o == Just SecondLock)
        && traceIfFalse "not re-locking the right amout" (Api.txOutValue o == lockValue)
    _ -> trace "there must be a output re-locked" False
mockValidatorSpendingPurpose _ _ _ _ _ = False

carefulValidator :: Script.MultiPurposeScript DHContract
carefulValidator =
  Script.MultiPurposeScript
    $ Script.toScript $$(compile [||script||])
  where
    script =
      Script.mkMultiPurposeScript
        $ Script.falseTypedMultiPurposeScript
        `Script.withSpendingPurpose` mockValidatorSpendingPurpose (\txi -> Api.getContinuingOutputs $ Api.ScriptContext txi (error ()) (error ()))

carelessValidator :: Script.MultiPurposeScript DHContract
carelessValidator =
  Script.MultiPurposeScript
    $ Script.toScript $$(compile [||script||])
  where
    script =
      Script.mkMultiPurposeScript
        $ Script.falseTypedMultiPurposeScript
        `Script.withSpendingPurpose` mockValidatorSpendingPurpose Api.txInfoOutputs
