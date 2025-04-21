{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Plutus.ReferenceScripts where

import Plutus.Script.Utils.V2 qualified as Script
import PlutusLedgerApi.V2 qualified as Api
import PlutusTx
import PlutusTx.Prelude

-- | This validator ensures that the given public key signs the
-- transaction.
requireSignerValidator :: Api.PubKeyHash -> Script.TypedValidator ()
requireSignerValidator =
  Script.mkTypedValidatorParam
    $$(compile [||val||])
    $$(compile [||wrap||])
  where
    val :: Api.PubKeyHash -> () -> () -> Api.ScriptContext -> Bool
    val pkh _ _ (Api.ScriptContext txInfo _) =
      traceIfFalse "the required signer is missing"
        $ elem pkh (Api.txInfoSignatories txInfo)

    wrap = Script.mkUntypedValidator

-- | This validator ensures that there is a transaction input that has
-- a reference script with the given hash.
requireRefScriptValidator :: Api.ScriptHash -> Script.TypedValidator ()
requireRefScriptValidator =
  Script.mkTypedValidatorParam
    $$(compile [||val||])
    $$(compile [||wrap||])
  where
    val :: Api.ScriptHash -> () -> () -> Api.ScriptContext -> Bool
    val expectedScriptHash _ _ (Api.ScriptContext txInfo _) =
      traceIfFalse "there is no reference input with the correct script hash"
        $ any
          ( \(Api.TxInInfo _ (Api.TxOut _ _ _ mRefScriptHash)) ->
              Just expectedScriptHash == mRefScriptHash
          )
          (Api.txInfoReferenceInputs txInfo)

    wrap = Script.mkUntypedValidator
