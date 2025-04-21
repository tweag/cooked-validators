{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Attack.DupToken where

import Plutus.Script.Utils.V3 qualified as Script
import PlutusCore.Version
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx
import PlutusTx.Prelude

{-# INLINEABLE carefulPolicyMintingPurpose #-}
carefulPolicyMintingPurpose :: Api.TokenName -> Integer -> Script.MintingPurposeType ()
carefulPolicyMintingPurpose tn n cs _ (Api.TxInfo {txInfoMint}) =
  case Api.flattenValue (Script.toValue txInfoMint) of
    [(cs', tn', n')] -> cs' == cs && tn' == tn && n' == n
    _ -> trace "tried to mint wrong amount" False

carefulPolicyCompiled :: CompiledCode (Api.TokenName -> Integer -> BuiltinData -> BuiltinUnit)
carefulPolicyCompiled = $$(compile [||script||])
  where
    script tn n =
      Script.mkMultiPurposeScript
        $ Script.falseTypedMultiPurposeScript
        `Script.withMintingPurpose` carefulPolicyMintingPurpose tn n

carefulPolicy :: Api.TokenName -> Integer -> Script.Versioned Script.MintingPolicy
carefulPolicy tName allowedAmount =
  Script.toVersioned
    $ Script.MultiPurposeScript @()
    $ Script.toScript
    $ carefulPolicyCompiled
    `unsafeApplyCode` liftCode plcVersion110 tName
    `unsafeApplyCode` liftCode plcVersion110 allowedAmount

carelessPolicy :: Script.Versioned Script.MintingPolicy
carelessPolicy = Script.toVersioned Script.trueMintingMPScript
