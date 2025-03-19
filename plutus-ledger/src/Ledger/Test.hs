{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ledger.Test where

import Cardano.Api qualified as C
import Ledger qualified
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value.CardanoAPI (policyId)
import Plutus.Script.Utils.Scripts (Language (PlutusV1, PlutusV2, PlutusV3), Versioned (Versioned), scriptCurrencySymbol, toMintingPolicy, toMintingPolicyHash, toValidator)
import Plutus.Script.Utils.Typed as PSU
import Plutus.Script.Utils.V1.Address qualified as PV1
import Plutus.Script.Utils.V1.Scripts qualified as PV1
import Plutus.Script.Utils.V2.Address qualified as PV2
import Plutus.Script.Utils.V2.Scripts qualified as PV2
import PlutusLedgerApi.V1 (Address)
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 qualified as PV2
import PlutusTx qualified
import PlutusTx.Builtins.Internal qualified as PlutusTx
import Prelude hiding (not)

someCode :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit)
someCode = $$(PlutusTx.compile [||\_ _ _ -> PlutusTx.unitval||])

someValidator :: Scripts.Validator
someValidator = toValidator someCode

someTypedValidator :: Scripts.TypedValidator Any
someTypedValidator = Scripts.unsafeMkTypedValidator (Versioned someValidator PlutusV1)

someValidatorHash :: PV1.ValidatorHash
someValidatorHash = PV1.validatorHash someValidator

someCardanoAddress :: C.NetworkId -> Ledger.CardanoAddress
someCardanoAddress = flip PV1.mkValidatorCardanoAddress someValidator

someAddress :: Address
someAddress = Ledger.scriptValidatorHashAddress someValidatorHash Nothing

someValidatorV2 :: Scripts.Validator
someValidatorV2 = toValidator someCode

someTypedValidatorV2 :: Scripts.TypedValidator Any
someTypedValidatorV2 = Scripts.unsafeMkTypedValidator (Versioned someValidator PlutusV2)

someValidatorHashV2 :: PV2.ValidatorHash
someValidatorHashV2 = PV2.validatorHash someValidatorV2

someCardanoAddressV2 :: C.NetworkId -> Ledger.CardanoAddress
someCardanoAddressV2 = flip PV2.mkValidatorCardanoAddress someValidatorV2

someAddressV2 :: Address
someAddressV2 = Ledger.scriptValidatorHashAddress someValidatorHashV2 Nothing

{-# INLINEABLE mkPolicy #-}
mkPolicy :: () -> Ledger.ScriptContext -> Bool
mkPolicy _ _ = True

{-# INLINEABLE mkPolicyV2 #-}
mkPolicyV2 :: () -> PV2.ScriptContext -> Bool
mkPolicyV2 _ _ = True

coinMintingPolicy :: Language -> Versioned Ledger.MintingPolicy
coinMintingPolicy lang = case lang of
  PlutusV1 -> Versioned coinMintingPolicyV1 lang
  PlutusV2 -> Versioned coinMintingPolicyV2 lang
  PlutusV3 -> error "Unsupported"

coinMintingPolicyV1 :: Ledger.MintingPolicy
coinMintingPolicyV1 = toMintingPolicy $$(PlutusTx.compile [||PSU.mkUntypedMintingPolicy mkPolicy||])

coinMintingPolicyV2 :: Ledger.MintingPolicy
coinMintingPolicyV2 = toMintingPolicy $$(PlutusTx.compile [||PSU.mkUntypedMintingPolicy mkPolicyV2||])

coinMintingPolicyHash :: Language -> Ledger.MintingPolicyHash
coinMintingPolicyHash = toMintingPolicyHash . coinMintingPolicy

coinMintingPolicyCurrencySymbol :: Language -> Value.CurrencySymbol
coinMintingPolicyCurrencySymbol = scriptCurrencySymbol . coinMintingPolicyHash

someToken :: Language -> Value.Value
someToken lang = Value.singleton (coinMintingPolicyCurrencySymbol lang) "someToken" 1

asRedeemer :: (PlutusTx.ToData a) => a -> Ledger.Redeemer
asRedeemer a = Ledger.Redeemer $ PlutusTx.dataToBuiltinData $ PlutusTx.toData a

asDatum :: (PlutusTx.ToData a) => a -> Ledger.Datum
asDatum a = Ledger.Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData a

coinMintingPolicyId :: Language -> C.PolicyId
coinMintingPolicyId = policyId . coinMintingPolicy

testNetworkMagic :: C.NetworkMagic
testNetworkMagic = C.NetworkMagic 1097911063

testnet :: C.NetworkId
testnet = C.Testnet testNetworkMagic
