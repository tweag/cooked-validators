{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ledger.Test where

import Cardano.Api qualified as C
import Ledger.Address qualified as Ledger
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value.CardanoAPI (policyId)
import Plutus.Script.Utils.Address (ToCardanoAddress (toCardanoAddress))
import Plutus.Script.Utils.Scripts (Language (PlutusV1, PlutusV2, PlutusV3), MintingPolicy, MintingPolicyHash, ValidatorHash, Versioned (Versioned), toCurrencySymbol, toMintingPolicy, toMintingPolicyHash, toValidator, toValidatorHash)
import Plutus.Script.Utils.Typed as PSU
import PlutusLedgerApi.V1 (Address)
import PlutusLedgerApi.V1 qualified as PV1
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

someValidatorHash :: ValidatorHash
someValidatorHash = toValidatorHash someTypedValidator

someCardanoAddress :: C.NetworkId -> Ledger.CardanoAddress
someCardanoAddress = flip toCardanoAddress someTypedValidator

someAddress :: Address
someAddress = Ledger.scriptValidatorHashAddress someValidatorHash Nothing

someValidatorV2 :: Scripts.Validator
someValidatorV2 = toValidator someCode

someTypedValidatorV2 :: Scripts.TypedValidator Any
someTypedValidatorV2 = Scripts.unsafeMkTypedValidator (Versioned someValidator PlutusV2)

someValidatorHashV2 :: ValidatorHash
someValidatorHashV2 = toValidatorHash someTypedValidatorV2

someCardanoAddressV2 :: C.NetworkId -> Ledger.CardanoAddress
someCardanoAddressV2 = flip toCardanoAddress someTypedValidatorV2

someAddressV2 :: Address
someAddressV2 = Ledger.scriptValidatorHashAddress someValidatorHashV2 Nothing

{-# INLINEABLE mkPolicy #-}
mkPolicy :: () -> PV1.ScriptContext -> Bool
mkPolicy _ _ = True

{-# INLINEABLE mkPolicyV2 #-}
mkPolicyV2 :: () -> PV2.ScriptContext -> Bool
mkPolicyV2 _ _ = True

coinMintingPolicy :: Language -> Versioned MintingPolicy
coinMintingPolicy lang = case lang of
  PlutusV1 -> Versioned coinMintingPolicyV1 lang
  PlutusV2 -> Versioned coinMintingPolicyV2 lang
  PlutusV3 -> error "Unsupported"

coinMintingPolicyV1 :: MintingPolicy
coinMintingPolicyV1 = toMintingPolicy $$(PlutusTx.compile [||PSU.mkUntypedMintingPolicy mkPolicy||])

coinMintingPolicyV2 :: MintingPolicy
coinMintingPolicyV2 = toMintingPolicy $$(PlutusTx.compile [||PSU.mkUntypedMintingPolicy mkPolicyV2||])

coinMintingPolicyHash :: Language -> MintingPolicyHash
coinMintingPolicyHash = toMintingPolicyHash . coinMintingPolicy

coinMintingPolicyCurrencySymbol :: Language -> Value.CurrencySymbol
coinMintingPolicyCurrencySymbol = toCurrencySymbol . coinMintingPolicyHash

someToken :: Language -> Value.Value
someToken lang = Value.singleton (coinMintingPolicyCurrencySymbol lang) "someToken" 1

asRedeemer :: (PlutusTx.ToData a) => a -> PV2.Redeemer
asRedeemer a = PV2.Redeemer $ PlutusTx.dataToBuiltinData $ PlutusTx.toData a

asDatum :: (PlutusTx.ToData a) => a -> PV2.Datum
asDatum a = PV2.Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData a

coinMintingPolicyId :: Language -> C.PolicyId
coinMintingPolicyId = policyId . coinMintingPolicy

testNetworkMagic :: C.NetworkMagic
testNetworkMagic = C.NetworkMagic 1097911063

testnet :: C.NetworkId
testnet = C.Testnet testNetworkMagic
