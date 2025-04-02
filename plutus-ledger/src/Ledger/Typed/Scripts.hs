module Ledger.Typed.Scripts
  ( mkForwardingMintingPolicy,
    mkTypedValidator,
  )
where

import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Typed qualified as Script
import Plutus.Script.Utils.V1.Generators qualified as PV1
import Plutus.Script.Utils.V1.Typed qualified as PV1
import Plutus.Script.Utils.V2.Generators qualified as PV2
import Plutus.Script.Utils.V2.Typed qualified as PV2

mkForwardingMintingPolicy :: Script.Versioned Script.Validator -> Script.Versioned Script.MintingPolicy
mkForwardingMintingPolicy vl@(Script.Versioned _ Script.PlutusV1) = PV1.mkForwardingMintingPolicy (Script.toValidatorHash vl) <$ vl
mkForwardingMintingPolicy vl@(Script.Versioned _ Script.PlutusV2) = PV2.mkForwardingMintingPolicy (Script.toValidatorHash vl) <$ vl
mkForwardingMintingPolicy _ = error "Standalone forwarding minting policy are no longer relevant in PlutusV3"

mkTypedValidator :: Script.Versioned Script.Validator -> Script.TypedValidator a
mkTypedValidator (Script.Versioned val Script.PlutusV1) = PV1.validatorToTypedValidator val
mkTypedValidator (Script.Versioned val Script.PlutusV2) = PV2.validatorToTypedValidator val
mkTypedValidator _ = error "Standalone typed validators are no longer relevant in PlutusV3"
