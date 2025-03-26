module Plutus.Script.Utils.V1.Address
  ( mkValidatorAddress,
    mkValidatorCardanoAddress,
    mkMintingPolicyCardanoAddress,
    mkStakeValidatorCardanoAddress,
  )
where

import Cardano.Api qualified as Script
import Plutus.Script.Utils.Scripts
  ( Script,
    Validator,
    getMintingPolicy,
    getStakeValidator,
    getValidator,
  )
import Plutus.Script.Utils.V1.Scripts qualified as PV1
import PlutusLedgerApi.V1 (Address (Address), Credential (ScriptCredential))

{-# INLINEABLE mkValidatorAddress #-}

-- | The address that should be used by a transaction output locked by the given
-- Plutus V1 validator script.
mkValidatorAddress :: Validator -> Address
mkValidatorAddress validator = Address (ScriptCredential (PV1.scriptHash $ getValidator validator)) Nothing

-- | Cardano address of a 'PV1.Validator' script.
mkValidatorCardanoAddress ::
  Script.NetworkId -> PV1.Validator -> Script.AddressInEra Script.ConwayEra
mkValidatorCardanoAddress networkId = toScriptAddress networkId . getValidator

-- | Cardano address of a 'PV1.MintingPolicy' script.
mkMintingPolicyCardanoAddress ::
  Script.NetworkId -> PV1.MintingPolicy -> Script.AddressInEra Script.ConwayEra
mkMintingPolicyCardanoAddress networkId = toScriptAddress networkId . getMintingPolicy

-- | Cardano address of a 'PV1.MintingPolicy' script.
mkStakeValidatorCardanoAddress ::
  Script.NetworkId -> PV1.StakeValidator -> Script.AddressInEra Script.ConwayEra
mkStakeValidatorCardanoAddress networkId = toScriptAddress networkId . getStakeValidator

-- | Convert a 'Script' to a 'cardano-api' address.
--
-- For why we depend on `cardano-api`,
-- see note [Hash computation of datums, redeemers and scripts]
toScriptAddress :: Script.NetworkId -> Script -> Script.AddressInEra Script.ConwayEra
toScriptAddress networkId script =
  Script.makeShelleyAddressInEra
    Script.shelleyBasedEra
    networkId
    ( Script.PaymentCredentialByScript
        . Script.hashScript
        $ PV1.toCardanoApiScript script
    )
    Script.NoStakeAddress
