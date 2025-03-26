module Plutus.Script.Utils.V3.Address
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
import Plutus.Script.Utils.V3.Scripts qualified as PV3
import PlutusLedgerApi.V3 (Address (Address), Credential (ScriptCredential))

{-# INLINEABLE mkValidatorAddress #-}

-- | The address that should be used by a transaction output locked by the given
-- Plutus V3 validator script.
mkValidatorAddress :: Validator -> Address
mkValidatorAddress validator = Address (ScriptCredential (PV3.scriptHash $ getValidator validator)) Nothing

-- | Cardano address of a 'PV3.Validator' script.
mkValidatorCardanoAddress ::
  Script.NetworkId -> PV3.Validator -> Script.AddressInEra Script.ConwayEra
mkValidatorCardanoAddress networkId = toScriptAddress networkId . getValidator

-- | Cardano address of a 'PV3.MintingPolicy' script.
mkMintingPolicyCardanoAddress ::
  Script.NetworkId -> PV3.MintingPolicy -> Script.AddressInEra Script.ConwayEra
mkMintingPolicyCardanoAddress networkId = toScriptAddress networkId . getMintingPolicy

-- | Cardano address of a 'PV3.StakeValidator' script.
mkStakeValidatorCardanoAddress ::
  Script.NetworkId -> PV3.StakeValidator -> Script.AddressInEra Script.ConwayEra
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
        $ PV3.toCardanoApiScript script
    )
    Script.NoStakeAddress
