{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Plutus.Script.Utils.V3.Scripts
  ( -- * Script data hashes
    PV3.Datum,
    PV3.DatumHash,
    PV3.Redeemer,
    PV3.RedeemerHash,

    -- * Script hashes
    PV3.Validator,
    PV3.ValidatorHash,
    PV3.MintingPolicy,
    PV3.MintingPolicyHash,
    PV3.StakeValidator,
    PV3.StakeValidatorHash,
    validatorHash,
    mintingPolicyHash,
    stakeValidatorHash,
    scriptHash,

    -- * Script utilities
    scriptCurrencySymbol,
    toCardanoApiScript,
  )
where

import Cardano.Api qualified as Script
import Cardano.Api.Shelley qualified as Script
import Plutus.Script.Utils.Scripts qualified as PV3
import PlutusLedgerApi.V3 qualified as PV3
import PlutusTx.Builtins qualified as Builtins

-- | Hash a 'PV3.Validator' script.
validatorHash :: PV3.Validator -> PV3.ValidatorHash
validatorHash =
  PV3.ValidatorHash
    . PV3.getScriptHash
    . scriptHash
    . PV3.getValidator

-- | Hash a 'PV3.MintingPolicy' script.
mintingPolicyHash :: PV3.MintingPolicy -> PV3.MintingPolicyHash
mintingPolicyHash =
  PV3.MintingPolicyHash
    . PV3.getScriptHash
    . scriptHash
    . PV3.getMintingPolicy

-- | Hash a 'PV3.StakeValidator' script.
stakeValidatorHash :: PV3.StakeValidator -> PV3.StakeValidatorHash
stakeValidatorHash =
  PV3.StakeValidatorHash
    . PV3.getScriptHash
    . scriptHash
    . PV3.getStakeValidator

-- | Convert a 'Builtins.BuiltinsData' value to a 'cardano-api' script
--  data value.
--
-- For why we depend on `cardano-api`,
-- see note [Hash computation of datums, redeemers and scripts]
-- toCardanoAPIData :: Builtins.BuiltinData -> Script.ScriptData
-- toCardanoAPIData = Script.fromPlutusData . Builtins.builtinDataToData

-- | Hash a 'Script'
scriptHash :: PV3.Script -> PV3.ScriptHash
scriptHash =
  PV3.ScriptHash
    . Builtins.toBuiltin
    . Script.serialiseToRawBytes
    . Script.hashScript
    . toCardanoApiScript

-- | Convert a 'Script' to a 'cardano-api' script.
--
-- For why we depend on `cardano-api`,
-- see note [Hash computation of datums, redeemers and scripts]
toCardanoApiScript :: PV3.Script -> Script.Script Script.PlutusScriptV3
toCardanoApiScript =
  Script.PlutusScript Script.PlutusScriptV3
    . Script.PlutusScriptSerialised
    . PV3.unScript

{-# INLINEABLE scriptCurrencySymbol #-}

-- | The 'CurrencySymbol' of a 'MintingPolicy'.
scriptCurrencySymbol :: PV3.MintingPolicy -> PV3.CurrencySymbol
scriptCurrencySymbol scrpt =
  let (PV3.MintingPolicyHash hsh) = mintingPolicyHash scrpt in PV3.CurrencySymbol hsh

{- See Note [Hash computation of datums, redeemers and scripts] -}

{- See Note [Scripts returning Bool] -}
