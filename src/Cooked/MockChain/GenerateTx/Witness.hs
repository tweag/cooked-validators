module Cooked.MockChain.GenerateTx.Witness
  ( toRewardAccount,
    toScriptWitness,
  )
where

import Cardano.Api.Shelley qualified as Cardano hiding (Testnet)
import Cardano.Ledger.Address qualified as Cardano
import Cardano.Ledger.BaseTypes qualified as Cardano
import Cardano.Ledger.Credential qualified as Cardano
import Cardano.Ledger.Crypto qualified as Crypto
import Control.Monad
import Control.Monad.Reader
import Cooked.Conversion
import Cooked.MockChain.GenerateTx.Common
import Cooked.Output
import Cooked.Skeleton
import Data.Map (Map)
import Data.Map qualified as Map
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

type WitnessGen a = TxGen (Map Api.TxOutRef Api.TxOut) a

-- | Translates a given credential to a reward account.
toRewardAccount :: Api.Credential -> WitnessGen (Cardano.RewardAcnt Crypto.StandardCrypto)
toRewardAccount cred =
  Cardano.RewardAcnt Cardano.Testnet <$> case cred of
    Api.ScriptCredential scriptHash -> do
      Cardano.ScriptHash cHash <-
        throwOnToCardanoError
          "toRewardAccount: Unable to convert script hash."
          (Ledger.toCardanoScriptHash scriptHash)
      return $ Cardano.ScriptHashObj cHash
    Api.PubKeyCredential pubkeyHash -> do
      Cardano.StakeKeyHash pkHash <-
        throwOnToCardanoError
          "toRewardAccount: Unable to convert private key hash."
          -- TODO: we take the pubkeyHash, maybe we should take the stakehash if
          -- any exists. The nature of the stake address can be confusing.
          (Ledger.toCardanoStakeKeyHash pubkeyHash)
      return $ Cardano.KeyHashObj pkHash

-- | Translates a serialised script and a redeemer to their Cardano
-- counterparts. They cannot be uncoupled because of the possible presence of a
-- reference script utxo in the redeemer.
toScriptAndRedeemerData :: Api.SerialisedScript -> TxSkelRedeemer -> WitnessGen (Cardano.PlutusScriptOrReferenceInput lang, Cardano.HashableScriptData)
toScriptAndRedeemerData script TxSkelNoRedeemer =
  return (Cardano.PScript $ Cardano.PlutusScriptSerialised script, Ledger.toCardanoScriptData $ Api.toBuiltinData ())
toScriptAndRedeemerData script (TxSkelRedeemerForScript redeemer) =
  return (Cardano.PScript $ Cardano.PlutusScriptSerialised script, Ledger.toCardanoScriptData $ Api.toBuiltinData redeemer)
toScriptAndRedeemerData script (TxSkelRedeemerForReferenceScript validatorOref redeemer) = do
  referenceScriptsMap <- asks $ Map.mapMaybe (^. outputReferenceScriptL)
  refScriptHash <-
    throwOnLookup
      "toScriptAndRedeemerData: Can't resolve reference script utxo."
      validatorOref
      referenceScriptsMap
  when (refScriptHash /= toScriptHash script) $
    throwOnString "toScriptAndRedeemerData: Wrong reference script hash."
  validatorTxIn <-
    throwOnToCardanoError
      "toScriptAndRedeemerData: Unable to translate reference script utxo."
      (Ledger.toCardanoTxIn validatorOref)
  scriptHash <-
    throwOnToCardanoError
      "toScriptAndRedeemerData: Unable to translate script hash of reference script."
      (Ledger.toCardanoScriptHash refScriptHash)
  return (Cardano.PReferenceScript validatorTxIn (Just scriptHash), Ledger.toCardanoScriptData $ Api.toBuiltinData redeemer)

-- | Translates a script with its associated redeemer and datum to a script
-- witness.
toScriptWitness :: (ToScript a) => a -> TxSkelRedeemer -> Cardano.ScriptDatum b -> WitnessGen (Cardano.ScriptWitness b Cardano.ConwayEra)
toScriptWitness (toScript -> (Script.Versioned (Script.Script script) version)) redeemer datum =
  case version of
    Script.PlutusV1 ->
      (\(x, y) -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV1InConway Cardano.PlutusScriptV1 x datum y Ledger.zeroExecutionUnits)
        <$> toScriptAndRedeemerData script redeemer
    Script.PlutusV2 ->
      (\(x, y) -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV2InConway Cardano.PlutusScriptV2 x datum y Ledger.zeroExecutionUnits)
        <$> toScriptAndRedeemerData script redeemer
    Script.PlutusV3 ->
      (\(x, y) -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV3InConway Cardano.PlutusScriptV3 x datum y Ledger.zeroExecutionUnits)
        <$> toScriptAndRedeemerData script redeemer
