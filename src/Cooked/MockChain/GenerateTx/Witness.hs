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
toRewardAccount :: Api.Credential -> WitnessGen (Cardano.RewardAccount Crypto.StandardCrypto)
toRewardAccount cred =
  Cardano.RewardAccount Cardano.Testnet <$> case cred of
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
          (Ledger.toCardanoStakeKeyHash pubkeyHash)
      return $ Cardano.KeyHashObj pkHash

-- | Translate a script and a reference script utxo into into either a plutus
-- script or a reference input containing the right script
toPlutusScriptOrReferenceInput :: Api.SerialisedScript -> Maybe Api.TxOutRef -> WitnessGen (Cardano.PlutusScriptOrReferenceInput lang)
toPlutusScriptOrReferenceInput script Nothing = return $ Cardano.PScript $ Cardano.PlutusScriptSerialised script
toPlutusScriptOrReferenceInput script (Just scriptOutRef) = do
  referenceScriptsMap <- asks $ Map.mapMaybe (^. outputReferenceScriptL)
  refScriptHash <-
    throwOnLookup
      "toPlutusScriptOrReferenceInput: Can't resolve reference script utxo."
      scriptOutRef
      referenceScriptsMap
  when (refScriptHash /= toScriptHash script) $
    throwOnString "toPlutusScriptOrReferenceInput: Wrong reference script hash."
  scriptTxIn <-
    throwOnToCardanoError
      "toPlutusScriptOrReferenceInput: Unable to translate reference script utxo."
      (Ledger.toCardanoTxIn scriptOutRef)
  scriptHash <-
    throwOnToCardanoError
      "toPlutusScriptOrReferenceInput: Unable to translate script hash of reference script."
      (Ledger.toCardanoScriptHash refScriptHash)
  return $ Cardano.PReferenceScript scriptTxIn (Just scriptHash)

-- | Translates a script with its associated redeemer and datum to a script
-- witness.
toScriptWitness :: (ToScript a) => a -> TxSkelRedeemer -> Cardano.ScriptDatum b -> WitnessGen (Cardano.ScriptWitness b Cardano.ConwayEra)
toScriptWitness (toScript -> (Script.Versioned (Script.Script script) version)) (TxSkelRedeemer {..}) datum =
  let scriptData = case txSkelRedeemer of
        EmptyRedeemer -> Ledger.toCardanoScriptData $ Api.toBuiltinData ()
        SomeRedeemer s -> Ledger.toCardanoScriptData $ Api.toBuiltinData s
   in case version of
        Script.PlutusV1 ->
          (\x -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV1InConway Cardano.PlutusScriptV1 x datum scriptData Ledger.zeroExecutionUnits)
            <$> toPlutusScriptOrReferenceInput script txSkelReferenceScript
        Script.PlutusV2 ->
          (\x -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV2InConway Cardano.PlutusScriptV2 x datum scriptData Ledger.zeroExecutionUnits)
            <$> toPlutusScriptOrReferenceInput script txSkelReferenceScript
        Script.PlutusV3 ->
          (\x -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV3InConway Cardano.PlutusScriptV3 x datum scriptData Ledger.zeroExecutionUnits)
            <$> toPlutusScriptOrReferenceInput script txSkelReferenceScript
