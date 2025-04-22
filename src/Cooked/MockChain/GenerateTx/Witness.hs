-- | This module exposes the generation of witnesses and reward account
module Cooked.MockChain.GenerateTx.Witness
  ( toRewardAccount,
    toScriptWitness,
  )
where

import Cardano.Api.Shelley qualified as Cardano hiding (Testnet)
import Cardano.Ledger.Address qualified as Cardano
import Cardano.Ledger.BaseTypes qualified as Cardano
import Cardano.Ledger.Credential qualified as Cardano
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Common
import Cooked.Output
import Cooked.Skeleton
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Translates a given credential to a reward account.
toRewardAccount :: (MonadBlockChainBalancing m) => Api.Credential -> m Cardano.RewardAccount
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

-- | Translates a script and a reference script utxo into either a plutus script
-- or a reference input containing the right script
toPlutusScriptOrReferenceInput :: (MonadBlockChainBalancing m) => Script.Versioned Script.Script -> Maybe Api.TxOutRef -> m (Cardano.PlutusScriptOrReferenceInput lang)
toPlutusScriptOrReferenceInput (Script.Versioned (Script.Script script) _) Nothing = return $ Cardano.PScript $ Cardano.PlutusScriptSerialised script
toPlutusScriptOrReferenceInput (Script.toScriptHash -> scriptHash) (Just scriptOutRef) = do
  ((^. outputReferenceScriptL) -> mScriptHash) <- unsafeTxOutByRef scriptOutRef
  case mScriptHash of
    Nothing -> throwOnString "toPlutusScriptOrReferenceInput: No reference script found in utxo."
    Just scriptHash' | scriptHash /= scriptHash' -> throwOnString "toPlutusScriptOrReferenceInput: Wrong reference script hash."
    _ ->
      Cardano.PReferenceScript
        <$> throwOnToCardanoError
          "toPlutusScriptOrReferenceInput: Unable to translate reference script utxo."
          (Ledger.toCardanoTxIn scriptOutRef)

-- | Translates a script with its associated redeemer and datum to a script
-- witness.
toScriptWitness :: (MonadBlockChainBalancing m, Script.ToVersioned Script.Script a) => a -> TxSkelRedeemer -> Cardano.ScriptDatum b -> m (Cardano.ScriptWitness b Cardano.ConwayEra)
toScriptWitness (Script.toVersioned -> script@(Script.Versioned _ version)) (TxSkelRedeemer {..}) datum =
  let scriptData = Ledger.toCardanoScriptData $ Api.toBuiltinData txSkelRedeemerContent
   in case version of
        Script.PlutusV1 ->
          (\x -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV1InConway Cardano.PlutusScriptV1 x datum scriptData Ledger.zeroExecutionUnits)
            <$> toPlutusScriptOrReferenceInput script txSkelRedeemerReferenceInput
        Script.PlutusV2 ->
          (\x -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV2InConway Cardano.PlutusScriptV2 x datum scriptData Ledger.zeroExecutionUnits)
            <$> toPlutusScriptOrReferenceInput script txSkelRedeemerReferenceInput
        Script.PlutusV3 ->
          (\x -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV3InConway Cardano.PlutusScriptV3 x datum scriptData Ledger.zeroExecutionUnits)
            <$> toPlutusScriptOrReferenceInput script txSkelRedeemerReferenceInput
