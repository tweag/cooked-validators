-- | This module exposes the generation of witnesses and reward account
module Cooked.MockChain.GenerateTx.Witness
  ( toRewardAccount,
    toScriptWitness,
    toKeyWitness,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Ledger.BaseTypes qualified as C.Ledger
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Control.Monad.Except (throwError)
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Common
import Cooked.Skeleton
import Cooked.Wallet
import Ledger.Address qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Translates a given credential to a reward account.
toRewardAccount :: (MonadBlockChainBalancing m) => Api.Credential -> m C.Ledger.RewardAccount
toRewardAccount cred =
  C.Ledger.RewardAccount C.Ledger.Testnet <$> case cred of
    Api.ScriptCredential scriptHash -> do
      Cardano.ScriptHash cHash <-
        throwOnToCardanoError
          "toRewardAccount: Unable to convert script hash."
          (Ledger.toCardanoScriptHash scriptHash)
      return $ C.Ledger.ScriptHashObj cHash
    Api.PubKeyCredential pubkeyHash -> do
      Cardano.StakeKeyHash pkHash <-
        throwOnToCardanoError
          "toRewardAccount: Unable to convert private key hash."
          (Ledger.toCardanoStakeKeyHash pubkeyHash)
      return $ C.Ledger.KeyHashObj pkHash

-- | Translates a script and a reference script utxo into either a plutus script
-- or a reference input containing the right script
toPlutusScriptOrReferenceInput :: (MonadBlockChainBalancing m) => Script.Versioned Script.Script -> Maybe Api.TxOutRef -> m (Cardano.PlutusScriptOrReferenceInput lang)
toPlutusScriptOrReferenceInput (Script.Versioned (Script.Script script) _) Nothing = return $ Cardano.PScript $ Cardano.PlutusScriptSerialised script
toPlutusScriptOrReferenceInput (Script.toScriptHash -> scriptHash) (Just scriptOutRef) = do
  (fmap Script.toScriptHash . txSkelOutReferenceScript -> mScriptHash) <- unsafeTxOutByRef scriptOutRef
  case mScriptHash of
    Just scriptHash'
      | scriptHash == scriptHash' ->
          Cardano.PReferenceScript
            <$> throwOnToCardanoError
              "toPlutusScriptOrReferenceInput: Unable to translate reference script utxo."
              (Ledger.toCardanoTxIn scriptOutRef)
    _ -> throwError $ MCEWrongReferenceScriptError scriptOutRef scriptHash mScriptHash

-- | Translates a script with its associated redeemer and datum to a script
-- witness. Note on the usage of 'Ledger.zeroExecutionUnits': at this stage of
-- the transaction create, we cannot know the execution units used by the
-- script. They will be filled out later on once the full body has been
-- generated. So, for now, we temporarily leave them to 0.
toScriptWitness :: (MonadBlockChainBalancing m, Script.ToVersioned Script.Script a) => a -> TxSkelRedeemer -> Cardano.ScriptDatum b -> m (Cardano.ScriptWitness b Cardano.ConwayEra)
toScriptWitness (Script.toVersioned -> script@(Script.Versioned _ version)) (TxSkelRedeemer {..}) datum = do
  let scriptData = Ledger.toCardanoScriptData $ Api.toBuiltinData txSkelRedeemerContent
  case version of
    Script.PlutusV1 ->
      (\x -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV1InConway Cardano.PlutusScriptV1 x datum scriptData Ledger.zeroExecutionUnits)
        <$> toPlutusScriptOrReferenceInput script txSkelRedeemerReferenceInput
    Script.PlutusV2 ->
      (\x -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV2InConway Cardano.PlutusScriptV2 x datum scriptData Ledger.zeroExecutionUnits)
        <$> toPlutusScriptOrReferenceInput script txSkelRedeemerReferenceInput
    Script.PlutusV3 ->
      (\x -> Cardano.PlutusScriptWitness Cardano.PlutusScriptV3InConway Cardano.PlutusScriptV3 x datum scriptData Ledger.zeroExecutionUnits)
        <$> toPlutusScriptOrReferenceInput script txSkelRedeemerReferenceInput

-- | Generates a list of witnesses for a given wallet and body
toKeyWitness :: Cardano.TxBody Cardano.ConwayEra -> Wallet -> Cardano.KeyWitness Cardano.ConwayEra
toKeyWitness txBody =
  Cardano.makeShelleyKeyWitness Cardano.ShelleyBasedEraConway txBody
    . Ledger.toWitness
    . Ledger.PaymentPrivateKey
    . walletSK
