-- | This module exposes the generation of witnesses and reward account
module Cooked.MockChain.GenerateTx.Witness
  ( toRewardAccount,
    toCardanoCredential,
    toScriptWitness,
    toKeyWitness,
    toStakeCredential,
    deserialiseFromBuiltinByteString,
    toScriptHash,
    toKeyHash,
    toDRepCredential,
    toStakePoolKeyHash,
    toColdCredential,
    toHotCredential,
    toVRFVerKeyHash,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Api.Ledger qualified as Cardano
import Cardano.Ledger.BaseTypes qualified as C.Ledger
import Cardano.Ledger.Hashes qualified as C.Ledger
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Control.Monad.Except (throwError)
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Common
import Cooked.Skeleton
import Cooked.Wallet
import Ledger.Address qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Translates a given credential to a reward account.
toRewardAccount :: (MonadBlockChainBalancing m) => Api.Credential -> m C.Ledger.RewardAccount
toRewardAccount = (C.Ledger.RewardAccount C.Ledger.Testnet <$>) . toCardanoCredential Cardano.AsStakeKey Cardano.unStakeKeyHash

-- TODO: if this works, migrate to plutus-ledger

-- | Converts an 'Api.PubKeyHash' to any kind of key
deserialiseFromBuiltinByteString ::
  (MonadBlockChainBalancing m, Cardano.SerialiseAsRawBytes a) =>
  Cardano.AsType a ->
  Api.BuiltinByteString ->
  m a
deserialiseFromBuiltinByteString asType =
  throwOnToCardanoError "deserialiseFromBuiltinByteString" . Ledger.deserialiseFromRawBytes asType . Api.fromBuiltin

-- | Converts a plutus script hash into a cardano ledger script hash
toScriptHash :: (MonadBlockChainBalancing m) => Api.ScriptHash -> m C.Ledger.ScriptHash
toScriptHash (Api.ScriptHash sHash) = do
  Cardano.ScriptHash cHash <- deserialiseFromBuiltinByteString Cardano.AsScriptHash sHash
  return cHash

-- | Converts a plutus pkhash into a certain cardano ledger hash
toKeyHash ::
  (MonadBlockChainBalancing m, Cardano.SerialiseAsRawBytes (Cardano.Hash key)) =>
  Cardano.AsType key ->
  (Cardano.Hash key -> C.Ledger.KeyHash kr) ->
  Api.PubKeyHash ->
  m (C.Ledger.KeyHash kr)
toKeyHash asType unwrap = fmap unwrap . deserialiseFromBuiltinByteString (Cardano.AsHash asType) . Api.getPubKeyHash

-- | Converts an 'Api.PubKeyHash' into a cardano ledger stake pool key hash
toStakePoolKeyHash :: (MonadBlockChainBalancing m) => Api.PubKeyHash -> m (C.Ledger.KeyHash 'C.Ledger.StakePool)
toStakePoolKeyHash = toKeyHash Cardano.AsStakePoolKey Cardano.unStakePoolKeyHash

-- | Converts an 'Api.PubKeyHash' into a cardano ledger VRFVerKeyHash
toVRFVerKeyHash :: (MonadBlockChainBalancing m) => Api.PubKeyHash -> m (C.Ledger.VRFVerKeyHash a)
toVRFVerKeyHash (Api.PubKeyHash pkh) = do
  Cardano.VrfKeyHash key <- deserialiseFromBuiltinByteString (Cardano.AsHash Cardano.AsVrfKey) pkh
  return $ Cardano.toVRFVerKeyHash key

-- | Converts an 'Api.Credential' to a Cardano Credential of the expected kind
toCardanoCredential ::
  (MonadBlockChainBalancing m, Cardano.SerialiseAsRawBytes (Cardano.Hash key)) =>
  Cardano.AsType key ->
  (Cardano.Hash key -> C.Ledger.KeyHash kr) ->
  Api.Credential ->
  m (C.Ledger.Credential kr)
toCardanoCredential _ _ (Api.ScriptCredential sHash) = C.Ledger.ScriptHashObj <$> toScriptHash sHash
toCardanoCredential asType unwrap (Api.PubKeyCredential pkHash) = C.Ledger.KeyHashObj <$> toKeyHash asType unwrap pkHash

-- | Translates a credential into a Cardano stake credential
toStakeCredential :: (MonadBlockChainBalancing m) => Api.Credential -> m (C.Ledger.Credential 'C.Ledger.Staking)
toStakeCredential = toCardanoCredential Cardano.AsStakeKey Cardano.unStakeKeyHash

-- | Translates a credential into a Cardano drep credential
toDRepCredential :: (MonadBlockChainBalancing m) => Api.Credential -> m (C.Ledger.Credential 'C.Ledger.DRepRole)
toDRepCredential = toCardanoCredential Cardano.AsDRepKey Cardano.unDRepKeyHash

-- | Translates a credential into a Cardano cold committee credential
toColdCredential :: (MonadBlockChainBalancing m) => Api.Credential -> m (C.Ledger.Credential 'C.Ledger.ColdCommitteeRole)
toColdCredential = toCardanoCredential Cardano.AsCommitteeColdKey Cardano.unCommitteeColdKeyHash

-- | Translates a credential into a Cardano hot committee credential
toHotCredential :: (MonadBlockChainBalancing m) => Api.Credential -> m (C.Ledger.Credential 'C.Ledger.HotCommitteeRole)
toHotCredential = toCardanoCredential Cardano.AsCommitteeHotKey Cardano.unCommitteeHotKeyHash

-- | Translates a script and a reference script utxo into either a plutus script
-- or a reference input containing the right script
toPlutusScriptOrReferenceInput :: (MonadBlockChainBalancing m) => VScript -> Maybe Api.TxOutRef -> m (Cardano.PlutusScriptOrReferenceInput lang)
toPlutusScriptOrReferenceInput (Script.Versioned (Script.Script script) _) Nothing = return $ Cardano.PScript $ Cardano.PlutusScriptSerialised script
toPlutusScriptOrReferenceInput (Script.toScriptHash -> scriptHash) (Just scriptOutRef) = do
  (preview (txSkelOutReferenceScriptL % txSkelOutReferenceScriptHashAF) -> mScriptHash) <- txSkelOutByRef scriptOutRef
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
toScriptWitness :: (MonadBlockChainBalancing m, ToVScript a) => a -> TxSkelRedeemer -> Cardano.ScriptDatum b -> m (Cardano.ScriptWitness b Cardano.ConwayEra)
toScriptWitness (toVScript -> script@(Script.Versioned _ version)) (TxSkelRedeemer {..}) datum = do
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
