-- | This module exposes the generation of key and script witnesses
module Cooked.MockChain.GenerateTx.Witness
  ( toScriptWitness,
    toKeyWitness,
  )
where

import Cardano.Api qualified as Cardano
import Cooked.MockChain.Error
import Cooked.MockChain.Read
import Cooked.Skeleton
import Ledger.Address qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error

-- | Translates a script and a reference script utxo into either a plutus script
-- or a reference input containing the right script
toPlutusScriptOrReferenceInput ::
  (Members '[MockChainRead, Error MockChainError, Error Ledger.ToCardanoError] effs) =>
  VScript ->
  Maybe Api.TxOutRef ->
  Sem effs (Cardano.PlutusScriptOrReferenceInput lang)
toPlutusScriptOrReferenceInput (Script.Versioned (Script.Script script) _) Nothing =
  return $ Cardano.PScript $ Cardano.PlutusScriptSerialised script
toPlutusScriptOrReferenceInput (Script.toScriptHash -> scriptHash) (Just scriptOutRef) = do
  (preview txSkelOutReferenceScriptHashAF -> mScriptHash) <- txSkelOutByRef scriptOutRef
  case mScriptHash of
    Just scriptHash'
      | scriptHash == scriptHash' -> do
          s <- fromEither $ Ledger.toCardanoTxIn scriptOutRef
          return $ Cardano.PReferenceScript s
    _ -> throw $ MCEWrongReferenceScriptError scriptOutRef scriptHash mScriptHash

-- | Translates a script with its associated redeemer and datum to a script
-- witness. Note on the usage of 'Ledger.zeroExecutionUnits': at this stage of
-- the transaction create, we cannot know the execution units used by the
-- script. They will be filled out later on once the full body has been
-- generated. So, for now, we temporarily leave them to 0.
toScriptWitness ::
  ( Members '[MockChainRead, Error MockChainError, Error Ledger.ToCardanoError] effs,
    ToVScript a
  ) =>
  a ->
  TxSkelRedeemer ->
  Cardano.ScriptDatum b ->
  Sem effs (Cardano.ScriptWitness b Cardano.ConwayEra)
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

-- | Generates a key witnesses for a given signatory and body, when the
-- signatory contains a private key.
toKeyWitness ::
  Cardano.TxBody Cardano.ConwayEra ->
  TxSkelSignatory ->
  Maybe (Cardano.KeyWitness Cardano.ConwayEra)
toKeyWitness txBody =
  fmap
    ( Cardano.makeShelleyKeyWitness Cardano.ShelleyBasedEraConway txBody
        . Ledger.toWitness
        . Ledger.PaymentPrivateKey
    )
    . preview txSkelSignatoryPrivateKeyAT
