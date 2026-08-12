-- | This module exposes the generation of transaction inputs
module Cooked.Automation.GenerateTx.Input (toTxInAndWitness) where

import Cardano.Api qualified as Cardano
import Cooked.Automation.GenerateTx.Witness
import Cooked.Effect.Query
import Cooked.Runtime.Error
import Cooked.Skeleton
import Ledger.Tx.CardanoAPI qualified as P.Ledger
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error

-- | Converts a 'TxSkel' input, which consists of a 'Api.TxOutRef' and a
-- 'TxSkelRedeemer', into a 'Cardano.TxIn', together with the appropriate witness.
toTxInAndWitness ::
  (Members '[Query, Error MockChainError, Error P.Ledger.ToCardanoError] effs) =>
  (Api.TxOutRef, TxSkelRedeemer) ->
  Sem
    effs
    ( Cardano.TxIn,
      Cardano.BuildTxWith Cardano.BuildTx (Cardano.Witness Cardano.WitCtxTxIn Cardano.ConwayEra)
    )
toTxInAndWitness (txOutRef, txSkelRedeemer) = do
  TxSkelOut {txSkelOutOwner, txSkelOutDatum} <- txSkelOutByRef txOutRef
  let toScriptDatum = case txSkelOutDatum of
        NoTxSkelOutDatum -> return $ Cardano.ScriptDatumForTxIn Nothing
        SomeTxSkelOutDatum _ Inline -> return Cardano.InlineScriptDatum
        SomeTxSkelOutDatum dat _ -> return $ Cardano.ScriptDatumForTxIn $ Just $ P.Ledger.toCardanoScriptData $ Api.toBuiltinData dat
        SomeTxSkelOutDatumHash hash -> throw $ MCESpendingHashOnlyDatum txOutRef hash
  witness <- case txSkelOutOwner of
    UserPubKey _ -> return $ Cardano.KeyWitness Cardano.KeyWitnessForSpending
    UserScript script -> do
      scriptDatum <- toScriptDatum
      Cardano.ScriptWitness Cardano.ScriptWitnessForSpending <$> toScriptWitness script txSkelRedeemer scriptDatum
    UserScriptHash sHash -> do
      scriptDatum <- toScriptDatum
      -- The full script is not available in the owner, so it must be recovered
      -- from the reference script of the redeemer's reference input.
      mVScript <- case txSkelRedeemerReferenceInput txSkelRedeemer of
        Nothing -> return Nothing
        Just refOutRef -> preview txSkelOutReferenceScriptAT <$> txSkelOutByRef refOutRef
      case mVScript of
        Just vScript
          | Script.toScriptHash vScript == sHash ->
              Cardano.ScriptWitness Cardano.ScriptWitnessForSpending <$> toScriptWitness vScript txSkelRedeemer scriptDatum
        _ -> throw $ MCESpendingHashOnlyScript txOutRef sHash
  (,Cardano.BuildTxWith witness) <$> fromEither (P.Ledger.toCardanoTxIn txOutRef)
