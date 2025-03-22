module Cooked.MockChain.GenerateTx.Input (toTxInAndWitness) where

import Cardano.Api qualified as Cardano
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Common
import Cooked.MockChain.GenerateTx.Witness
import Cooked.Skeleton
import Ledger.Tx.CardanoAPI qualified as Ledger
import PlutusLedgerApi.V3 qualified as Api

-- | Converts a 'TxSkel' input, which consists of a 'Api.TxOutRef' and a
-- 'TxSkelIn', into a 'Cardano.TxIn', together with the appropriate witness.
toTxInAndWitness ::
  (MonadBlockChainBalancing m) =>
  (Api.TxOutRef, TxSkelRedeemer) ->
  m (Cardano.TxIn, Cardano.BuildTxWith Cardano.BuildTx (Cardano.Witness Cardano.WitCtxTxIn Cardano.ConwayEra))
toTxInAndWitness (txOutRef, txSkelRedeemer) = do
  Api.TxOut (Api.Address cred _) _ datum _ <- throwOnMaybe "toCollateralTriplet: unresolved txOutRefs" =<< txOutByRef txOutRef
  witness <- case cred of
    Api.PubKeyCredential _ -> return $ Cardano.KeyWitness Cardano.KeyWitnessForSpending
    Api.ScriptCredential scriptHash -> do
      validator <- throwOnMaybe "toTxInAndWitness: Unknown validator" =<< scriptFromHash scriptHash
      scriptDatum <- case datum of
        Api.NoOutputDatum -> return $ Cardano.ScriptDatumForTxIn Nothing
        Api.OutputDatum _ -> return Cardano.InlineScriptDatum
        Api.OutputDatumHash datumHash -> do
          sDatum <- throwOnMaybe "toTxInAndWitness: Unknown datum hash" =<< datumFromHash datumHash
          return $ Cardano.ScriptDatumForTxIn $ Just $ Ledger.toCardanoScriptData $ Api.getDatum sDatum
      Cardano.ScriptWitness Cardano.ScriptWitnessForSpending <$> toScriptWitness validator txSkelRedeemer scriptDatum
  throwOnToCardanoErrorOrApply
    "toTxInAndWitness: Unable to translate TxOutRef"
    (,Cardano.BuildTxWith witness)
    (Ledger.toCardanoTxIn txOutRef)
