-- | This modules exposes the generation of transaction outputs
module Cooked.MockChain.GenerateTx.Output (toCardanoTxOut) where

import Cardano.Api.Shelley qualified as Cardano
import Cardano.Node.Emulator.Internal.Node.Params qualified as Emulator
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Common
import Cooked.Skeleton
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core
import Plutus.Script.Utils.Data qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Converts a 'TxSkelOut' to the corresponding 'Cardano.TxOut'
toCardanoTxOut :: (MonadBlockChainBalancing m) => TxSkelOut -> m (Cardano.TxOut Cardano.CtxTx Cardano.ConwayEra)
toCardanoTxOut output = do
  let oAddress = view txSkelOutAddressG output
      oValue = view txSkelOutValueL output
      oDatum = output ^. txSkelOutDatumL
      oRefScript = preview (txSkelOutReferenceScriptL % txSkelOutReferenceScriptVersionedP) output
  networkId <- Emulator.pNetworkId <$> getParams
  address <-
    throwOnToCardanoError
      ("toCardanoTxOut: Unable to translate the following address: " <> show oAddress)
      (Ledger.toCardanoAddressInEra networkId oAddress)
  (Ledger.toCardanoTxOutValue -> value) <-
    throwOnToCardanoError
      ("toCardanoTxOut: Unable to translate the following value:" <> show oValue)
      (Ledger.toCardanoValue oValue)
  datum <- case oDatum of
    NoTxSkelOutDatum -> return Cardano.TxOutDatumNone
    SomeTxSkelOutDatum datum (Hashed NotResolved) ->
      Cardano.TxOutDatumHash Cardano.AlonzoEraOnwardsConway
        <$> throwOnToCardanoError
          "toCardanoTxOut: Unable to resolve/transate a datum hash."
          (Ledger.toCardanoScriptDataHash $ Script.datumHash $ Api.Datum $ Api.toBuiltinData datum)
    SomeTxSkelOutDatum datum (Hashed Resolved) -> return $ Cardano.TxOutSupplementalDatum Cardano.AlonzoEraOnwardsConway $ Ledger.toCardanoScriptData $ Api.toBuiltinData datum
    SomeTxSkelOutDatum datum Inline -> return $ Cardano.TxOutDatumInline Cardano.BabbageEraOnwardsConway $ Ledger.toCardanoScriptData $ Api.toBuiltinData datum
  return $ Cardano.TxOut address value datum $ Ledger.toCardanoReferenceScript oRefScript
