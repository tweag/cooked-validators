module Cooked.MockChain.GenerateTx.Output (toCardanoTxOut) where

import Cardano.Api.Shelley qualified as Cardano
import Cardano.Node.Emulator.Internal.Node.Params qualified as Emulator
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Common
import Cooked.Output
import Cooked.Skeleton
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Converts a 'TxSkelOut' to the corresponding 'Cardano.TxOut'
toCardanoTxOut :: (MonadBlockChainBalancing m) => TxSkelOut -> m (Cardano.TxOut Cardano.CtxTx Cardano.ConwayEra)
toCardanoTxOut (Pays output) = do
  let oAddress = outputAddress output
      oValue = outputValue output
      oDatum = output ^. outputDatumL
      oRefScript = output ^. outputReferenceScriptL
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
    TxSkelOutNoDatum -> return Cardano.TxOutDatumNone
    TxSkelOutDatumHash datum ->
      throwOnToCardanoError
        "toCardanoTxOut: Unable to resolve/transate a datum hash."
        $ Cardano.TxOutDatumHash Cardano.AlonzoEraOnwardsConway
          <$> Ledger.toCardanoScriptDataHash (Script.datumHash $ Api.Datum $ Api.toBuiltinData datum)
    TxSkelOutDatum datum -> return $ Cardano.TxOutSupplementalDatum Cardano.AlonzoEraOnwardsConway $ Ledger.toCardanoScriptData $ Api.toBuiltinData datum
    TxSkelOutInlineDatum datum -> return $ Cardano.TxOutDatumInline Cardano.BabbageEraOnwardsConway $ Ledger.toCardanoScriptData $ Api.toBuiltinData datum
  return $ Cardano.TxOut address value datum $ Ledger.toCardanoReferenceScript (Script.toVersioned <$> oRefScript)
