-- | This modules exposes the generation of transaction outputs
module Cooked.MockChain.GenerateTx.Output (toCardanoTxOut) where

import Cardano.Api qualified as Cardano
import Cardano.Node.Emulator.Internal.Node.Params qualified as Emulator
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Common
import Cooked.Skeleton
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core
import Plutus.Script.Utils.Data qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Converts a 'TxSkelOut' to the corresponding 'Cardano.TxOut'
toCardanoTxOut :: (MonadBlockChainBalancing m) => TxSkelOut -> m (Cardano.TxOut Cardano.CtxTx Cardano.ConwayEra)
toCardanoTxOut output = do
  let oAddress = txSkelOutAddress output
      oValue = txSkelOutValue output
      oDatum = output ^. txSkelOutDatumL
      oRefScript = txSkelOutReferenceScript output
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
    TxSkelOutSomeDatum datum (Hashed NotResolved) ->
      Cardano.TxOutDatumHash Cardano.AlonzoEraOnwardsConway
        <$> throwOnToCardanoError
          "toCardanoTxOut: Unable to resolve/transate a datum hash."
          (Ledger.toCardanoScriptDataHash $ Script.datumHash $ Api.Datum $ Api.toBuiltinData datum)
    TxSkelOutSomeDatum datum (Hashed Resolved) -> return $ Cardano.TxOutSupplementalDatum Cardano.AlonzoEraOnwardsConway $ Ledger.toCardanoScriptData $ Api.toBuiltinData datum
    TxSkelOutSomeDatum datum Inline -> return $ Cardano.TxOutDatumInline Cardano.BabbageEraOnwardsConway $ Ledger.toCardanoScriptData $ Api.toBuiltinData datum
  return $ Cardano.TxOut address value datum $ Ledger.toCardanoReferenceScript (Script.toVersioned <$> oRefScript)
