-- | This modules exposes the generation of transaction outputs
module Cooked.MockChain.GenerateTx.Output (toCardanoTxOut) where

import Cardano.Api qualified as Cardano
import Cardano.Node.Emulator.Internal.Node.Params qualified as Emulator
import Cooked.MockChain.Read
import Cooked.Skeleton.Datum
import Cooked.Skeleton.Output
import Ledger.Tx.CardanoAPI qualified as P.Ledger
import Optics.Core
import Plutus.Script.Utils.Data qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error

-- | Converts a 'TxSkelOut' to the corresponding 'Cardano.TxOut'
toCardanoTxOut ::
  (Members '[MockChainRead, Error P.Ledger.ToCardanoError] effs) =>
  TxSkelOut ->
  Sem effs (Cardano.TxOut Cardano.CtxTx Cardano.ConwayEra)
toCardanoTxOut output = do
  let oAddress = view txSkelOutAddressG output
      oValue = view txSkelOutValueL output
      oDatum = view txSkelOutDatumL output
      oRefScript = view txSkelOutMReferenceScriptL output
  networkId <- Emulator.pNetworkId <$> getParams
  address <- fromEither $ P.Ledger.toCardanoAddressInEra networkId oAddress
  (P.Ledger.toCardanoTxOutValue -> value) <- fromEither $ P.Ledger.toCardanoValue oValue
  datum <- case oDatum of
    NoTxSkelOutDatum -> return Cardano.TxOutDatumNone
    SomeTxSkelOutDatum datum (Hashed NotResolved) ->
      Cardano.TxOutDatumHash Cardano.AlonzoEraOnwardsConway
        <$> fromEither (P.Ledger.toCardanoScriptDataHash $ Script.datumHash $ Api.Datum $ Api.toBuiltinData datum)
    SomeTxSkelOutDatum datum (Hashed Resolved) ->
      return $
        Cardano.TxOutSupplementalDatum Cardano.AlonzoEraOnwardsConway $
          P.Ledger.toCardanoScriptData $
            Api.toBuiltinData datum
    SomeTxSkelOutDatum datum Inline ->
      return $
        Cardano.TxOutDatumInline Cardano.BabbageEraOnwardsConway $
          P.Ledger.toCardanoScriptData $
            Api.toBuiltinData datum
  return $ Cardano.TxOut address value datum $ P.Ledger.toCardanoReferenceScript oRefScript
