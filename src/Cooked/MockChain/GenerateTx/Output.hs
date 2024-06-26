module Cooked.MockChain.GenerateTx.Output
  ( toCardanoTxOut,
  )
where

import Cardano.Api.Shelley qualified as Cardano
import Control.Monad.Reader
import Cooked.Conversion
import Cooked.MockChain.GenerateTx.Common
import Cooked.Output
import Cooked.Skeleton
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

type OutputGen a = TxGen Cardano.NetworkId a

toHashableScriptData :: (Api.ToData a) => a -> Cardano.HashableScriptData
toHashableScriptData = Cardano.unsafeHashableScriptData . Cardano.fromPlutusData . Api.builtinDataToData . Api.toBuiltinData

-- | Converts a 'TxSkelOut' to the corresponding 'Cardano.TxOut'
toCardanoTxOut :: TxSkelOut -> OutputGen (Cardano.TxOut Cardano.CtxTx Cardano.ConwayEra)
toCardanoTxOut (Pays output) = do
  let oAddress = outputAddress output
      oValue = outputValue output
      oDatum = output ^. outputDatumL
      oRefScript = output ^. outputReferenceScriptL
  networkId <- ask
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
    TxSkelOutDatum datum -> return $ Cardano.TxOutDatumInTx Cardano.AlonzoEraOnwardsConway $ toHashableScriptData datum
    TxSkelOutInlineDatum datum -> return $ Cardano.TxOutDatumInline Cardano.BabbageEraOnwardsConway $ toHashableScriptData datum
  return $ Cardano.TxOut address value datum $ Ledger.toCardanoReferenceScript (toScript <$> oRefScript)
