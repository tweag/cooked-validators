-- | This module exposes the generation of transaction inputs
module Cooked.MockChain.GenerateTx.Input (toTxInAndWitness) where

import Cardano.Api qualified as Cardano
import Cooked.MockChain.Error
import Cooked.MockChain.GenerateTx.Witness
import Cooked.MockChain.Read
import Cooked.Skeleton
import Ledger.Tx.CardanoAPI qualified as Ledger
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error

-- | Converts a 'TxSkel' input, which consists of a 'Api.TxOutRef' and a
-- 'TxSkelRedeemer', into a 'Cardano.TxIn', together with the appropriate witness.
toTxInAndWitness ::
  (Members '[MockChainRead, Error MockChainError, Error Ledger.ToCardanoError] effs) =>
  (Api.TxOutRef, TxSkelRedeemer) ->
  Sem
    effs
    ( Cardano.TxIn,
      Cardano.BuildTxWith Cardano.BuildTx (Cardano.Witness Cardano.WitCtxTxIn Cardano.ConwayEra)
    )
toTxInAndWitness (txOutRef, txSkelRedeemer) = do
  TxSkelOut owner _ datum _ _ _ <- txSkelOutByRef txOutRef
  witness <- case owner of
    UserPubKey _ -> return $ Cardano.KeyWitness Cardano.KeyWitnessForSpending
    UserScript script ->
      fmap (Cardano.ScriptWitness Cardano.ScriptWitnessForSpending) $
        toScriptWitness script txSkelRedeemer $
          case datum of
            NoTxSkelOutDatum -> Cardano.ScriptDatumForTxIn Nothing
            SomeTxSkelOutDatum _ Inline -> Cardano.InlineScriptDatum
            SomeTxSkelOutDatum dat _ -> Cardano.ScriptDatumForTxIn $ Just $ Ledger.toCardanoScriptData $ Api.toBuiltinData dat
  (,Cardano.BuildTxWith witness) <$> fromEither (Ledger.toCardanoTxIn txOutRef)
