-- | This module exposes the generation of transaction inputs
module Cooked.MockChain.GenerateTx.Input (toTxInAndWitness) where

import Cardano.Api qualified as Cardano
import Cooked.MockChain.Error
import Cooked.MockChain.GenerateTx.Witness
import Cooked.MockChain.Read
import Cooked.Skeleton
import Ledger.Tx.CardanoAPI qualified as P.Ledger
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error

-- | Converts a 'TxSkel' input, which consists of a 'Api.TxOutRef' and a
-- 'TxSkelRedeemer', into a 'Cardano.TxIn', together with the appropriate witness.
toTxInAndWitness ::
  (Members '[MockChainRead, Error MockChainError, Error P.Ledger.ToCardanoError] effs) =>
  (Api.TxOutRef, TxSkelRedeemer) ->
  Sem
    effs
    ( Cardano.TxIn,
      Cardano.BuildTxWith Cardano.BuildTx (Cardano.Witness Cardano.WitCtxTxIn Cardano.ConwayEra)
    )
toTxInAndWitness (txOutRef, txSkelRedeemer) = do
  TxSkelOut {txSkelOutOwner, txSkelOutDatum} <- txSkelOutByRef txOutRef
  witness <- case txSkelOutOwner of
    UserPubKey _ -> return $ Cardano.KeyWitness Cardano.KeyWitnessForSpending
    UserScript script -> do
      scriptDatum <- case txSkelOutDatum of
        NoTxSkelOutDatum -> return $ Cardano.ScriptDatumForTxIn Nothing
        SomeTxSkelOutDatum _ Inline -> return Cardano.InlineScriptDatum
        SomeTxSkelOutDatum dat _ -> return $ Cardano.ScriptDatumForTxIn $ Just $ P.Ledger.toCardanoScriptData $ Api.toBuiltinData dat
        SomeTxSkelOutDatumHash hash -> throw $ MCESpendingHashOnlyDatum txOutRef hash
      Cardano.ScriptWitness Cardano.ScriptWitnessForSpending <$> toScriptWitness script txSkelRedeemer scriptDatum
  (,Cardano.BuildTxWith witness) <$> fromEither (P.Ledger.toCardanoTxIn txOutRef)
