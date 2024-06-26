module Cooked.MockChain.GenerateTx.Input
  ( toTxInAndWitness,
    InputContext (..),
  )
where

import Cardano.Api qualified as Cardano
import Control.Monad.Reader
import Cooked.MockChain.GenerateTx.Common
import Cooked.MockChain.GenerateTx.Witness
import Cooked.Skeleton
import Data.Map (Map)
import Ledger.Tx.CardanoAPI qualified as Ledger
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

data InputContext where
  InputContext ::
    { managedData :: Map Api.DatumHash Api.Datum,
      managedTxOuts :: Map Api.TxOutRef Api.TxOut,
      managedValidators :: Map Script.ValidatorHash (Script.Versioned Script.Validator)
    } ->
    InputContext

instance Transform InputContext (Map Api.TxOutRef Api.TxOut) where
  transform = managedTxOuts

type InputGen a = TxGen InputContext a

-- | Convert a 'TxSkel' input, which consists of a 'Api.TxOutRef' and a
-- 'TxSkelIn', into a 'Cardano.TxIn', together with the appropriate witness.
toTxInAndWitness ::
  (Api.TxOutRef, TxSkelRedeemer) ->
  InputGen (Cardano.TxIn, Cardano.BuildTxWith Cardano.BuildTx (Cardano.Witness Cardano.WitCtxTxIn Cardano.ConwayEra))
toTxInAndWitness (txOutRef, txSkelRedeemer) = do
  Api.TxOut (Api.Address cred _) _ datum _ <- throwOnLookup "toTxInAndWitness: Unknown txOutRef" txOutRef =<< asks managedTxOuts
  witness <- case cred of
    Api.PubKeyCredential _ -> return $ Cardano.KeyWitness Cardano.KeyWitnessForSpending
    Api.ScriptCredential (Api.ScriptHash scriptHash) -> do
      validator <- throwOnLookup "toTxInAndWitness: Unknown validator" (Script.ValidatorHash scriptHash) =<< asks managedValidators
      scriptDatum <- case datum of
        Api.NoOutputDatum -> throwOnString "toTxInAndWitness: No datum found on script output"
        Api.OutputDatum _ -> return Cardano.InlineScriptDatum
        Api.OutputDatumHash datumHash -> do
          sDatum <- throwOnLookup "toTxInAndWitness: Unknown datum hash" datumHash =<< asks managedData
          return $ Cardano.ScriptDatumForTxIn $ Ledger.toCardanoScriptData $ Api.getDatum sDatum
      Cardano.ScriptWitness Cardano.ScriptWitnessForSpending <$> liftTxGen (toScriptWitness validator txSkelRedeemer scriptDatum)
  throwOnToCardanoErrorOrApply
    "toTxInAndWitness: Unable to translate TxOutRef"
    (,Cardano.BuildTxWith witness)
    $ Ledger.toCardanoTxIn txOutRef
