module Cooked.MockChain.GenerateTx.Input
  ( toTxInAndWitness,
    InputContext (..),
  )
where

import Cardano.Api qualified as Cardano
import Control.Monad.Reader
import Cooked.Conversion
import Cooked.MockChain.GenerateTx.Common
import Cooked.MockChain.GenerateTx.Witness
import Cooked.Output
import Cooked.Skeleton
import Data.Map (Map)
import Data.Map qualified as Map
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
-- 'TxSkelIn', into a 'Cardano.TxIn', together with the appropriate witness. If
-- you add reference inputs, don't forget to also update the 'txInsReference'!
toTxInAndWitness ::
  (Api.TxOutRef, TxSkelRedeemer) ->
  InputGen (Cardano.TxIn, Cardano.BuildTxWith Cardano.BuildTx (Cardano.Witness Cardano.WitCtxTxIn Cardano.ConwayEra))
toTxInAndWitness (txOutRef, txSkelRedeemer) = do
  txOut <- asks (Map.lookup txOutRef . managedTxOuts)
  witness <- case toCredential . Api.txOutAddress <$> txOut of
    Just (Api.PubKeyCredential _) -> return $ Cardano.KeyWitness Cardano.KeyWitnessForSpending
    Just (Api.ScriptCredential _) -> do
      (script, datum) <- toScriptAndDatum txOutRef
      Cardano.ScriptWitness Cardano.ScriptWitnessForSpending <$> liftTxGen (toScriptWitness script txSkelRedeemer datum)
    Nothing -> throwOnString "toTxInAndWitness: unable to resolve input txOutRef"
  throwOnToCardanoErrorOrApply
    "txSkelIntoTxIn, translating TxOutRef"
    (,Cardano.BuildTxWith witness)
    $ Ledger.toCardanoTxIn txOutRef

-- | Takes a `TxOutRef` and returns its associated script, script hash
-- and translated datum when applicatble.
toScriptAndDatum ::
  Api.TxOutRef ->
  InputGen (Script.Versioned Script.Validator, Cardano.ScriptDatum Cardano.WitCtxTxIn)
toScriptAndDatum txOutRef = do
  txOut <- throwOnLookup "toTxInAndWitness: Unknown txOutRef" txOutRef =<< asks managedTxOuts
  validatorHash <-
    case outputAddress txOut of
      (Api.Address (Api.ScriptCredential (Api.ScriptHash validatorHash)) _) -> return $ Script.ValidatorHash validatorHash
      _ -> throwOnString $ "toTxInAndWitness: Output is not a script output" <> show txOut
  validator <- throwOnLookup "toTxInAndWitness: Unknown validator" validatorHash =<< asks managedValidators
  datum <-
    case outputOutputDatum txOut of
      Api.NoOutputDatum -> throwOnString "toTxInAndWitness: No datum found on script output"
      Api.OutputDatum _ -> return Cardano.InlineScriptDatum
      Api.OutputDatumHash datumHash -> do
        datum <- throwOnLookup "toTxInAndWitness: Datum hash could not be resolved" datumHash =<< asks managedData
        return $ Cardano.ScriptDatumForTxIn $ Ledger.toCardanoScriptData $ Api.getDatum datum
  return (validator, datum)
