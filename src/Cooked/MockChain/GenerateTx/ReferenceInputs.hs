module Cooked.MockChain.GenerateTx.ReferenceInputs (toInsReference) where

import Cardano.Api qualified as Cardano
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Common
import Cooked.Skeleton.Datum
import Data.Set qualified as Set
import Ledger.Tx.CardanoAPI qualified as Ledger
import PlutusLedgerApi.V3 qualified as Api

toInsReference :: (MonadBlockChainBalancing m) => [Api.TxOutRef] -> m (Cardano.TxInsReference Cardano.BuildTx Cardano.ConwayEra)
toInsReference refInputs | null refInputs = return Cardano.TxInsReferenceNone
toInsReference refInputs = do
  resolvedOutputs <- mapM unsafeDatumFromTxOutRef refInputs
  let additionalDatums = [Ledger.toCardanoScriptData $ Api.toBuiltinData dat | TxSkelOutSomeDatum dat (Hashed _) <- resolvedOutputs]
  throwOnToCardanoErrorOrApply
    "toInsReference: Unable to translate reference inputs."
    (flip (Cardano.TxInsReference Cardano.BabbageEraOnwardsConway) (Cardano.BuildTxWith $ Set.fromList additionalDatums))
    (mapM Ledger.toCardanoTxIn refInputs)
