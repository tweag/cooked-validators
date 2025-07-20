-- | This module allows the generation of Cardano reference inputs
module Cooked.MockChain.GenerateTx.ReferenceInputs (toInsReference) where

import Cardano.Api qualified as Cardano
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx.Common
import Cooked.Skeleton
import Data.Map qualified as Map
import Data.Set qualified as Set
import Ledger.Tx.CardanoAPI qualified as Ledger
import PlutusLedgerApi.V3 qualified as Api

-- | Takes a 'TxSkel' and generates the associated 'Cardano.TxInsReference' from
-- its content. These reference inputs can be found in two places, either in
-- direct reference inputs 'txSkelInsReference' or scattered in the various
-- redeemers of the transaction, which can be gathered with
-- 'txSkelInsReferenceInRedeemers'.
toInsReference :: (MonadBlockChainBalancing m) => TxSkel -> m (Cardano.TxInsReference Cardano.BuildTx Cardano.ConwayEra)
toInsReference skel = do
  -- As regular inputs can be used to hold scripts as if in reference inputs, we
  -- need to remove from the reference inputs stored in redeemers the ones that
  -- already appear in the inputs to avoid validation errors.
  let indirectReferenceInputs = txSkelInsReferenceInRedeemers skel
      redundantReferenceInputs = indirectReferenceInputs `Set.intersection` Map.keysSet (txSkelIns skel)
      refInputs = Set.toList (txSkelInsReference skel <> indirectReferenceInputs `Set.difference` redundantReferenceInputs)
  if null refInputs
    then return Cardano.TxInsReferenceNone
    else do
      cardanoRefInputs <-
        throwOnToCardanoError
          "toInsReference: Unable to translate reference inputs."
          (mapM Ledger.toCardanoTxIn refInputs)
      resolvedDatums <- mapM (unsafeViewByRef txSkelOutDatumL) refInputs
      return $
        Cardano.TxInsReference Cardano.BabbageEraOnwardsConway cardanoRefInputs $
          Cardano.BuildTxWith $
            Set.fromList
              [Ledger.toCardanoScriptData $ Api.toBuiltinData dat | SomeTxSkelOutDatum dat (Hashed _) <- resolvedDatums]
