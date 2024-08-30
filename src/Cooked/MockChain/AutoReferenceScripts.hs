-- | This module provides a function to ensure that each redeemer used in a
-- skeleton is attached a reference scripts when a known utxos contains it.
module Cooked.MockChain.AutoReferenceScripts (setAllReferenceScripts) where

import Control.Monad
import Cooked.Conversion
import Cooked.MockChain.BlockChain
import Cooked.MockChain.UtxoSearch
import Cooked.Output
import Cooked.Skeleton
import Data.Map qualified as Map
import Data.Maybe
import Optics.Core
import PlutusLedgerApi.V3 qualified as Api

-- | Searches through the known utxos for a utxo containing a reference script
-- with a given script hash, and returns the first such utxo found, if any.
retrieveReferenceScript :: (MonadBlockChain m, ToScriptHash s) => s -> m (Maybe Api.TxOutRef)
retrieveReferenceScript script = listToMaybe . (fst <$>) <$> runUtxoSearch (referenceScriptOutputsSearch script)

-- | Attempts to attach a reference script to a redeemer when it can be found in
-- the context. Will not override any existing reference script.
updateRedeemer :: (MonadBlockChain m, ToScriptHash s) => s -> TxSkelRedeemer -> m TxSkelRedeemer
updateRedeemer script (TxSkelRedeemer red Nothing) = TxSkelRedeemer red <$> retrieveReferenceScript script
updateRedeemer _ redeemer = return redeemer

-- | Goes through the various parts of the skeleton where a redeemer can appear,
-- and attempt to attach a reference script to each of them, following the rules
-- from `updateRedeemer`
setAllReferenceScripts :: (MonadBlockChain m) => TxSkel -> m TxSkel
setAllReferenceScripts txSkel = do
  newMints <- forM (txSkelMintsToList $ txSkel ^. txSkelMintsL) $ \(mPol, red, tk, nb) ->
    (mPol,,tk,nb) <$> updateRedeemer mPol red
  newInputs <- forM (Map.toList $ txSkel ^. txSkelInsL) $ \(oRef, red) -> do
    outputM <- txOutByRef oRef
    -- We retrieve the possible script hash of the current oRef
    case (^. outputOwnerL) <$> (outputM >>= isScriptOutput) of
      -- Either the txOutRef is unknown, or it belongs to a private key
      Nothing -> return (oRef, red)
      Just scriptHash -> (oRef,) <$> updateRedeemer scriptHash red
  newProposals <- forM (txSkel ^. txSkelProposalsL) $ \prop ->
    case prop ^. txSkelProposalWitnessL of
      Nothing -> return prop
      Just (script, red) -> flip (set txSkelProposalWitnessL) prop . Just . (script,) <$> updateRedeemer script red
  newWithdrawals <- forM (Map.toList $ txSkel ^. txSkelWithdrawalsL) $ \(wit, quantity) -> case wit of
    Right _ -> return (wit, quantity)
    Left (script, red) -> (,quantity) . Left . (script,) <$> updateRedeemer script red
  return $
    txSkel
      & txSkelMintsL
      .~ txSkelMintsFromList newMints
      & txSkelInsL
      .~ Map.fromList newInputs
      & txSkelProposalsL
      .~ newProposals
      & txSkelWithdrawalsL
      .~ Map.fromList newWithdrawals
