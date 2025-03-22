-- | This module provides a function to ensure that each redeemer used in a
-- skeleton is attached a reference input with the right reference script when
-- it exists in the index.
module Cooked.MockChain.AutoReferenceScripts (toTxSkelWithReferenceScripts) where

import Control.Monad
import Cooked.MockChain.BlockChain
import Cooked.MockChain.UtxoSearch
import Cooked.Output
import Cooked.Skeleton
import Data.Map qualified as Map
import Data.Maybe
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Searches through the known utxos for a utxo containing a reference script
-- with a given script hash, and returns the first such utxo found, if any.
retrieveReferenceScript :: (MonadBlockChain m, Script.ToScriptHash s) => s -> m (Maybe Api.TxOutRef)
retrieveReferenceScript = (listToMaybe . (fst <$>) <$>) . runUtxoSearch . referenceScriptOutputsSearch

-- | Attempts to find in the index a utxo containing a reference script with the
-- given script hash, and attaches it to a redeemer when it does not yet have a
-- reference input, in which case an event is logged.
updateRedeemer :: (MonadBlockChain m, Script.ToScriptHash s) => s -> TxSkelRedeemer -> m TxSkelRedeemer
updateRedeemer script txSkelRed@(TxSkelRedeemer red Nothing) = do
  oRefM <- retrieveReferenceScript script
  case oRefM of
    Nothing -> return txSkelRed
    Just oRef -> do
      logEvent $ MCLogAddedReferenceScript red oRef (Script.toScriptHash script)
      return $ TxSkelRedeemer red $ Just oRef
updateRedeemer _ redeemer = return redeemer

-- | Goes through the various parts of the skeleton where a redeemer can appear,
-- and attempt to attach a reference input to each of them, following the rules
-- from `updateRedeemer`
toTxSkelWithReferenceScripts :: (MonadBlockChain m) => TxSkel -> m TxSkel
toTxSkelWithReferenceScripts txSkel = do
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
  newWithdrawals <- forM (Map.toList $ txSkel ^. txSkelWithdrawalsL) $ \(wit, (red, quantity)) -> case wit of
    Right _ -> return (wit, (red, quantity))
    Left script -> (Left script,) . (,quantity) <$> updateRedeemer script red
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
