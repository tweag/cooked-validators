-- | This module provides a function to ensure that each redeemer used in a
-- skeleton is attached a reference input with the right reference script when
-- it exists in the index.
module Cooked.MockChain.AutoReferenceScripts (toTxSkelWithReferenceScripts) where

import Control.Monad
import Cooked.MockChain.BlockChain
import Cooked.MockChain.UtxoSearch
import Cooked.Skeleton
import Data.List (find)
import Data.Map qualified as Map
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Attempts to find in the index a utxo containing a reference script with the
-- given script hash, and attaches it to a redeemer when it does not yet have a
-- reference input and when it is allowed, in which case an event is logged.
updateRedeemedScript :: (MonadBlockChain m) => [Api.TxOutRef] -> RedeemedScript -> m RedeemedScript
updateRedeemedScript inputs rs@(RedeemedScript (Script.toVersioned @Script.Script -> script) txSkelRed@(TxSkelRedeemer _ Nothing True)) = do
  oRefsInInputs <- runUtxoSearch (referenceScriptOutputsSearch script)
  maybe
    -- We leave the redeemer unchanged if no reference input was found
    (return rs)
    -- If a reference input is found, we assign it and log the event
    ( \oRef -> do
        logEvent $ MCLogAddedReferenceScript txSkelRed oRef (Script.toScriptHash script)
        return $ over redeemedScriptRedeemerL (`withReferenceInput` oRef) rs
    )
    $ case oRefsInInputs of
      [] -> Nothing
      -- If possible, we use a reference input appearing in regular inputs
      l | Just (oRefM', _) <- find (\(r, _) -> r `elem` inputs) l -> Just oRefM'
      -- If none exist, we use the first one we find elsewhere
      ((oRefM', _) : _) -> Just oRefM'
updateRedeemedScript _ rs = return rs

-- | Goes through the various parts of the skeleton where a redeemer can appear,
-- and attempts to attach a reference input to each of them, whenever it is
-- allowed and one has not already been set.
toTxSkelWithReferenceScripts :: forall m. (MonadBlockChain m) => TxSkel -> m TxSkel
toTxSkelWithReferenceScripts txSkel = do
  let inputs = view (txSkelInsL % to Map.keys) txSkel
  newMints <- forM (view (txSkelMintsL % txSkelMintsListI) txSkel) $ \(Mint rs tks) ->
    (`Mint` tks) <$> updateRedeemedScript inputs rs
  newInputs <- forM (view (txSkelInsL % to Map.toList) txSkel) $ \(oRef, red) ->
    (oRef,) <$> do
      validatorM <- previewByRef txSkelOutValidatorAT oRef
      case validatorM of
        Nothing -> return red
        Just val -> redeemedScriptRedeemer <$> updateRedeemedScript inputs (RedeemedScript val red)
  newProposals <- forM (view txSkelProposalsL txSkel) $ \prop ->
    case preview txSkelProposalRedeemedScriptAT prop of
      Nothing -> return prop
      Just rs -> flip (set txSkelProposalRedeemedScriptAT) prop <$> updateRedeemedScript inputs rs
  newWithdrawals <- forM (view (txSkelWithdrawalsL % to Map.toList) txSkel) $ \entry@(wit, (red, _)) -> case wit of
    Right _ -> return entry
    Left script -> flip (set (_2 % _1)) entry . redeemedScriptRedeemer <$> updateRedeemedScript inputs (RedeemedScript script red)
  return $
    txSkel
      & txSkelMintsL
      % txSkelMintsListI
      .~ newMints
      & txSkelInsL
      .~ Map.fromList newInputs
      & txSkelProposalsL
      .~ newProposals
      & txSkelWithdrawalsL
      .~ Map.fromList newWithdrawals
