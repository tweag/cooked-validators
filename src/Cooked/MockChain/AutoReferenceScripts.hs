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
updateRedeemedScript :: (MonadBlockChain m) => [Api.TxOutRef] -> User IsScript Redemption -> m (User IsScript Redemption)
updateRedeemedScript inputs rs@(UserRedeemedScript (toVScript -> vScript) txSkelRed@(TxSkelRedeemer {txSkelRedeemerAutoFill = True})) = do
  oRefsInInputs <- runUtxoSearch (referenceScriptOutputsSearch vScript)
  maybe
    -- We leave the redeemer unchanged if no reference input was found
    (return rs)
    -- If a reference input is found, we assign it and log the event
    ( \oRef -> do
        logEvent $ MCLogAddedReferenceScript txSkelRed oRef (Script.toScriptHash vScript)
        return $ over userTxSkelRedeemerAT (autoFillReferenceInput oRef) rs
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
      validatorM <- previewByRef (txSkelOutOwnerL % userVScriptAT) oRef
      case validatorM of
        Nothing -> return red
        Just val -> view userTxSkelRedeemerL <$> updateRedeemedScript inputs (UserRedeemedScript val red)
  newProposals <- forM (view txSkelProposalsL txSkel) $ \prop ->
    case preview (txSkelProposalMConstitutionAT % _Just) prop of
      Nothing -> return prop
      Just rs -> flip (set (txSkelProposalMConstitutionAT % _Just)) prop <$> updateRedeemedScript inputs rs
  newWithdrawals <- forM (toListOf (txSkelWithdrawalsL % to Map.toList % traversed) txSkel) $
    \(hash, (user, lv)) -> case preview userEitherScriptP user of
      Nothing -> return (hash, (user, lv))
      Just urs -> (hash,) . (,lv) . review userEitherScriptP <$> updateRedeemedScript inputs urs
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
