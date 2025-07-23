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
updateRedeemer :: (MonadBlockChain m, Script.ToScriptHash s) => s -> [Api.TxOutRef] -> TxSkelRedeemer -> m TxSkelRedeemer
updateRedeemer script inputs txSkelRed@(TxSkelRedeemer _ Nothing True) = do
  oRefsInInputs <- runUtxoSearch (referenceScriptOutputsSearch script)
  maybe
    -- We leave the redeemer unchanged if no reference input was found
    (return txSkelRed)
    -- If a reference input is found, we assign it and log the event
    ( \oRef -> do
        logEvent $ MCLogAddedReferenceScript txSkelRed oRef (Script.toScriptHash script)
        return $ txSkelRed `withReferenceInput` oRef
    )
    $ case oRefsInInputs of
      [] -> Nothing
      -- If possible, we use a reference input appearing in regular inputs
      l | Just (oRefM', _) <- find (\(r, _) -> r `elem` inputs) l -> Just oRefM'
      -- If none exist, we use the first one we find elsewhere
      ((oRefM', _) : _) -> Just oRefM'
updateRedeemer _ _ redeemer = return redeemer

-- | Goes through the various parts of the skeleton where a redeemer can appear,
-- and attempts to attach a reference input to each of them, whenever it is
-- allowed and one has not already been set.
toTxSkelWithReferenceScripts :: (MonadBlockChain m) => TxSkel -> m TxSkel
toTxSkelWithReferenceScripts txSkel@TxSkel {..} = do
  let inputs = Map.keys txSkelIns
  newMints <- forM (view txSkelMintsListI txSkelMints) $ \(Mint mPol red tks) ->
    (\x -> Mint mPol x tks) <$> updateRedeemer (Script.toVersioned @Script.MintingPolicy mPol) inputs red
  newInputs <- forM (Map.toList txSkelIns) $ \(oRef, red) -> do
    validatorM <- previewByRef txSkelOutValidatorAT oRef
    case validatorM of
      Nothing -> return (oRef, red)
      Just scriptHash -> (oRef,) <$> updateRedeemer scriptHash inputs red
  newProposals <- forM txSkelProposals $ \prop ->
    case preview txSkelProposalConstitutionRedeemerAT prop of
      Nothing -> return prop
      Just (script, red) -> flip (set txSkelProposalConstitutionRedeemerAT) prop . (script,) <$> updateRedeemer script inputs red
  newWithdrawals <- forM (Map.toList txSkelWithdrawals) $ \(wit, (red, quantity)) -> case wit of
    Right _ -> return (wit, (red, quantity))
    Left script -> (Left script,) . (,quantity) <$> updateRedeemer script inputs red
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
