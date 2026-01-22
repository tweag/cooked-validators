-- | This module exposes functions to automatically fill parts of a
-- 'Cooked.Skeleton.TxSkel' based on the current state of the blockchain.
module Cooked.MockChain.AutoFilling where

import Cardano.Api qualified as Cardano
import Cardano.Ledger.Shelley.Core qualified as Shelley
import Cardano.Node.Emulator.Internal.Node.Params qualified as Emulator
import Control.Monad
import Cooked.MockChain.GenerateTx.Output
import Cooked.MockChain.Log
import Cooked.MockChain.Read
import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.List (find)
import Data.Map qualified as Map
import Data.Maybe
import Ledger.Tx qualified as Ledger
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error

-- * Auto filling withdrawal amounts

-- | Goes through all the withdrawals of the input skeleton and attempts to fill
-- out the withdrawn amount based on the associated user rewards. Does not
-- tamper with an existing specified amount in such withdrawals. Logs an event
-- when an amount has been successfully auto-filled.
autoFillWithdrawalAmounts ::
  (Members '[MockChainRead, Tweak, MockChainLog] effs) =>
  Sem effs ()
autoFillWithdrawalAmounts = do
  withdrawals <- viewTweak (txSkelWithdrawalsL % txSkelWithdrawalsListI)
  newWithdrawals <- forM withdrawals $ \withdrawal -> do
    currentReward <- getCurrentReward $ view withdrawalUserL withdrawal
    let (changed, newWithdrawal) = case currentReward of
          Nothing -> (False, withdrawal)
          Just reward -> (isn't withdrawalAmountAT withdrawal, fillAmount reward withdrawal)
    when changed $
      logEvent $
        MCLogAutoFilledWithdrawalAmount
          (view (withdrawalUserL % to Script.toCredential) newWithdrawal)
          (fromJust (preview withdrawalAmountAT newWithdrawal))
    return newWithdrawal
  setTweak (txSkelWithdrawalsL % txSkelWithdrawalsListI) newWithdrawals

-- * Auto filling constitution script

-- | Goes through all the proposals of the input skeleton and attempts to fill
-- out the constitution scripts with the current one. Does not tamper with an
-- existing specified script in such withdrawals. Logs an event when the
-- constitution script has been successfully auto-filled.
autoFillConstitution ::
  (Members '[MockChainRead, Tweak, MockChainLog] effs) =>
  Sem effs ()
autoFillConstitution = do
  currentConstitution <- getConstitutionScript
  case currentConstitution of
    Nothing -> return ()
    Just constitutionScript -> do
      proposals <- viewTweak txSkelProposalsL
      newProposals <- forM proposals $ \prop -> do
        when (isn't txSkelProposalConstitutionAT prop) $
          logEvent $
            MCLogAutoFilledConstitution $
              Script.toScriptHash constitutionScript
        return (fillConstitution constitutionScript prop)
      setTweak txSkelProposalsL newProposals

-- -- * Auto filling reference scripts

-- | Attempts to find in the index a utxo containing a reference script with the
-- given script hash, and attaches it to a redeemer when it does not yet have a
-- reference input and when it is allowed, in which case an event is logged.
updateRedeemedScript ::
  (Member MockChainLog effs) =>
  [Api.TxOutRef] ->
  User IsScript Redemption ->
  Sem effs (User IsScript Redemption)
updateRedeemedScript
  inputs
  rs@( UserRedeemedScript
         (toVScript -> vScript)
         txSkelRed@(TxSkelRedeemer {txSkelRedeemerAutoFill = True})
       ) = do
    oRefsInInputs <- undefined -- runUtxoSearch (referenceScriptOutputsSearch vScript)
    maybe
      -- We leave the redeemer unchanged if no reference input was found
      (return rs)
      -- If a reference input is found, we assign it and log the event
      ( \oRef -> do
          logEvent $ MCLogAddedReferenceScript txSkelRed oRef (Script.toScriptHash vScript)
          return $ over userTxSkelRedeemerAT (fillReferenceInput oRef) rs
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
-- allowed and one has not already been set. Logs an event whenever such an
-- addition occurs.
autoFillReferenceScripts ::
  (Members '[Tweak, MockChainRead, MockChainLog] effs) =>
  Sem effs ()
autoFillReferenceScripts = do
  inputsKeys <- viewTweak $ txSkelInsL % to Map.keys
  -- Updating minting redeemers
  mints <- viewTweak $ txSkelMintsL % txSkelMintsListI
  newMints <- forM mints $ \(Mint rs tks) -> (`Mint` tks) <$> updateRedeemedScript inputsKeys rs
  setTweak (txSkelMintsL % txSkelMintsListI) newMints
  -- Updating spending redeemers
  inputsList <- viewTweak $ txSkelInsL % to Map.toList
  newInputs <- forM inputsList $ \(oRef, red) ->
    (oRef,) <$> do
      validatorM <- previewByRef (txSkelOutOwnerL % userVScriptAT) oRef
      case validatorM of
        Nothing -> return red
        Just val -> view userTxSkelRedeemerL <$> updateRedeemedScript inputsKeys (UserRedeemedScript val red)
  setTweak txSkelInsL $ Map.fromList newInputs
  -- Updating proposing redeemers
  proposals <- viewTweak txSkelProposalsL
  newProposals <- forM proposals $ \prop ->
    case preview (txSkelProposalMConstitutionAT % _Just) prop of
      Nothing -> return prop
      Just rs -> flip (set (txSkelProposalMConstitutionAT % _Just)) prop <$> updateRedeemedScript inputsKeys rs
  setTweak txSkelProposalsL newProposals
  -- Updating widrawing redeemers
  withdrawals <- viewTweak $ txSkelWithdrawalsL % txSkelWithdrawalsListI
  newWithdrawals <- forM withdrawals $
    \withdrawal@(Withdrawal user lv) -> case preview userEitherScriptP user of
      Nothing -> return withdrawal
      Just urs -> (`Withdrawal` lv) . review userEitherScriptP <$> updateRedeemedScript inputsKeys urs
  setTweak (txSkelWithdrawalsL % txSkelWithdrawalsListI) newWithdrawals

-- * Auto filling min ada amounts

-- | Compute the required minimal ADA for a given output
getTxSkelOutMinAda ::
  (Members '[MockChainRead, Error Ledger.ToCardanoError] effs) =>
  TxSkelOut ->
  Sem effs Integer
getTxSkelOutMinAda txSkelOut = do
  params <- Emulator.pEmulatorPParams <$> getParams
  Cardano.unCoin
    . Shelley.getMinCoinTxOut params
    . Cardano.toShelleyTxOut Cardano.ShelleyBasedEraConway
    . Cardano.toCtxUTxOTxOut
    <$> toCardanoTxOut txSkelOut

-- | This transforms an output into another output which contains the minimal
-- required ada. If the previous quantity of ADA was sufficient, it remains
-- unchanged. This can require a few iterations to converge, as the added ADA
-- will increase the size of the UTXO which in turn might need more ADA.
toTxSkelOutWithMinAda ::
  forall effs.
  (Members '[MockChainRead, MockChainLog, Error Ledger.ToCardanoError] effs) =>
  TxSkelOut ->
  Sem effs TxSkelOut
-- The auto adjustment is disabled so nothing is done here
toTxSkelOutWithMinAda txSkelOut@((^. txSkelOutValueAutoAdjustL) -> False) = return txSkelOut
-- The auto adjustment is enabled
toTxSkelOutWithMinAda txSkelOut = do
  txSkelOut' <- go txSkelOut
  let originalAda = view (txSkelOutValueL % valueLovelaceL) txSkelOut
      updatedAda = view (txSkelOutValueL % valueLovelaceL) txSkelOut'
  when (originalAda /= updatedAda) $ logEvent $ MCLogAdjustedTxSkelOut txSkelOut updatedAda
  return txSkelOut'
  where
    go :: TxSkelOut -> Sem effs TxSkelOut
    go skelOut = do
      -- Computing the required minimal amount of ADA in this output
      requiredAda <- getTxSkelOutMinAda skelOut
      -- If this amount is sufficient, we return Nothing, otherwise, we adjust the
      -- output and possibly iterate
      if Api.getLovelace (skelOut ^. txSkelOutValueL % valueLovelaceL) >= requiredAda
        then return skelOut
        else go $ skelOut & txSkelOutValueL % valueLovelaceL .~ Api.Lovelace requiredAda

-- | This goes through all the `TxSkelOut`s of the given skeleton and updates
-- their ada value when requested by the user and required by the protocol
-- parameters. Logs an event whenever such a change occurs.
autoFillMinAda ::
  (Members '[Tweak, MockChainRead, MockChainLog, Error Ledger.ToCardanoError] effs) =>
  Sem effs ()
autoFillMinAda = do
  outputs <- viewTweak txSkelOutsL
  newOutputs <- forM outputs toTxSkelOutWithMinAda
  setTweak txSkelOutsL newOutputs
