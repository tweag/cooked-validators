-- | This module exposes functions to automatically attach reference inputs
-- carrying reference scripts to the redeemers of a 'Cooked.Skeleton.TxSkel',
-- based on the current state of the blockchain.
module Cooked.Automation.AutoFilling.ReferenceScripts
  ( updateRedeemedScript,
    autoFillReferenceScripts,
  )
where

import Control.Monad
import Cooked.Effect.Log
import Cooked.Effect.Query
import Cooked.Runtime.Journal
import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Query
import Cooked.Tweak.Update
import Cooked.Utilities.UtxoSearch
import Data.List (find)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy

-- * Auto filling reference scripts

-- | Attempts to find in the index a utxo containing a reference script with the
-- given script hash, and attaches it to a redeemer when it does not yet have a
-- reference input and when it is allowed, in which case an event is logged.
updateRedeemedScript ::
  (Members '[Log, Query] effs) =>
  [Api.TxOutRef] ->
  User IsScript Redemption ->
  Sem effs (User IsScript Redemption)
updateRedeemedScript
  inputs
  rs@( UserRedeemedScript
         (toVScript -> vScript)
         txSkelRed@(TxSkelRedeemer {txSkelRedeemerAutoFill = True})
       ) = do
    oRefsInInputs <-
      allUtxos
        >>= ensureAFoldIs (txSkelOutReferenceScriptHashAF % filtered (== Script.toScriptHash vScript))
        >>= retrieveTxOutRefs
    maybe
      -- We leave the redeemer unchanged if no reference input was found
      (return rs)
      -- If a reference input is found, we assign it and log the event
      ( \oRef -> do
          logEvent $ CLogAddedReferenceScript txSkelRed oRef (Script.toScriptHash vScript)
          return $ over userRedeemerAT (fillReferenceInput oRef) rs
      )
      $ case oRefsInInputs of
        s | null s -> Nothing
        -- If possible, we use a reference input appearing in regular inputs
        s | Just oRefM' <- find (`elem` inputs) s -> Just oRefM'
        -- If none exist, we use the first one we find elsewhere
        s -> Just $ Set.elemAt 0 s
updateRedeemedScript _ rs = return rs

-- | Goes through the various parts of the skeleton where a redeemer can appear,
-- and attempts to attach a reference input to each of them, whenever it is
-- allowed and one has not already been set. Logs an event whenever such an
-- addition occurs.
autoFillReferenceScripts ::
  (Members '[Tweak, Query, Log] effs) =>
  Sem effs ()
autoFillReferenceScripts = do
  inputsKeys <- viewTweak $ txSkelInputsL % to Map.keys
  -- Updating spending redeemers, whose validators are fetched from the index
  -- based on the inputs' references, and thus require a dedicated treatment.
  inputsList <- viewTweak $ txSkelInputsL % to Map.toList
  newInputs <- forM inputsList $ \(oRef, red) ->
    (oRef,) <$> do
      validatorM <- previewByRef (txSkelOutOwnerL % userVScriptAT) oRef
      case validatorM of
        Nothing -> return red
        Just val -> view userRedeemerL <$> updateRedeemedScript inputsKeys (UserRedeemedScript val red)
  setTweak txSkelInputsL $ Map.fromList newInputs
  -- Updating minting, proposing, withdrawing and certifying redeemers, whose
  -- scripts are directly stored in the skeleton, in one go.
  traverseTweak txSkelRedeemedScriptsT (updateRedeemedScript inputsKeys)
