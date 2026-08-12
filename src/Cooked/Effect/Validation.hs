{-# LANGUAGE TemplateHaskell #-}

-- | This module exposes the `Validate` effect, which is responsible
-- for turning a `Cooked.Skeleton.TxSkel` into an actual transaction and
-- submitting it to the emulated ledger. This includes running the whole
-- adjustment pipeline (auto-filling, balancing and transaction generation) and
-- updating the mockchain state based on the validation outcome.
module Cooked.Effect.Validation
  ( -- * The `Validate` effect
    Validate (..),
    validateTxSkel,
    validateTxSkel',
    validateTxSkelL,
    validateTxSkel_,

    -- * Interpreting the effect
    runMockChainValidate,
  )
where

import Cardano.Api qualified as Cardano
import Control.Monad
import Cooked.Automation
import Cooked.Effect.Log
import Cooked.Effect.Params
import Cooked.Effect.Query
import Cooked.Effect.Submission
import Cooked.Runtime.Error
import Cooked.Runtime.State
import Cooked.Skeleton
import Cooked.Utilities.Aliases
import Data.Foldable.Extra
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Ledger.Orphans ()
import Ledger.Tx qualified as P.Ledger
import Ledger.Tx.CardanoAPI qualified as P.Ledger
import Optics.Core
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.State

-- | An effect that offers the ability to submit a 'TxSkel' throughout the
-- modification and validation pipeline. Technically, this effect is not needed
-- from a semantical perspective, as all of this could already be expressed in
-- 'Submit', however, we want this effect to exist on its own to be
-- eligible to be modified by tweaks.
data Validate :: Effect where
  ValidateTxSkel :: TxSkel -> Validate m (ExtendedTxSkel, SubmissionFailures, Transaction, Utxos)

makeSem_ ''Validate

-- | Generates, balances and validates a transaction from a skeleton. Returns
-- the extended skeleton, generated transaction and the new produced outputs.
validateTxSkel ::
  (Member Validate effs) =>
  TxSkel ->
  Sem effs (ExtendedTxSkel, SubmissionFailures, Transaction, Utxos)

-- | Same as `validateTxSkel`, but only returns the generated UTxOs
validateTxSkel' ::
  (Member Validate effs) =>
  TxSkel ->
  Sem effs Utxos
validateTxSkel' = fmap (view _4) . validateTxSkel

-- | Same as `validateTxSkel'`, but only returns the list of produced
-- 'Api.TxOutRef'
validateTxSkelL ::
  (Member Validate effs) =>
  TxSkel ->
  Sem effs [Api.TxOutRef]
validateTxSkelL = fmap (toList . Map.keysSet) . validateTxSkel'

-- | Same as `validateTxSkel`, but discards the returned transaction
validateTxSkel_ ::
  (Member Validate effs) =>
  TxSkel ->
  Sem effs ()
validateTxSkel_ = void . validateTxSkel

-- | Interpretes the 'Validate' effects in terms of other effects, in
-- particular 'Submit'.
runMockChainValidate ::
  ( Members
      '[ Log,
         Query,
         Params,
         Submit,
         Error P.Ledger.ToCardanoError,
         Error MockChainError,
         State ChainIndex,
         Fail
       ]
      effs
  ) =>
  Sem (Validate : effs) a ->
  Sem effs a
runMockChainValidate = interpret $ \case
  ValidateTxSkel txSkel -> do
    -- We fetch the skeleton options
    let TxSkelOpts {..} = txSkelOpts txSkel
    -- We log the submission of the new skeleton
    logEvent $ MCLogSubmittedTxSkel txSkel
    -- We run the automation pipeline on the original skeleton
    eSkel@(ExtendedTxSkel finalTxSkel fee mCollaterals txBody exUnitsFailures) <- runAutomationPipeline txSkel
    -- We log the adjusted skeleton
    logEvent $ MCLogAdjustedTxSkel finalTxSkel fee mCollaterals
    -- We handle the execution units failures when applicable
    when (notNull exUnitsFailures) $
      if txSkelOptHaltOnExUnitsFailures
        -- If requested, we treat them as fatal, ending the run
        then throw $ MCEExUnitsFailures exUnitsFailures
        -- Otherwise, we just log them
        else logEvent $ MCELogExUnitsFailures exUnitsFailures
    -- We build the Cardano transaction, and apply on it the modification in the
    -- skeleton option
    let cardanoTx = txSkelOptModTx $ txSignatoriesAndBodyToCardanoTx (view txSkelSignatoriesL finalTxSkel) txBody
    -- We wrap it for plutus-ledger usage
    let pCardanoTx = P.Ledger.CardanoTx cardanoTx Cardano.ShelleyBasedEraConway
    -- We compute the id of the new transaction
    let txId = P.Ledger.fromCardanoTxId $ P.Ledger.getCardanoTxId pCardanoTx
    -- We submit the transaction for validation
    submissionFailures <- submitTransaction cardanoTx
    -- We handle the submission failures when applicable
    when (notNull submissionFailures) $
      if txSkelOptHaltOnSubmissionFailures
        -- If requested, we treat them as fatal, ending the run
        then throw $ MCESubmissionFailures submissionFailures
        -- Otherwise, we just log them
        else logEvent $ MCELogSubmissionFailures submissionFailures
    -- We compute the set of consumed outputs and new outputs, based on the
    -- validity of the transaction, producing some validity logs in the process.
    (consumedInputs, newOutputs) <-
      if
        -- the transaction is valid, the index is modified based on the regular
        -- inputs and outputs of the transaction.
        | null submissionFailures && null exUnitsFailures -> do
            let inputs = Map.keysSet $ txSkelInputs finalTxSkel
                outputs = fromCardanoIndex (P.Ledger.getCardanoTxProducedOutputs pCardanoTx) $ txSkelOutputs finalTxSkel
            logEvent $ MCLogNewTx txId $ Valid (length inputs) (Map.size outputs)
            return (inputs, outputs)
        -- the transaction fails in phase 1, the index remains unchanged.
        | notNull submissionFailures -> do
            logEvent $ MCLogNewTx txId InvalidPhase1
            return (Set.empty, Map.empty)
        -- the transaction fails in phase 2, but no collaterals were
        -- provided. This is an unreachable case.
        | Nothing <- mCollaterals ->
            fail
              "Unreachable case when processing validation result, please report a bug at https://github.com/tweag/cooked-validators/issues"
        -- the transaction fails in phase 2, and collaterals are provided, the
        -- index is modified based on the collateral inputs and outputs of the
        -- transaction.
        | Just (colIns, retCol) <- mCollaterals -> do
            let outputs = fromCardanoIndex (P.Ledger.getCardanoTxProducedReturnCollateral pCardanoTx) $ toList retCol
            logEvent $ MCLogNewTx txId $ InvalidPhase2 (length colIns) (Map.size outputs)
            return (colIns, outputs)
    -- We update the index with the consumed and produced outputs
    modify' $ removeOutputs consumedInputs
    modify' $ addOutputs $ Map.toList newOutputs
    return (eSkel, submissionFailures, cardanoTx, newOutputs)
    where
      fromCardanoIndex index = Map.fromList . zip (P.Ledger.fromCardanoTxIn . fst <$> Map.toList index)
