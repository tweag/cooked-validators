{-# LANGUAGE TemplateHaskell #-}

-- | This module exposes the `MockChainValidate` effect, which is responsible
-- for turning a `Cooked.Skeleton.TxSkel` into an actual transaction and
-- submitting it to the emulated ledger. This includes running the whole
-- adjustment pipeline (auto-filling, balancing and transaction generation) and
-- updating the mockchain state based on the validation outcome.
module Cooked.MockChain.Effect.Validation
  ( -- * The `MockChainValidate` effect
    MockChainValidate (..),
    runMockChainValidateEmul,
    runMockChainValidateNode,

    -- * Sending `Cooked.Skeleton.TxSkel`s for validation
    submitTransaction,
    validateTxSkel,
    validateTxSkel',
    validateTxSkelL,
    validateTxSkel_,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Ledger.Conway qualified as Conway
import Cardano.Ledger.Conway.Rules qualified as Conway
import Cardano.Ledger.Shelley.API.Mempool qualified as Shelley
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Monad
import Cooked.MockChain.Automation
import Cooked.MockChain.Common
import Cooked.MockChain.Effect.Log
import Cooked.MockChain.Effect.Read.Chain
import Cooked.MockChain.Effect.Read.Conf
import Cooked.MockChain.Runtime.Error
import Cooked.MockChain.Runtime.State
import Cooked.Skeleton
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Ledger.Index qualified as P.Ledger
import Ledger.Orphans ()
import Ledger.Tx qualified as P.Ledger
import Ledger.Tx.CardanoAPI qualified as P.Ledger
import Optics.Core
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.Reader
import Polysemy.State

-- | An effect that offers the ability to submit a transaction for validation,
-- while returning the list of validation failures, if any.
data MockChainValidate :: Effect where
  SubmitTransaction :: Tx -> MockChainValidate m [Conway.ConwayLedgerPredFailure Conway.ConwayEra]

makeSem_ ''MockChainValidate

submitTransaction :: (Member MockChainValidate effs) => Tx -> Sem effs [Conway.ConwayLedgerPredFailure Conway.ConwayEra]

-- | Generates, balances and validates a transaction from a skeleton
validateTxSkel ::
  ( Members
      '[ MockChainValidate,
         MockChainLog,
         MockChainReadChain,
         MockChainReadConf,
         Error P.Ledger.ToCardanoError,
         Error MockChainError,
         Fail
       ]
      effs
  ) =>
  TxSkel ->
  Sem effs (Tx, Utxos)
validateTxSkel txSkel = do
  -- We fetch the skeleton options
  let TxSkelOpts {..} = txSkelOpts txSkel
  -- We log the submission of the new skeleton
  logEvent $ MCLogSubmittedTxSkel txSkel
  -- We run the automation pipeline on the original skeleton
  ExtendedTxSkel finalTxSkel fee mCollaterals txBody valErrorsExUnits <- runAutomationPipeline txSkel
  -- We log the adjusted skeleton
  logEvent $ MCLogAdjustedTxSkel finalTxSkel fee mCollaterals
  -- We retrieve the extra signatories to add to the transaction
  let signatories = view txSkelSignatoriesL finalTxSkel
  -- We build the Cardano transaction
  let cardanoTx = txSkelOptModTx $ txSignatoriesAndBodyToCardanoTx signatories txBody
  -- We wrap it for plutus-ledger usage
  let pCardanoTx = P.Ledger.CardanoTx cardanoTx Cardano.ShelleyBasedEraConway
  -- We submit the transaction for validation
  valErrorsSubmission <- submitTransaction cardanoTx
  --   newOutputs <- case  of
  --     -- In case of a phase 1 error, we give back the same index
  --     (_, P.Ledger.FailPhase1 _ err) -> throw $ MCEValidationError P.Ledger.Phase1 [err]
  --     (newELedgerState, P.Ledger.FailPhase2 _ err _) | Just (colInputs, mRetColOutput) <- mCollaterals -> do
  --       -- We update the emulated ledger state
  --       modify' $ set emulatorStateLedgerStateL newELedgerState
  --       -- We remove the collateral utxos from our own stored outputs
  --       forM_ colInputs $ modify' . removeOutput
  --       -- We add the returned collateral to our outputs when it exists
  --       case (mRetColOutput, Map.toList $ P.Ledger.getCardanoTxProducedReturnCollateral cardanoTx) of
  --         (Nothing, []) -> return ()
  --         (Just retColOutput, [(txIn, _)]) -> modify' $ addOutput (P.Ledger.fromCardanoTxIn txIn) retColOutput
  --         _ -> fail "Unreachable case when processing return collaterals, please report a bug at https://github.com/tweag/cooked-validators/issues"
  --       -- We throw a mockchain error
  --       throw $ MCEValidationError P.Ledger.Phase2 [err]
  --     -- In case of success, we update the index with all inputs and outputs
  --     -- contained in the transaction
  --     (newELedgerState, P.Ledger.Success {}) -> do
  --       -- We retrieve the utxos created by the transaction
  --       let utxos = P.Ledger.fromCardanoTxIn . snd <$> P.Ledger.getCardanoTxOutRefs cardanoTx
  --       -- We combine them with their corresponding `TxSkelOut`
  --       let newOutputs = zip utxos (txSkelOutputs finalTxSkel)
  --       -- We add the news utxos to the state
  --       forM_ newOutputs $ modify' . uncurry addOutput
  --       -- And remove the old ones
  --       forM_ (Map.toList $ txSkelInputs finalTxSkel) $ modify' . removeOutput . fst
  --       -- We return the newly created outputs
  --       return $ Map.fromList newOutputs
  --     -- This is a theoretical unreachable case. Since we fail in Phase 2, it
  --     -- means the transaction involved script, and thus we must have generated
  --     -- collaterals.
  --     (_, P.Ledger.FailPhase2 {})
  --       | Nothing <- mCollaterals ->
  --           fail "Unreachable case when processing validation result, please report a bug at https://github.com/tweag/cooked-validators/issues"
  --   -- We increase the slot number
  --   modify' $ over emulatorStateLedgerStateL Emulator.nextSlot
  --   -- We log the validated transaction
  logEvent $
    MCLogNewTx
      (P.Ledger.fromCardanoTxId $ P.Ledger.getCardanoTxId pCardanoTx)
      (fromIntegral $ length $ P.Ledger.getCardanoTxOutRefs pCardanoTx)
  -- We return the validated transaction
  return (cardanoTx, newOutputs)

-- | Same as `validateTxSkel`, but only returns the generated UTxOs
validateTxSkel' ::
  ( Members
      '[ MockChainValidate,
         MockChainLog,
         MockChainReadChain,
         MockChainReadConf,
         Error P.Ledger.ToCardanoError,
         Error MockChainError,
         Fail
       ]
      effs
  ) =>
  TxSkel ->
  Sem effs Utxos
validateTxSkel' = fmap snd . validateTxSkel

-- | Same as `validateTxSkel`, but only returns the list of 'Api.TxOutRef'
validateTxSkelL ::
  ( Members
      '[ MockChainValidate,
         MockChainLog,
         MockChainReadChain,
         MockChainReadConf,
         Error P.Ledger.ToCardanoError,
         Error MockChainError,
         Fail
       ]
      effs
  ) =>
  TxSkel ->
  Sem effs [Api.TxOutRef]
validateTxSkelL = fmap (Set.toList . Map.keysSet . snd) . validateTxSkel

-- | Same as `validateTxSkel`, but discards the returned transaction
validateTxSkel_ ::
  ( Members
      '[ MockChainValidate,
         MockChainLog,
         MockChainReadChain,
         MockChainReadConf,
         Error P.Ledger.ToCardanoError,
         Error MockChainError,
         Fail
       ]
      effs
  ) =>
  TxSkel ->
  Sem effs ()
validateTxSkel_ = void . validateTxSkel

-- | Interprets the `MockChainValidate` effect on an emulator
runMockChainValidateEmul ::
  forall effs a.
  (Member (State EmulatorState) effs) =>
  Sem (MockChainValidate : effs) a ->
  Sem effs a
runMockChainValidateEmul = interpret $ \case
  SubmitTransaction cardanoTx -> do
    -- To run transaction validation we need a minimal ledger state
    eLedgerState <- gets emulatorStateLedgerState
    -- And the emulator params
    params <- gets emulatorStateParams
    -- We run the transaction validation through the emulator
    let (newELedgerState, validationResult) = Emulator.validateCardanoTx params eLedgerState $ P.Ledger.CardanoEmulatorEraTx cardanoTx
    -- We update the index with the utxos consumed and produced by the tx
    modify' $ set emulatorStateLedgerStateL newELedgerState
    -- We return the validation result
    return undefined

-- | Interprets the `MockChainValidate` effect by submitting the generated
-- transaction to a deployed node through a `Cardano.LocalNodeConnectInfo`
-- (socket path and network id) provided via a `Reader`, running in a stack
-- featuring @IO@ (via `Embed`).
runMockChainValidateNode ::
  forall effs a.
  ( Members
      '[ Embed IO,
         Error Cardano.EraMismatch,
         MockChainReadConf,
         Reader Cardano.LocalNodeConnectInfo,
         Fail
       ]
      effs
  ) =>
  Sem (MockChainValidate : effs) a ->
  Sem effs a
runMockChainValidateNode = interpret $ \case
  SubmitTransaction cardanoTx -> do
    -- We retrieve the local node connection info.
    conn <- ask
    -- We submit the transaction to the node
    result <- embed $ Cardano.submitTxToNodeLocal conn $ Cardano.TxInMode Cardano.ShelleyBasedEraConway cardanoTx
    -- We disect the result the node sends us
    case result of
      Cardano.SubmitFail (Cardano.TxValidationErrorInCardanoMode (Cardano.ShelleyTxValidationError Cardano.ShelleyBasedEraConway (Shelley.ApplyTxError err))) ->
        return $ toList err
      -- Somehow, the error does not correspond to the proper era, should be unreachable
      Cardano.SubmitFail (Cardano.TxValidationErrorInCardanoMode _) -> fail "TxValidationErrorInCardanoMode: Unreachable case"
      -- There is an era mismatch between the ledger era and the transaction era
      Cardano.SubmitFail (Cardano.TxValidationEraMismatch eraMismatch) -> throw eraMismatch
      -- The submission was successful (no phase 1 error)
      Cardano.SubmitSuccess -> return []
