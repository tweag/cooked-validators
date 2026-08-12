{-# LANGUAGE TemplateHaskell #-}

-- | This module exposes the 'MockChainSubmit' effect, which is responsible for
-- submitting a Cardano transaction for validation.
module Cooked.MockChain.Effect.Submission
  ( -- * The 'MockChainSubmit' effect
    MockChainSubmit (..),
    submitTransaction,

    -- * Interpretation functions
    runMockChainSubmit,
    runBlockChainSubmit,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Ledger.Shelley.API.Mempool qualified as Shelley
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Cooked.MockChain.Common
import Cooked.MockChain.Effect.Read.Conf
import Cooked.MockChain.Runtime.State
import Data.Foldable.Extra
import Ledger.Orphans ()
import Optics.Core
import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.Reader
import Polysemy.State

-- | An effect allow to submit a transaction for validation
data MockChainSubmit :: Effect where
  SubmitTransaction :: Transaction -> MockChainSubmit m SubmissionFailures

makeSem_ ''MockChainSubmit

-- | Submits a transaction for validation, returning a (possibly empty) list of
-- submission failures.
submitTransaction ::
  (Member MockChainSubmit effs) =>
  Transaction ->
  Sem effs SubmissionFailures

-- | Interprets the `MockChainSubmit` effect on an emulator
runMockChainSubmit ::
  forall effs a.
  (Member (State EmulatorState) effs) =>
  Sem (MockChainSubmit : effs) a ->
  Sem effs a
runMockChainSubmit = interpret $ \case
  SubmitTransaction cardanoTx -> do
    -- To run transaction validation we need a minimal ledger state
    eLedgerState <- gets emulatorStateLedgerState
    -- And the emulator params
    params <- gets emulatorStateParams
    -- We run the transaction validation through the emulator
    let (newELedgerState, submissionFailures) = case Emulator.validateAndApplyTx params eLedgerState cardanoTx of
          Left (Shelley.ApplyTxError errs) -> (newELedgerState, toList errs)
          Right (newELedgerState', _) -> (newELedgerState', [])
    -- We update the index with the utxos consumed and produced by the tx
    modify' $ set emulatorStateLedgerStateL newELedgerState
    -- We return the validation result
    return submissionFailures

-- | Interprets the `MockChainSubmit` effect by submitting the generated
-- transaction to a deployed node through a `Cardano.LocalNodeConnectInfo`
-- (socket path and network id) provided via a `Reader`, running in a stack
-- featuring @IO@ (via `Embed`).
runBlockChainSubmit ::
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
  Sem (MockChainSubmit : effs) a ->
  Sem effs a
runBlockChainSubmit = interpret $ \case
  SubmitTransaction cardanoTx -> do
    -- We retrieve the local node connection info.
    conn <- ask
    -- We submit the transaction to the node
    result <- embed $ Cardano.submitTxToNodeLocal conn $ Cardano.TxInMode Cardano.ShelleyBasedEraConway cardanoTx
    -- We disect the result the node sends us
    case result of
      Cardano.SubmitFail
        ( Cardano.TxValidationErrorInCardanoMode
            (Cardano.ShelleyTxValidationError Cardano.ShelleyBasedEraConway (Shelley.ApplyTxError err))
          ) -> return $ toList err
      -- Somehow, the error does not correspond to the proper era, should be unreachable
      Cardano.SubmitFail (Cardano.TxValidationErrorInCardanoMode _) -> fail "TxValidationErrorInCardanoMode: Unreachable case"
      -- There is an era mismatch between the ledger era and the transaction era
      Cardano.SubmitFail (Cardano.TxValidationEraMismatch eraMismatch) -> throw eraMismatch
      -- The submission was successful (no phase 1 error)
      Cardano.SubmitSuccess -> return []
