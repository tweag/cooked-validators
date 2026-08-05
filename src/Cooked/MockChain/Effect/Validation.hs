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
    validateTxSkel,
    validateTxSkel',
    validateTxSkel_,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Monad
import Cooked.MockChain.Automation.Pipeline
import Cooked.MockChain.Common
import Cooked.MockChain.Effect.Log
import Cooked.MockChain.Effect.Read.Chain
import Cooked.MockChain.Effect.Read.Conf
import Cooked.MockChain.Runtime.Error
import Cooked.MockChain.Runtime.State
import Cooked.Skeleton
import Data.Map.Strict qualified as Map
import Ledger.Index qualified as P.Ledger
import Ledger.Orphans ()
import Ledger.Tx qualified as P.Ledger
import Ledger.Tx.CardanoAPI qualified as P.Ledger
import Optics.Core
import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.Reader
import Polysemy.State

-- | An effect that offers the ability to send a `Cooked.Skeleton.TxSkel` for
-- validation on the emulated blockchain.
data MockChainValidate :: Effect where
  ValidateTxSkel :: TxSkel -> MockChainValidate m (P.Ledger.CardanoTx, Utxos)

makeSem_ ''MockChainValidate

-- | Generates, balances and validates a transaction from a skeleton, and
-- returns the validated transaction, alongside the created UTxOs.
validateTxSkel :: (Member MockChainValidate effs) => TxSkel -> Sem effs (P.Ledger.CardanoTx, Utxos)

-- | Same as `validateTxSkel`, but only returns the generated UTxOs
validateTxSkel' :: (Members '[MockChainReadChain, MockChainValidate] effs) => TxSkel -> Sem effs Utxos
validateTxSkel' = fmap snd . validateTxSkel

-- | Same as `validateTxSkel`, but discards the returned transaction
validateTxSkel_ :: (Member MockChainValidate effs) => TxSkel -> Sem effs ()
validateTxSkel_ = void . validateTxSkel

-- | Interprets the `MockChainValidate` effect on an emulator
runMockChainValidateEmul ::
  forall effs a.
  ( Members
      '[ State EmulatorState,
         State ChainIndex,
         Error P.Ledger.ToCardanoError,
         Error MockChainError,
         MockChainLog,
         MockChainReadChain,
         MockChainReadConf,
         Fail
       ]
      effs
  ) =>
  Sem (MockChainValidate : effs) a ->
  Sem effs a
runMockChainValidateEmul = interpret $ \case
  ValidateTxSkel skel -> do
    (finalTxSkel, (cardanoTx, mCollaterals, _fee)) <- runAutomationPipeline skel
    -- To run transaction validation we need a minimal ledger state
    eLedgerState <- gets emulatorStateLedgerState
    -- And the emulator params
    params <- gets emulatorStateParams
    -- We finally run the emulated validation. We update our internal state
    -- based on the validation result, and throw an error if this fails. If at
    -- some point we want to allows mockchain runs with validation errors, the
    -- caller will need to catch those errors and do something with them.
    newOutputs <- case Emulator.validateCardanoTx params eLedgerState cardanoTx of
      -- In case of a phase 1 error, we give back the same index
      (_, P.Ledger.FailPhase1 _ err) -> throw $ MCEValidationError P.Ledger.Phase1 [err]
      (newELedgerState, P.Ledger.FailPhase2 _ err _) | Just (colInputs, mRetColOutput) <- mCollaterals -> do
        -- We update the emulated ledger state
        modify' $ set emulatorStateLedgerStateL newELedgerState
        -- We remove the collateral utxos from our own stored outputs
        forM_ colInputs $ modify' . removeOutput
        -- We add the returned collateral to our outputs when it exists
        case (mRetColOutput, Map.toList $ P.Ledger.getCardanoTxProducedReturnCollateral cardanoTx) of
          (Nothing, []) -> return ()
          (Just retColOutput, [(txIn, _)]) -> modify' $ addOutput (P.Ledger.fromCardanoTxIn txIn) retColOutput
          _ -> fail "Unreachable case when processing return collaterals, please report a bug at https://github.com/tweag/cooked-validators/issues"
        -- We throw a mockchain error
        throw $ MCEValidationError P.Ledger.Phase2 [err]
      -- In case of success, we update the index with all inputs and outputs
      -- contained in the transaction
      (newELedgerState, P.Ledger.Success {}) -> do
        -- We update the index with the utxos consumed and produced by the tx
        modify' (set emulatorStateLedgerStateL newELedgerState)
        -- We retrieve the utxos created by the transaction
        let utxos = P.Ledger.fromCardanoTxIn . snd <$> P.Ledger.getCardanoTxOutRefs cardanoTx
        -- We combine them with their corresponding `TxSkelOut`
        let newOutputs = zip utxos (txSkelOutputs finalTxSkel)
        -- We add the news utxos to the state
        forM_ newOutputs $ modify' . uncurry addOutput
        -- And remove the old ones
        forM_ (Map.toList $ txSkelInputs finalTxSkel) $ modify' . removeOutput . fst
        -- We return the newly created outputs
        return newOutputs
      -- This is a theoretical unreachable case. Since we fail in Phase 2, it
      -- means the transaction involved script, and thus we must have generated
      -- collaterals.
      (_, P.Ledger.FailPhase2 {})
        | Nothing <- mCollaterals ->
            fail "Unreachable case when processing validation result, please report a bug at https://github.com/tweag/cooked-validators/issues"
    -- We increase the slot number
    modify' $ over emulatorStateLedgerStateL Emulator.nextSlot
    -- We log the validated transaction
    logEvent $
      MCLogNewTx
        (P.Ledger.fromCardanoTxId $ P.Ledger.getCardanoTxId cardanoTx)
        (fromIntegral $ length $ P.Ledger.getCardanoTxOutRefs cardanoTx)
    -- We return the validated transaction
    return (cardanoTx, newOutputs)

-- | Interprets the `MockChainValidate` effect by submitting the generated
-- transaction to a deployed node through a `Cardano.LocalNodeConnectInfo`
-- (socket path and network id) provided via a `Reader`, running in a stack
-- featuring @IO@ (via `Embed`).
--
-- NOTE: this is a first sketch. It runs the same adjustment pipeline as the
-- emulator interpreter to obtain a balanced Cardano transaction, then submits it
-- to the node instead of validating it locally. Several aspects still need to be
-- decided (see the open questions raised alongside this implementation).
runMockChainValidateNode ::
  forall effs a.
  ( Members
      '[ Embed IO,
         Error P.Ledger.ToCardanoError,
         Error MockChainError,
         MockChainLog,
         MockChainReadChain,
         MockChainReadConf,
         Reader Cardano.LocalNodeConnectInfo,
         Fail
       ]
      effs
  ) =>
  Sem (MockChainValidate : effs) a ->
  Sem effs a
runMockChainValidateNode = interpret $ \case
  ValidateTxSkel skel -> do
    -- We run the whole adjustment pipeline to obtain a balanced Cardano
    -- transaction, exactly like the emulator interpreter does.
    (finalTxSkel, (cardanoTx, _mCollaterals, _fee)) <- runAutomationPipeline skel
    -- We retrieve the local node connection info.
    conn <- ask
    -- We unwrap the underlying Cardano transaction to wrap it into a
    -- 'Cardano.TxInMode' and submit it to the node.
    let P.Ledger.CardanoEmulatorEraTx cTx = cardanoTx
    result <-
      embed $
        Cardano.submitTxToNodeLocal conn $
          Cardano.TxInMode Cardano.ShelleyBasedEraConway cTx
    case result of
      -- On success we mirror the emulator bookkeeping: we register the newly
      -- created outputs and drop the consumed ones from our local state.
      Cardano.SubmitSuccess -> do
        let utxos = P.Ledger.fromCardanoTxIn . snd <$> P.Ledger.getCardanoTxOutRefs cardanoTx
            newOutputs = zip utxos (txSkelOutputs finalTxSkel)
        logEvent $
          MCLogNewTx
            (P.Ledger.fromCardanoTxId $ P.Ledger.getCardanoTxId cardanoTx)
            (fromIntegral $ length $ P.Ledger.getCardanoTxOutRefs cardanoTx)
        return (cardanoTx, newOutputs)
      -- On rejection we currently surface the reason as a plain failure. This
      -- should likely be turned into a dedicated 'MockChainError' constructor.
      Cardano.SubmitFail reason ->
        fail $ "Node rejected the transaction: " <> show reason
