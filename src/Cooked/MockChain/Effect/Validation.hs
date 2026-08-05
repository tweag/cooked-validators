{-# LANGUAGE TemplateHaskell #-}

-- | This module exposes the `MockChainValidate` effect, which is responsible
-- for turning a `Cooked.Skeleton.TxSkel` into an actual transaction and
-- submitting it to the emulated ledger. This includes running the whole
-- adjustment pipeline (auto-filling, balancing and transaction generation) and
-- updating the mockchain state based on the validation outcome.
module Cooked.MockChain.Effect.Validation
  ( -- * The `MockChainValidate` effect
    MockChainValidate (..),
    runMockChainValidate,

    -- * Sending `Cooked.Skeleton.TxSkel`s for validation
    validateTxSkel,
    validateTxSkel',
    validateTxSkel_,
  )
where

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
import Polysemy.State

-- | An effect that offers the ability to send a `Cooked.Skeleton.TxSkel` for
-- validation on the emulated blockchain.
data MockChainValidate :: Effect where
  ValidateTxSkel :: TxSkel -> MockChainValidate m (P.Ledger.CardanoTx, Utxos)

makeSem_ ''MockChainValidate

-- | Interpretes the `MockChainValidate` effect
runMockChainValidate ::
  forall effs a.
  ( Members
      '[ State MockChainState,
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
runMockChainValidate = interpret $ \case
  ValidateTxSkel skel -> do
    (finalTxSkel, (cardanoTx, mCollaterals, _)) <- runAutomationPipeline skel
    -- To run transaction validation we need a minimal ledger state
    eLedgerState <- gets mcstLedgerState
    -- And the emulator params
    params <- gets mcstParams
    -- We finally run the emulated validation. We update our internal state
    -- based on the validation result, and throw an error if this fails. If at
    -- some point we want to allows mockchain runs with validation errors, the
    -- caller will need to catch those errors and do something with them.
    newOutputs <- case Emulator.validateCardanoTx params eLedgerState cardanoTx of
      -- In case of a phase 1 error, we give back the same index
      (_, P.Ledger.FailPhase1 _ err) -> throw $ MCEValidationError P.Ledger.Phase1 [err]
      (newELedgerState, P.Ledger.FailPhase2 _ err _) | Just (colInputs, mRetColOutput) <- mCollaterals -> do
        -- We update the emulated ledger state
        modify' (set mcstLedgerStateL newELedgerState)
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
        modify' (set mcstLedgerStateL newELedgerState)
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
    modify' $ over mcstLedgerStateL Emulator.nextSlot
    -- We log the validated transaction
    logEvent $
      MCLogNewTx
        (P.Ledger.fromCardanoTxId $ P.Ledger.getCardanoTxId cardanoTx)
        (fromIntegral $ length $ P.Ledger.getCardanoTxOutRefs cardanoTx)
    -- We return the validated transaction
    return (cardanoTx, newOutputs)

-- | Generates, balances and validates a transaction from a skeleton, and
-- returns the validated transaction, alongside the created UTxOs.
validateTxSkel :: (Member MockChainValidate effs) => TxSkel -> Sem effs (P.Ledger.CardanoTx, Utxos)

-- | Same as `validateTxSkel`, but only returns the generated UTxOs
validateTxSkel' :: (Members '[MockChainReadChain, MockChainValidate] effs) => TxSkel -> Sem effs Utxos
validateTxSkel' = fmap snd . validateTxSkel

-- | Same as `validateTxSkel`, but discards the returned transaction
validateTxSkel_ :: (Member MockChainValidate effs) => TxSkel -> Sem effs ()
validateTxSkel_ = void . validateTxSkel
