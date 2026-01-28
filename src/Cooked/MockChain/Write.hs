{-# LANGUAGE TemplateHaskell #-}

-- | This module exposes primitives to update the current state of the
-- blockchain, including by sending transactions for validation.
module Cooked.MockChain.Write
  ( -- * The `MockChainWrite` effect
    MockChainWrite (..),
    runMockChainWrite,

    -- * Modifications of the current time
    waitNSlots,
    awaitSlot,
    awaitEnclosingSlot,
    waitNMSFromSlotLowerBound,
    waitNMSFromSlotUpperBound,

    -- * Sending `Cooked.Skeleton.TxSkel`s for validation
    validateTxSkel,
    validateTxSkel',
    validateTxSkel_,

    -- * Other operations
    setParams,
    setConstitutionScript,
    forceOutputs,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Api.Ledger qualified as Cardano
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Lens qualified as Lens
import Control.Monad
import Cooked.MockChain.AutoFilling
import Cooked.MockChain.Balancing
import Cooked.MockChain.Common
import Cooked.MockChain.Error
import Cooked.MockChain.GenerateTx.Body
import Cooked.MockChain.GenerateTx.Output
import Cooked.MockChain.Log
import Cooked.MockChain.Read
import Cooked.MockChain.State
import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.Map.Strict qualified as Map
import Ledger.Index qualified as Ledger
import Ledger.Orphans ()
import Ledger.Slot qualified as Ledger
import Ledger.Tx qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.State

-- | An effect that offers all the primitives that are performing modifications
-- on the blockchain state.
data MockChainWrite :: Effect where
  WaitNSlots :: Integer -> MockChainWrite m Ledger.Slot
  SetParams :: Emulator.Params -> MockChainWrite m ()
  ValidateTxSkel :: TxSkel -> MockChainWrite m (Ledger.CardanoTx, Utxos)
  SetConstitutionScript :: (ToVScript s) => s -> MockChainWrite m ()
  ForceOutputs :: [TxSkelOut] -> MockChainWrite m Utxos

makeSem_ ''MockChainWrite

-- | Interpretes the `MockChainWrite` effect
runMockChainWrite ::
  forall effs a.
  ( Members
      '[ State MockChainState,
         Error Ledger.ToCardanoError,
         Error MockChainError,
         MockChainLog,
         MockChainRead,
         Fail
       ]
      effs
  ) =>
  Sem (MockChainWrite : effs) a ->
  Sem effs a
runMockChainWrite = interpret $ \case
  SetParams params -> do
    modify $ set mcstParamsL params
    modify $ over mcstLedgerStateL $ Emulator.updateStateParams params
  WaitNSlots n -> do
    cs <- gets (Emulator.getSlot . mcstLedgerState)
    if
      | n == 0 -> return cs
      | n > 0 -> do
          let newSlot = cs + fromIntegral n
          modify' (over mcstLedgerStateL $ Lens.set Emulator.elsSlotL $ fromIntegral newSlot)
          return newSlot
      | otherwise -> throw $ MCEPastSlot cs (cs + fromIntegral n)
  SetConstitutionScript (toVScript -> cScript) -> do
    modify' (mcstConstitutionL ?~ cScript)
    modify' $
      over mcstLedgerStateL $
        Lens.set Emulator.elsConstitutionScriptL $
          (Cardano.SJust . Cardano.toShelleyScriptHash . Script.toCardanoScriptHash)
            cScript
  ForceOutputs outputs -> do
    -- We retrieve the protocol parameters
    params <- getParams
    -- The emulator takes for granted transactions with a single pseudo input,
    -- which we build to force transaction validation
    let input =
          ( Cardano.genesisUTxOPseudoTxIn (Emulator.pNetworkId params) $
              Cardano.GenesisUTxOKeyHash $
                Cardano.KeyHash "23d51e91ae5adc7ae801e9de4cd54175fb7464ec2680b25686bbb194",
            Cardano.BuildTxWith $ Cardano.KeyWitness Cardano.KeyWitnessForSpending
          )
    -- We adjust the outputs for the minimal required ADA if needed
    outputsMinAda <- mapM toTxSkelOutWithMinAda outputs
    -- We transform these outputs to Cardano outputs
    outputs' <- mapM toCardanoTxOut outputsMinAda
    -- We create our transaction body, which only consists of the dummy input
    -- and the outputs to force, and make a transaction out of it.
    cardanoTx <-
      Ledger.CardanoEmulatorEraTx . txSignatoriesAndBodyToCardanoTx []
        <$> fromEither
          ( Emulator.createTransactionBody params $
              Ledger.CardanoBuildTx
                ( Ledger.emptyTxBodyContent
                    { Cardano.txOuts = outputs',
                      Cardano.txIns = [input]
                    }
                )
          )
    -- We need to adjust our internal state to account for the forced
    -- transaction. We beging by computing the new map of outputs.
    let outputsMap =
          Map.fromList $
            zipWith
              (\x y -> (x, (y, True)))
              (Ledger.fromCardanoTxIn . snd <$> Ledger.getCardanoTxOutRefs cardanoTx)
              outputsMinAda
    -- We update the index, which effectively receives the new utxos
    modify'
      ( over mcstLedgerStateL $
          Lens.over
            Emulator.elsUtxoL
            ( Ledger.fromPlutusIndex
                . Ledger.insert cardanoTx
                . Ledger.toPlutusIndex
            )
      )
    -- We update our internal map by adding the new outputs
    modify' (over mcstOutputsL (<> outputsMap))
    -- Finally, we return the created utxos
    return $ Map.toList (fst <$> outputsMap)
  ValidateTxSkel skel -> fmap snd $ runTweak skel $ do
    -- We retrieve the current skeleton options
    TxSkelOpts {..} <- viewTweak txSkelOptsL
    -- We log the submission of the new skeleton
    viewTweak simple >>= logEvent . MCLogSubmittedTxSkel
    -- We retrieve the current parameters
    oldParams <- getParams
    -- We compute the optionally modified parameters
    let newParams = txSkelOptModParams oldParams
    -- We change the parameters for the duration of the validation process
    modify $ set mcstParamsL newParams
    modify $ over mcstLedgerStateL $ Emulator.updateStateParams newParams
    -- We ensure that the outputs have the required minimal amount of ada, when
    -- requested in the skeleton options
    autoFillMinAda
    -- We retrieve the official constitution script and attach it to each
    -- proposal that requires it, if it's not empty
    autoFillConstitution
    -- We add reference scripts in the various redeemers of the skeleton, when
    -- they can be found in the index and are allowed to be auto filled
    autoFillReferenceScripts
    -- We attach the reward amount to withdrawals when applicable
    autoFillWithdrawalAmounts
    -- We balance the skeleton when requested in the skeleton option, and get
    -- the associated fee, collateral inputs and return collateral user
    (finalTxSkel, fee, mCollaterals) <- viewTweak simple >>= balanceTxSkel
    -- We log the adjusted skeleton
    logEvent $ MCLogAdjustedTxSkel finalTxSkel fee mCollaterals
    -- We generate the transaction asscoiated with the skeleton, and apply on it
    -- the modifications from the skeleton options
    cardanoTx <- Ledger.CardanoEmulatorEraTx . txSkelOptModTx <$> txSkelToCardanoTx finalTxSkel fee mCollaterals
    -- To run transaction validation we need a minimal ledger state
    eLedgerState <- gets mcstLedgerState
    -- We finally run the emulated validation. We update our internal state
    -- based on the validation result, and throw an error if this fails. If at
    -- some point we want to allows mockchain runs with validation errors, the
    -- caller will need to catch those errors and do something with them.
    newOutputs <- case Emulator.validateCardanoTx newParams eLedgerState cardanoTx of
      -- In case of a phase 1 error, we give back the same index
      (_, Ledger.FailPhase1 _ err) -> throw $ MCEValidationError Ledger.Phase1 err
      (newELedgerState, Ledger.FailPhase2 _ err _) | Just (colInputs, retColUser) <- mCollaterals -> do
        -- We update the emulated ledger state
        modify' (set mcstLedgerStateL newELedgerState)
        -- We remove the collateral utxos from our own stored outputs
        forM_ colInputs $ modify' . removeOutput
        -- We add the returned collateral to our outputs (in practice this map
        -- either contains no element, or a single one)
        forM_ (Map.toList $ Ledger.getCardanoTxProducedReturnCollateral cardanoTx) $ \(txIn, txOut) ->
          modify' $
            addOutput
              (Ledger.fromCardanoTxIn txIn)
              (retColUser `receives` Value (Api.txOutValue . Ledger.fromCardanoTxOutToPV2TxInfoTxOut . Ledger.getTxOut $ txOut))
        -- We throw a mockchain error
        throw $ MCEValidationError Ledger.Phase2 err
      -- In case of success, we update the index with all inputs and outputs
      -- contained in the transaction
      (newELedgerState, Ledger.Success {}) -> do
        -- We update the index with the utxos consumed and produced by the tx
        modify' (set mcstLedgerStateL newELedgerState)
        -- We retrieve the utxos created by the transaction
        let utxos = Ledger.fromCardanoTxIn . snd <$> Ledger.getCardanoTxOutRefs cardanoTx
        -- We combine them with their corresponding `TxSkelOut`
        let newOutputs = zip utxos (txSkelOuts finalTxSkel)
        -- We add the news utxos to the state
        forM_ newOutputs $ modify' . uncurry addOutput
        -- And remove the old ones
        forM_ (Map.toList $ txSkelIns finalTxSkel) $ modify' . removeOutput . fst
        -- We return the newly created outputs
        return newOutputs
      -- This is a theoretical unreachable case. Since we fail in Phase 2, it
      -- means the transaction involved script, and thus we must have generated
      -- collaterals.
      (_, Ledger.FailPhase2 {})
        | Nothing <- mCollaterals ->
            fail "Unreachable case when processing validation result, please report a bug at https://github.com/tweag/cooked-validators/issues"
    -- We apply a change of slot when requested in the options
    when txSkelOptAutoSlotIncrease $ modify' (over mcstLedgerStateL Emulator.nextSlot)
    -- We return the parameters to their original state
    modify $ set mcstParamsL oldParams
    modify $ over mcstLedgerStateL $ Emulator.updateStateParams oldParams
    -- We log the validated transaction
    logEvent $ MCLogNewTx (Ledger.fromCardanoTxId $ Ledger.getCardanoTxId cardanoTx) (fromIntegral $ length $ Ledger.getCardanoTxOutRefs cardanoTx)
    -- We return the validated transaction
    return (cardanoTx, newOutputs)

-- | Waits a certain number of slots and returns the new slot
waitNSlots :: (Member MockChainWrite effs) => Integer -> Sem effs Ledger.Slot

-- | Wait for a certain slot, or throws an error if the slot is already past
awaitSlot :: (Members '[MockChainRead, MockChainWrite] effs) => Ledger.Slot -> Sem effs Ledger.Slot
awaitSlot (Ledger.Slot targetSlot) = do
  Ledger.Slot now <- currentSlot
  waitNSlots (targetSlot - now)

-- | Waits until the current slot becomes greater or equal to the slot
--  containing the given POSIX time.  Note that that it might not wait for
--  anything if the current slot is large enough.
awaitEnclosingSlot :: (Members '[MockChainRead, MockChainWrite] effs) => Api.POSIXTime -> Sem effs Ledger.Slot
awaitEnclosingSlot time = getEnclosingSlot time >>= awaitSlot

-- | Wait a given number of ms from the lower bound of the current slot and
-- returns the current slot after waiting.
waitNMSFromSlotLowerBound :: (Members '[MockChainRead, MockChainWrite, Fail] effs) => Integer -> Sem effs Ledger.Slot
waitNMSFromSlotLowerBound duration = currentMSRange >>= awaitEnclosingSlot . (+ fromIntegral duration) . fst

-- | Wait a given number of ms from the upper bound of the current slot and
-- returns the current slot after waiting.
waitNMSFromSlotUpperBound :: (Members '[MockChainRead, MockChainWrite, Fail] effs) => Integer -> Sem effs Ledger.Slot
waitNMSFromSlotUpperBound duration = currentMSRange >>= awaitEnclosingSlot . (+ fromIntegral duration) . snd

-- | Generates, balances and validates a transaction from a skeleton, and
-- returns the validated transaction, alongside the created UTxOs.
validateTxSkel :: (Member MockChainWrite effs) => TxSkel -> Sem effs (Ledger.CardanoTx, Utxos)

-- | Same as `validateTxSkel`, but only returns the generated UTxOs
validateTxSkel' :: (Members '[MockChainRead, MockChainWrite] effs) => TxSkel -> Sem effs Utxos
validateTxSkel' = fmap snd . validateTxSkel

-- | Same as `validateTxSkel`, but discards the returned transaction
validateTxSkel_ :: (Member MockChainWrite effs) => TxSkel -> Sem effs ()
validateTxSkel_ = void . validateTxSkel

-- | Updates the current parameters
setParams :: (Member MockChainWrite effs) => Emulator.Params -> Sem effs ()

-- | Sets the current script to act as the official constitution script
setConstitutionScript :: (Member MockChainWrite effs, ToVScript s) => s -> Sem effs ()

-- | Forces the generation of utxos corresponding to certain
-- `TxSkelOut`. Returns the created UTxOs, which might differ from the original
-- list if some min ADA adjustment occured.
forceOutputs :: (Member MockChainWrite effs) => [TxSkelOut] -> Sem effs Utxos
