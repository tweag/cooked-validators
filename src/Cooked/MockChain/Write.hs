{-# LANGUAGE TemplateHaskell #-}

-- | This module exposes primitives to update the current state of the
-- blockchain, including by sending transactions for validation.
module Cooked.MockChain.Write
  ( -- * The `MockChainWrite` effect
    MockChainWrite,
    reinterpretMockChainWriteWithTweak,
    runMockChainWrite,

    -- * Untyped tweaks and associated modalities
    UntypedTweak (..),
    somewhere,
    everywhere,
    nowhere,
    whenAble,
    there,
    withTweak,

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
import Cooked.Ltl
import Cooked.MockChain.AutoFilling
import Cooked.MockChain.Balancing
import Cooked.MockChain.Error
import Cooked.MockChain.GenerateTx.Body
import Cooked.MockChain.GenerateTx.Output
import Cooked.MockChain.Log
import Cooked.MockChain.MockChainState
import Cooked.MockChain.Read
import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.Coerce
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
import Polysemy.Internal
import Polysemy.NonDet
import Polysemy.State

-- | An effect that offers all the primitives that are performing modifications
-- on the blockchain state.
data MockChainWrite :: Effect where
  WaitNSlots :: Integer -> MockChainWrite m Ledger.Slot
  SetParams :: Emulator.Params -> MockChainWrite m ()
  ValidateTxSkel :: TxSkel -> MockChainWrite m Ledger.CardanoTx
  SetConstitutionScript :: (ToVScript s) => s -> MockChainWrite m ()
  ForceOutputs :: [TxSkelOut] -> MockChainWrite m [Api.TxOutRef]

makeSem_ ''MockChainWrite

type TypedTweak tweakEffs a = Sem (Tweak : NonDet : tweakEffs) a

-- | Wrapping up tweaks while hiding their return type and unsuring their stack
-- of effects begins with `Tweak` and `NonDet`.
data UntypedTweak tweakEffs where
  UntypedTweak :: TypedTweak tweakEffs a -> UntypedTweak tweakEffs

fromTweak ::
  TypedTweak tweakEffs a ->
  Ltl (UntypedTweak tweakEffs)
fromTweak = LtlAtom . UntypedTweak

-- | Applies a 'Tweak' to every step in a trace where it is applicable,
-- branching at any such locations. The tweak must apply at least once.
somewhere ::
  (Members '[ModifyGlobally (UntypedTweak tweakEffs)] effs) =>
  TypedTweak tweakEffs b ->
  Sem effs a ->
  Sem effs a
somewhere = modifyLtl . ltlEventually . fromTweak

-- | Applies a 'Tweak' to every transaction in a given trace. Fails if the tweak
-- fails anywhere in the trace.
everywhere ::
  (Members '[ModifyGlobally (UntypedTweak tweakEffs)] effs) =>
  TypedTweak tweakEffs b ->
  Sem effs a ->
  Sem effs a
everywhere = modifyLtl . ltlAlways . fromTweak

-- | Ensures a given 'Tweak' can never successfully be applied in a computation,
-- and leaves the computation unchanged.
nowhere ::
  (Members '[ModifyGlobally (UntypedTweak tweakEffs)] effs) =>
  TypedTweak tweakEffs b ->
  Sem effs a ->
  Sem effs a
nowhere = modifyLtl . ltlNever . fromTweak

-- | Apply a given 'Tweak' at every location in a computation where it does not
-- fail, which might never occur.
whenAble ::
  (Members '[ModifyGlobally (UntypedTweak tweakEffs)] effs) =>
  TypedTweak tweakEffs b ->
  Sem effs a ->
  Sem effs a
whenAble = modifyLtl . ltlWhenPossible . fromTweak

-- | Apply a 'Tweak' to the (0-indexed) nth transaction in a given
-- trace. Successful when this transaction exists and can be modified.
--
-- See also `Cooked.Tweak.Labels.labelled` to select transactions based on
-- labels instead of their index.
there ::
  (Members '[ModifyGlobally (UntypedTweak tweakEffs)] effs) =>
  Integer ->
  TypedTweak tweakEffs b ->
  Sem effs a ->
  Sem effs a
there n = modifyLtl . ltlDelay n . fromTweak

-- | Apply a 'Tweak' to the next transaction in the given trace. The order of
-- arguments enables an idiom like
--
-- > do ...
-- >    endpoint arguments `withTweak` someModification
-- >    ...
--
-- where @endpoint@ builds and validates a single transaction depending on the
-- given @arguments@. Then `withTweak` says "I want to modify the transaction
-- returned by this endpoint in the following way".
withTweak ::
  (Members '[ModifyGlobally (UntypedTweak tweakEffs)] effs) =>
  Sem effs a ->
  TypedTweak tweakEffs b ->
  Sem effs a
withTweak = flip (there 0)

-- | Reinterpretes `MockChainWrite` in itself, when the `ModifyLocally` effect
-- exists in the stack, applying the relevant modifications in the process.
reinterpretMockChainWriteWithTweak ::
  forall tweakEffs effs a.
  ( Members
      '[ ModifyLocally (UntypedTweak tweakEffs),
         NonDet
       ]
      effs,
    Subsume tweakEffs effs
  ) =>
  Sem (MockChainWrite : effs) a ->
  Sem (MockChainWrite : effs) a
reinterpretMockChainWriteWithTweak = reinterpret @MockChainWrite $ \case
  ValidateTxSkel skel -> do
    requirements <- getRequirements
    let sumTweak :: TypedTweak tweakEffs () =
          foldr
            ( \req acc -> case req of
                Apply (UntypedTweak tweak) -> tweak >> acc
                EnsureFailure (UntypedTweak tweak) -> do
                  txSkel' <- getTxSkel
                  results <- raise_ $ runNonDet @[] $ runTweak txSkel' tweak
                  guard $ null results
                  acc
            )
            (return ())
            requirements
    newTxSkel <- raise $ subsume_ $ fst <$> runTweak skel sumTweak
    validateTxSkel newTxSkel
  a -> send $ coerce a

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
    fmap fst <$> utxosFromCardanoTx cardanoTx
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
    case Emulator.validateCardanoTx newParams eLedgerState cardanoTx of
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
        -- We add the news utxos to the state
        forM_ (zip utxos (txSkelOuts finalTxSkel)) $ modify' . uncurry addOutput
        -- And remove the old ones
        forM_ (Map.toList $ txSkelIns finalTxSkel) $ modify' . removeOutput . fst
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
    return cardanoTx

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
-- returns the validated transaction.
validateTxSkel :: (Member MockChainWrite effs) => TxSkel -> Sem effs Ledger.CardanoTx

-- | Same as `validateTxSkel`, but only returns the generated UTxOs
validateTxSkel' :: (Members '[MockChainRead, MockChainWrite] effs) => TxSkel -> Sem effs [Api.TxOutRef]
validateTxSkel' = (fmap fst <$>) . utxosFromCardanoTx <=< validateTxSkel

-- | Same as `validateTxSkel`, but discards the returned transaction
validateTxSkel_ :: (Member MockChainWrite effs) => TxSkel -> Sem effs ()
validateTxSkel_ = void . validateTxSkel

-- | Updates the current parameters
setParams :: (Member MockChainWrite effs) => Emulator.Params -> Sem effs ()

-- | Sets the current script to act as the official constitution script
setConstitutionScript :: (Member MockChainWrite effs, ToVScript s) => s -> Sem effs ()

-- | Forces the generation of utxos corresponding to certain `TxSkelOut`
forceOutputs :: (Member MockChainWrite effs) => [TxSkelOut] -> Sem effs [Api.TxOutRef]
