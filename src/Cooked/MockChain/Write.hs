{-# LANGUAGE TemplateHaskell #-}

-- | This module exposes primitives to update the current state of the
-- blockchain, including by sending transactions for validation.
module Cooked.MockChain.Write
  ( -- * The `MockChainWrite` effect
    MockChainWrite,
    reinterpretMockChainWriteWithTweaks,
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

import Cardano.Node.Emulator qualified as Emulator
import Cooked.Ltl
import Cooked.Skeleton
import Cooked.Tweak
import Ledger.Slot qualified as Ledger
import Ledger.Tx qualified as Ledger
import PlutusLedgerApi.V3 qualified as Api
import Polysemy

-- | An effect that offers all the primitives that are performing modifications
-- on the blockchain state.
data MockChainWrite :: Effect where
  WaitNSlots :: Integer -> MockChainWrite m Ledger.Slot
  SetParams :: Emulator.Params -> MockChainWrite m ()
  ValidateTxSkel :: TxSkel -> MockChainWrite m Ledger.CardanoTx
  SetConstitutionScript :: (ToVScript s) => s -> MockChainWrite m ()
  ForceOutputs :: [TxSkelOut] -> MockChainWrite m [Api.TxOutRef]

makeSem_ ''MockChainWrite

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
    let sumTweak :: Sem (Tweak : NonDet : tweakEffs) () =
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
  ValidateTxSkel skel -> do
    undefined
  ForceOutputs outs -> undefined
  builtin -> undefined

-- | Waits a certain number of slots and returns the new slot
waitNSlots :: (Member MockChainWrite effs) => Integer -> Sem effs Ledger.Slot

-- | Wait for a certain slot, or throws an error if the slot is already past
awaitSlot :: (Member MockChainWrite effs, Integral i) => i -> m Ledger.Slot
awaitSlot slot = currentSlot >>= waitNSlots . (slot -) . fromIntegral

-- | Waits until the current slot becomes greater or equal to the slot
--  containing the given POSIX time.  Note that that it might not wait for
--  anything if the current slot is large enough.
awaitEnclosingSlot :: (Member MockChainWrite effs) => Api.POSIXTime -> m Ledger.Slot
awaitEnclosingSlot = awaitSlot <=< getEnclosingSlot

-- | Wait a given number of ms from the lower bound of the current slot and
-- returns the current slot after waiting.
waitNMSFromSlotLowerBound :: (Member MockChainWrite effs, Integral i) => i -> m Ledger.Slot
waitNMSFromSlotLowerBound duration = currentMSRange >>= awaitEnclosingSlot . (+ fromIntegral duration) . fst

-- | Wait a given number of ms from the upper bound of the current slot and
-- returns the current slot after waiting.
waitNMSFromSlotUpperBound :: (Member MockChainWrite effs, Integral i) => i -> m Ledger.Slot
waitNMSFromSlotUpperBound duration = currentMSRange >>= awaitEnclosingSlot . (+ fromIntegral duration) . snd

-- | Generates, balances and validates a transaction from a skeleton, and
-- returns the validated transaction.
validateTxSkel :: (Member MockChainWrite effs) => TxSkel -> Sem effs Ledger.CardanoTx

-- | Same as `validateTxSkel`, but only returns the generated UTxOs
validateTxSkel' :: (Member MockChainWrite effs) => TxSkel -> m [Api.TxOutRef]
validateTxSkel' = ((fmap fst <$>) . utxosFromCardanoTx) <=< validateTxSkel

-- | Same as `validateTxSkel`, but discards the returned transaction
validateTxSkel_ :: (Member MockChainWrite effs) => TxSkel -> m ()
validateTxSkel_ = void . validateTxSkel

-- | Updates the current parameters
setParams :: (Member MockChainWrite effs) => Emulator.Params -> Sem effs ()

-- | Sets the current script to act as the official constitution script
setConstitutionScript :: (Member MockChainWrite effs, ToVScript s) => s -> Sem eff ()

-- | Forces the generation of utxos corresponding to certain `TxSkelOut`
forceOutputs :: (Member MockChainWrite effs) => [TxSkelOut] -> Sem effs [Api.TxOutRef]
