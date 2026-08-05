{-# LANGUAGE TemplateHaskell #-}

-- | This module exposes primitives to manually (and artificially) update the
-- current state of the blockchain.
module Cooked.MockChain.Effect.Write
  ( -- * The `MockChainWrite` effect
    MockChainWrite (..),
    runMockChainWrite,

    -- * Modifications of the current time
    waitNSlots,
    awaitSlot,
    awaitEnclosingSlot,
    waitNMSFromSlotLowerBound,
    waitNMSFromSlotUpperBound,

    -- * Other operations
    setParams,
    setConstitutionScript,
    forceOutputs,
    forceOutputs_,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Api.Ledger qualified as Cardano
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Lens qualified as Lens
import Control.Monad
import Cooked.MockChain.Automation.AutoFilling.MinAda
import Cooked.MockChain.Automation.GenerateTx.Body
import Cooked.MockChain.Automation.GenerateTx.Output
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
import Ledger.Slot qualified as P.Ledger
import Ledger.Tx qualified as P.Ledger
import Ledger.Tx.CardanoAPI qualified as P.Ledger
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
  WaitNSlots :: Integer -> MockChainWrite m P.Ledger.Slot
  SetParams :: Emulator.Params -> MockChainWrite m ()
  SetConstitutionScript :: (ToVScript s) => s -> MockChainWrite m ()
  ForceOutputs :: [TxSkelOut] -> MockChainWrite m Utxos

makeSem_ ''MockChainWrite

-- | Interprets the `MockChainWrite` effect
runMockChainWrite ::
  forall effs a.
  ( Members
      '[ State EmulatorState,
         State ChainIndex,
         Error P.Ledger.ToCardanoError,
         Error MockChainError,
         MockChainLog,
         MockChainReadChain,
         MockChainReadConf
       ]
      effs
  ) =>
  Sem (MockChainWrite : effs) a ->
  Sem effs a
runMockChainWrite = interpret $ \case
  SetParams params -> do
    modify $ set emulatorStateParamsL params
    modify $ over emulatorStateLedgerStateL $ Emulator.updateStateParams params
  WaitNSlots n -> do
    cs <- gets (Emulator.getSlot . emulatorStateLedgerState)
    if
      | n == 0 -> return cs
      | n > 0 -> do
          let newSlot = cs + fromIntegral n
          modify' (over emulatorStateLedgerStateL $ Lens.set Emulator.elsSlotL $ fromIntegral newSlot)
          return newSlot
      | otherwise -> throw $ MCEPastSlot cs (cs + fromIntegral n)
  SetConstitutionScript (toVScript -> cScript) -> do
    modify' (chainIndexConstitutionL ?~ cScript)
    modify' $
      over emulatorStateLedgerStateL $
        Lens.set Emulator.elsConstitutionScriptL $
          (Cardano.SJust . Cardano.toShelleyScriptHash . Script.toCardanoScriptHash)
            cScript
  ForceOutputs outputs -> do
    -- We retrieve the protocol parameters
    params <- getParams
    -- We retrieve the network id
    networkId <- getNetworkId
    -- We adjust the outputs for the minimal required ADA if needed
    outputsMinAda <- mapM toTxSkelOutWithMinAda outputs
    -- We transform these outputs to Cardano outputs
    outputs' <- mapM toCardanoTxOut outputsMinAda
    -- We create our transaction body, which only consists of the dummy input
    -- and the outputs to force, and make a transaction out of it.
    cardanoTx <-
      P.Ledger.CardanoEmulatorEraTx . (`Cardano.Tx` [])
        <$> txBodyContentToTxBody
          ( P.Ledger.emptyTxBodyContent
              { Cardano.txOuts = outputs',
                -- The emulator takes for granted transactions with a single pseudo input,
                -- which we build to force transaction validation
                Cardano.txIns =
                  [ ( Cardano.genesisUTxOPseudoTxIn networkId $
                        Cardano.GenesisUTxOKeyHash $
                          Cardano.KeyHash "23d51e91ae5adc7ae801e9de4cd54175fb7464ec2680b25686bbb194",
                      Cardano.BuildTxWith $ Cardano.KeyWitness Cardano.KeyWitnessForSpending
                    )
                  ],
                Cardano.txProtocolParams = Cardano.BuildTxWith . Just . Cardano.LedgerProtocolParameters $ params
              }
          )
    -- We need to adjust our internal state to account for the forced
    -- transaction. We begin by computing the new map of outputs.
    let outputsMap =
          Map.fromList $
            zipWith
              (\x y -> (x, (y, True)))
              (P.Ledger.fromCardanoTxIn . snd <$> P.Ledger.getCardanoTxOutRefs cardanoTx)
              outputsMinAda
    -- We update the index, which effectively receives the new utxos
    modify'
      ( over emulatorStateLedgerStateL $
          Lens.over
            Emulator.elsUtxoL
            ( P.Ledger.fromPlutusIndex
                . P.Ledger.insert cardanoTx
                . P.Ledger.toPlutusIndex
            )
      )
    -- We update our internal map by adding the new outputs
    modify' (over chainIndexOutputsL (<> outputsMap))
    -- Finally, we return the created utxos
    return $ Map.toList (fst <$> outputsMap)

-- | Waits a certain number of slots and returns the new slot
waitNSlots :: (Member MockChainWrite effs) => Integer -> Sem effs P.Ledger.Slot

-- | Wait for a certain slot, or throws an error if the slot is already past
awaitSlot :: (Members '[MockChainReadChain, MockChainWrite] effs) => P.Ledger.Slot -> Sem effs P.Ledger.Slot
awaitSlot (P.Ledger.Slot targetSlot) = do
  P.Ledger.Slot now <- currentSlot
  waitNSlots (targetSlot - now)

-- | Waits until the current slot becomes greater or equal to the slot
--  containing the given POSIX time.  Note that that it might not wait for
--  anything if the current slot is large enough.
awaitEnclosingSlot :: (Members '[MockChainReadChain, MockChainWrite] effs) => Api.POSIXTime -> Sem effs P.Ledger.Slot
awaitEnclosingSlot time = getEnclosingSlot time >>= awaitSlot

-- | Wait a given number of ms from the lower bound of the current slot and
-- returns the current slot after waiting.
waitNMSFromSlotLowerBound :: (Members '[MockChainReadChain, MockChainWrite, Fail] effs) => Integer -> Sem effs P.Ledger.Slot
waitNMSFromSlotLowerBound duration = currentMSRange >>= awaitEnclosingSlot . (+ fromIntegral duration) . fst

-- | Wait a given number of ms from the upper bound of the current slot and
-- returns the current slot after waiting.
waitNMSFromSlotUpperBound :: (Members '[MockChainReadChain, MockChainWrite, Fail] effs) => Integer -> Sem effs P.Ledger.Slot
waitNMSFromSlotUpperBound duration = currentMSRange >>= awaitEnclosingSlot . (+ fromIntegral duration) . snd

-- | Updates the current parameters
setParams :: (Member MockChainWrite effs) => Emulator.Params -> Sem effs ()

-- | Sets the current script to act as the official constitution script
setConstitutionScript :: (Member MockChainWrite effs, ToVScript s) => s -> Sem effs ()

-- | Forces the generation of utxos corresponding to certain
-- `TxSkelOut`. Returns the created UTxOs, which might differ from the original
-- list if some min ADA adjustment occurred.
forceOutputs :: (Member MockChainWrite effs) => [TxSkelOut] -> Sem effs Utxos

-- | Same as `forceOutputs`, but discards the returned outputs
forceOutputs_ :: (Member MockChainWrite effs) => [TxSkelOut] -> Sem effs ()
forceOutputs_ = void . forceOutputs
