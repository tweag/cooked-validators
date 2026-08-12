{-# LANGUAGE TemplateHaskell #-}

-- | This module exposes primitives to manually (and artificially) update the
-- current state of the blockchain.
module Cooked.Effect.Override
  ( -- * The `Override` effect
    Override (..),
    runMockChainOverride,

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
import Cooked.Automation.AutoFilling.MinAda
import Cooked.Automation.GenerateTx.Body
import Cooked.Automation.GenerateTx.Output
import Cooked.Effect.Log
import Cooked.Effect.Params
import Cooked.Effect.Query
import Cooked.Runtime.Error
import Cooked.Runtime.State
import Cooked.Skeleton
import Cooked.Utilities.Aliases
import Data.Map.Strict qualified as Map
import Ledger.Index qualified as P.Ledger
import Ledger.Orphans ()
import Ledger.Tx qualified as P.Ledger
import Ledger.Tx.CardanoAPI qualified as P.Ledger
import Optics.Core
import Plutus.Script.Utils.Scripts qualified as Script
import Polysemy
import Polysemy.Error
import Polysemy.State

-- | An effect that offers all the primitives that are performing modifications
-- on the blockchain state.
data Override :: Effect where
  SetParams :: Emulator.Params -> Override m ()
  SetConstitutionScript :: (ToVScript s) => s -> Override m ()
  ForceOutputs :: [TxSkelOut] -> Override m Utxos

makeSem_ ''Override

-- | Interprets the `Override` effect
runMockChainOverride ::
  forall effs a.
  ( Members
      '[ State EmulatorState,
         State ChainIndex,
         Error P.Ledger.ToCardanoError,
         Error ChainError,
         Log,
         Query,
         Params
       ]
      effs
  ) =>
  Sem (Override : effs) a ->
  Sem effs a
runMockChainOverride = interpret $ \case
  SetParams params -> do
    modify $ set emulatorStateParamsL params
    modify $ over emulatorStateLedgerStateL $ Emulator.updateStateParams params
  SetConstitutionScript (toVScript -> cScript) -> do
    modify' $ chainIndexConstitutionL ?~ cScript
    modify' $
      over emulatorStateLedgerStateL $
        Lens.set
          Emulator.elsConstitutionScriptL
          (Cardano.SJust $ Cardano.toShelleyScriptHash $ Script.toCardanoScriptHash cScript)
  ForceOutputs outputs -> do
    -- We adjust the outputs for the minimal required ADA if needed
    outputsMinAda <- mapM toTxSkelOutWithMinAda outputs
    -- We transform these outputs to Cardano outputs
    outputs' <- mapM toCardanoTxOut outputsMinAda
    -- We create our transaction body, composed of the forced outputs
    cardanoTx <-
      P.Ledger.CardanoEmulatorEraTx . (`Cardano.Tx` [])
        <$> txBodyContentToTxBody (P.Ledger.emptyTxBodyContent {Cardano.txOuts = outputs'})
    -- We need to adjust our internal state to account for the forced
    -- transaction. We begin by computing the new outputs.
    let outputsList = zip (P.Ledger.fromCardanoTxIn . snd <$> P.Ledger.getCardanoTxOutRefs cardanoTx) outputsMinAda
    -- We update the index, which effectively receives the new utxos
    modify' $
      over emulatorStateLedgerStateL $
        Lens.over Emulator.elsUtxoL $
          P.Ledger.fromPlutusIndex
            . P.Ledger.insert cardanoTx
            . P.Ledger.toPlutusIndex
    -- We update our internal map by adding the new outputs
    modify' $ addOutputs outputsList
    -- Finally, we return the created utxos
    return $ Map.fromList outputsList

-- | Updates the current parameters
setParams :: (Member Override effs) => Emulator.Params -> Sem effs ()

-- | Sets the current script to act as the official constitution script
setConstitutionScript :: (Member Override effs, ToVScript s) => s -> Sem effs ()

-- | Forces the generation of utxos corresponding to certain
-- `TxSkelOut`. Returns the created UTxOs, which might differ from the original
-- list if some min ADA adjustment occurred.
forceOutputs :: (Member Override effs) => [TxSkelOut] -> Sem effs Utxos

-- | Same as `forceOutputs`, but discards the returned outputs
forceOutputs_ :: (Member Override effs) => [TxSkelOut] -> Sem effs ()
forceOutputs_ = void . forceOutputs
