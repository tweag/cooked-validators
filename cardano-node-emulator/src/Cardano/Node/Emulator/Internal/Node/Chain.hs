{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Node.Emulator.Internal.Node.Chain where

import Cardano.Node.Emulator.Internal.Node.Params (Params)
import Cardano.Node.Emulator.Internal.Node.Validation qualified as Validation
import Control.Lens (makeLenses, makePrisms, over, view, (%~), (&), (.~))
import Control.Monad.Freer (Eff, Member, Members, send, type (~>))
import Control.Monad.Freer.Extras.Log (LogMsg, logDebug, logInfo, logWarn)
import Control.Monad.Freer.State (State, gets, modify)
import Control.Monad.State qualified as S
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (traverse_)
import Data.List ((\\))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Ledger
  ( Block,
    Blockchain,
    CardanoTx,
    OnChainTx,
    Slot,
    getCardanoTxId,
    unOnChain,
  )
import Ledger.Index qualified as Index
import Prettyprinter (Pretty (pretty), vsep, (<+>))

-- | Events produced by the blockchain emulator.
data ChainEvent
  = -- | A transaction has been validated and added to the blockchain.
    TxnValidation !Index.ValidationResult
  | SlotAdd !Slot
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty ChainEvent where
  pretty = \case
    TxnValidation res ->
      vsep
        ["TxnValidation" <+> pretty (getCardanoTxId $ Index.cardanoTxFromValidationResult res), pretty res]
    SlotAdd sl -> "SlotAdd" <+> pretty sl

chainEventOnChainTx :: ChainEvent -> Maybe OnChainTx
chainEventOnChainTx (TxnValidation result) = Index.toOnChain result
chainEventOnChainTx _ = Nothing

-- | A pool of transactions which have yet to be validated.
type TxPool = [CardanoTx]

data ChainState = ChainState
  { -- | The current chain, with the newest transactions first in the list.
    _chainNewestFirst :: !Blockchain,
    -- | The pool of pending transactions.
    _txPool :: !TxPool,
    -- | The UTxO index, used for validation.
    _index :: !Index.UtxoIndex,
    -- | The internal state of the ledger.
    _ledgerState :: Validation.EmulatedLedgerState
  }
  deriving (Show, Generic)

makeLenses ''ChainState

emptyChainState :: Params -> ChainState
emptyChainState params = ChainState [] [] mempty (Validation.initialState params)

fromBlockchain :: Params -> Blockchain -> ChainState
fromBlockchain params bc =
  emptyChainState params
    & chainNewestFirst .~ bc
    & index .~ Index.initialise bc

data ChainControlEffect r where
  ProcessBlock :: ChainControlEffect Block
  ModifySlot :: (Slot -> Slot) -> ChainControlEffect Slot

data ChainEffect r where
  QueueTx :: CardanoTx -> ChainEffect ()
  GetCurrentSlot :: ChainEffect Slot
  GetParams :: ChainEffect Params

-- | Make a new block
processBlock :: (Member ChainControlEffect effs) => Eff effs Block
processBlock = send ProcessBlock

-- | Adjust the current slot number, returning the new slot.
modifySlot :: (Member ChainControlEffect effs) => (Slot -> Slot) -> Eff effs Slot
modifySlot = send . ModifySlot

queueTx :: (Member ChainEffect effs) => CardanoTx -> Eff effs ()
queueTx tx = send (QueueTx tx)

getParams :: (Member ChainEffect effs) => Eff effs Params
getParams = send GetParams

getCurrentSlot :: (Member ChainEffect effs) => Eff effs Slot
getCurrentSlot = send GetCurrentSlot

type ChainEffs = '[State ChainState, LogMsg ChainEvent]

handleControlChain :: (Members ChainEffs effs) => Params -> ChainControlEffect ~> Eff effs
handleControlChain params = \case
  ProcessBlock -> do
    pool <- gets $ view txPool
    idx <- gets $ view index
    ls <- gets $ view ledgerState

    let ValidatedBlock block events idx' ls' =
          validateBlock params idx ls pool

    modify $ txPool .~ []
    modify $ index .~ idx'
    modify $ ledgerState .~ ls'
    modify $ addBlock block

    traverse_ logEvent events
    pure block
  ModifySlot f -> do
    _ <-
      modify @ChainState
        ( over
            ledgerState
            (Validation.updateSlot (\(Validation.SlotNo s) -> fromIntegral (f (fromIntegral s))))
        )
    gets (Validation.getSlot . view ledgerState)

logEvent :: (Member (LogMsg ChainEvent) effs) => ChainEvent -> Eff effs ()
logEvent e = case e of
  SlotAdd {} -> logDebug e
  TxnValidation Index.FailPhase1 {} -> logWarn e
  TxnValidation Index.FailPhase2 {} -> logWarn e
  TxnValidation Index.Success {} -> logInfo e

handleChain :: (Members ChainEffs effs) => Params -> ChainEffect ~> Eff effs
handleChain params = \case
  QueueTx tx -> modify (addTxToPool tx)
  GetCurrentSlot -> gets (Validation.getSlot . view ledgerState)
  GetParams -> pure params

-- | The result of validating a block.
data ValidatedBlock = ValidatedBlock
  { -- | The transactions that have been validated in this block.
    vlbValid :: !Block,
    -- | Transaction validation events for the transactions in this block.
    vlbEvents :: ![ChainEvent],
    -- | The updated UTxO index after processing the block
    vlbIndex :: !Index.UtxoIndex,
    vlbLedgerState :: !Validation.EmulatedLedgerState
  }

data ValidationCtx = ValidationCtx
  { vctxIndex :: !Index.UtxoIndex,
    vctxParams :: !Params,
    vctxLedgerState :: Validation.EmulatedLedgerState
  }

-- | Validate a block given the current slot and UTxO index, returning the valid
--  transactions, success/failure events and the updated UTxO set.
validateBlock ::
  Params -> Index.UtxoIndex -> Validation.EmulatedLedgerState -> TxPool -> ValidatedBlock
validateBlock params idx ls txns =
  let -- Validate transactions, updating the UTXO index each time
      (results, ValidationCtx idx' _ ls') =
        flip S.runState (ValidationCtx idx params ls) $ for txns validateEm

      -- The new block contains all transaction that were validated
      -- successfully
      block = mapMaybe Index.toOnChain results

      -- Also return an `EmulatorEvent` for each transaction that was
      -- processed
      nextSlot = Validation.getSlot ls + 1
      events = (TxnValidation <$> results) ++ [SlotAdd nextSlot]
   in ValidatedBlock block events idx' ls'

-- | Validate a transaction in the current emulator state.
validateEm ::
  (S.MonadState ValidationCtx m) =>
  CardanoTx ->
  m Index.ValidationResult
validateEm txn = do
  ctx@(ValidationCtx idx params ls) <- S.get
  let (ls', res) = Validation.validateCardanoTx params ls txn
      idx' = case res of
        Index.FailPhase1 {} -> idx
        Index.FailPhase2 {} -> Index.insertCollateral txn idx
        Index.Success {} -> Index.insert txn idx
  _ <- S.put ctx {vctxIndex = idx', vctxLedgerState = fromMaybe ls ls'}
  pure res

-- | Adds a block to ChainState, without validation.
addBlock :: Block -> ChainState -> ChainState
addBlock blk st =
  st
    & chainNewestFirst %~ (blk :)
    -- The block update may contain txs that are not in this client's
    -- `txPool` which will get ignored
    & txPool %~ (\\ map unOnChain blk)

addTxToPool :: CardanoTx -> ChainState -> ChainState
addTxToPool tx = over txPool (tx :)

makePrisms ''ChainEvent
