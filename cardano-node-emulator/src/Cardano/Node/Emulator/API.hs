{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | If you want to run the node emulator without using the `Contract` monad, this module provides a simple MTL-based interface.
module Cardano.Node.Emulator.API
  ( -- * Updating the blockchain
    queueTx,
    nextSlot,
    currentSlot,
    currentTimeRange,
    awaitSlot,
    awaitTime,

    -- * Querying the blockchain
    utxosAt,
    utxosAtPlutus,
    utxoAtTxIn,
    utxosAtTxIns,
    utxoAtTxOutRef,
    fundsAt,
    lookupDatum,
    getMemPoolEnv,
    getLedgerState,

    -- * Transactions
    signTx,

    -- * Logging
    logDebug,
    logInfo,
    logWarn,
    logError,

    -- * Types
    EmulatorEra,
    EmulatorState (EmulatorState),
    esChainState,
    esAddressMap,
    esDatumMap,
    EmulatedLedgerState,
    ledgerEnv,
    memPoolState,
    EmulatorError (..),
    EmulatorLogs,
    EmulatorMsg (..),
    L.LogMessage (..),
    MonadEmulator,
    EmulatorT,
    EmulatorM,
    emptyEmulatorState,
    emptyEmulatorStateWithInitialDist,
    Params (..),
    getParams,
  )
where

import Cardano.Api qualified as C
import Cardano.Ledger.Shelley.API
  ( LedgerState,
    MempoolEnv,
  )
import Cardano.Node.Emulator.Internal.API
  ( EmulatorError (CustomError, ToCardanoError, ValidationError),
    EmulatorLogs,
    EmulatorM,
    EmulatorState (EmulatorState),
    EmulatorT,
    MonadEmulator,
    esAddressMap,
    esChainState,
    esDatumMap,
    handleChain,
    modifySlot,
    processBlock,
  )
import Cardano.Node.Emulator.Internal.Node
  ( EmulatedLedgerState,
    Params (pConfig, pSlotConfig),
    ledgerEnv,
    memPoolState,
    posixTimeToEnclosingSlot,
    slotToBeginPOSIXTime,
    slotToEndPOSIXTime,
  )
import Cardano.Node.Emulator.Internal.Node.Chain qualified as E
  ( chainNewestFirst,
    emptyChainState,
    getCurrentSlot,
    index,
    ledgerState,
    queueTx,
  )
import Cardano.Node.Emulator.Internal.Node.Params qualified as E (Params)
import Cardano.Node.Emulator.Internal.Node.Validation qualified as E
  ( ledgerEnv,
    memPoolState,
    setUtxo,
    unsafeMakeValid,
  )
import Cardano.Node.Emulator.LogMessages
  ( EmulatorMsg (ChainEvent, GenericMsg, TxBalanceMsg),
    TxBalanceMsg (SigningTx, SubmittingTx),
  )
import Control.Lens (use, (%~), (&), (.~), (<>~), (^.))
import Control.Monad (void)
import Control.Monad.Freer.Extras.Log qualified as L
import Control.Monad.RWS.Class (ask, asks, get, tell)
import Data.Aeson (ToJSON (toJSON))
import Data.Map (Map)
import Data.Map qualified as Map
import Ledger
  ( CardanoAddress,
    Datum,
    DatumHash,
    DecoratedTxOut,
    POSIXTime,
    Slot,
    TxOutRef,
    UtxoIndex,
  )
import Ledger.AddressMap qualified as AM
import Ledger.Index qualified as Index
import Ledger.Tx
  ( CardanoTx,
    TxOut,
    addCardanoTxWitness,
    cardanoTxOutValue,
    getCardanoTxData,
    toCtxUTxOTxOut,
    toDecoratedTxOut,
  )
import Ledger.Tx.CardanoAPI
  ( EmulatorEra,
    fromCardanoTxIn,
    fromPlutusIndex,
    toCardanoTxIn,
  )

emptyEmulatorState :: E.Params -> EmulatorState
emptyEmulatorState params = EmulatorState (E.emptyChainState params) mempty mempty

emptyEmulatorStateWithInitialDist :: E.Params -> Map CardanoAddress C.Value -> EmulatorState
emptyEmulatorStateWithInitialDist params initialDist =
  let tx = Index.createGenesisTransaction initialDist
      vtx = E.unsafeMakeValid tx
      index = Index.insertBlock [vtx] mempty
   in emptyEmulatorState params
        & esChainState . E.chainNewestFirst %~ ([vtx] :)
        & esChainState . E.index .~ index
        & esChainState . E.ledgerState %~ E.setUtxo params (fromPlutusIndex index)
        & esAddressMap %~ AM.updateAllAddresses vtx
        & esDatumMap <>~ getCardanoTxData tx

getParams :: (MonadEmulator m) => m E.Params
getParams = ask

-- | Queue the transaction, it will be processed when @nextSlot@ is called.
queueTx :: (MonadEmulator m) => CardanoTx -> m ()
queueTx tx = do
  logMsg L.Info $ TxBalanceMsg $ SubmittingTx tx
  handleChain (E.queueTx tx)

-- | Process the queued transactions and increase the slot number.
nextSlot :: (MonadEmulator m) => m ()
nextSlot = void $ processBlock >> modifySlot succ

-- | Get the current slot number of the emulated node.
currentSlot :: (MonadEmulator m) => m Slot
currentSlot = handleChain E.getCurrentSlot

-- | Get the time range of the current slot of the emulated node.
currentTimeRange :: (MonadEmulator m) => m (POSIXTime, POSIXTime)
currentTimeRange = do
  slotConfig <- asks pSlotConfig
  slot <- currentSlot
  pure
    ( slotToBeginPOSIXTime slotConfig slot,
      slotToEndPOSIXTime slotConfig slot
    )

-- | Call `nextSlot` until the current slot number equals or exceeds the given slot number.
awaitSlot :: (MonadEmulator m) => Slot -> m ()
awaitSlot s = do
  c <- currentSlot
  if s <= c
    then pure ()
    else do
      nextSlot
      awaitSlot s

-- | Call `nextSlot` until the given time has been reached.
awaitTime :: (MonadEmulator m) => POSIXTime -> m ()
awaitTime t = do
  slotConfig <- asks pSlotConfig
  awaitSlot (posixTimeToEnclosingSlot slotConfig t + 1)

-- | Query the unspent transaction outputs at the given address.
utxosAt :: (MonadEmulator m) => CardanoAddress -> m UtxoIndex
utxosAt addr = do
  es <- get
  pure $ C.UTxO $ Map.map (toCtxUTxOTxOut . snd) $ es ^. esAddressMap . AM.fundsAt addr

-- | Query the unspent transaction outputs at the given address (using Plutus types).
utxosAtPlutus :: (MonadEmulator m) => CardanoAddress -> m (Map TxOutRef DecoratedTxOut)
utxosAtPlutus addr = do
  es <- get
  pure $
    Map.mapKeys fromCardanoTxIn $
      Map.mapMaybe (toDecoratedTxOut . snd) $
        es ^. esAddressMap . AM.fundsAt addr

-- | Query the unspent transaction outputs at the given transaction inputs.
utxosAtTxIns :: (MonadEmulator m, Foldable f) => f C.TxIn -> m UtxoIndex
utxosAtTxIns txIns = do
  idx <- use (esChainState . E.index)
  pure $ foldMap (\txIn -> maybe mempty (Index.singleton txIn) $ Index.lookupUTxO txIn idx) txIns

-- | Resolve the transaction input.
utxoAtTxIn :: (MonadEmulator m) => C.TxIn -> m (Maybe TxOut)
utxoAtTxIn txIn = Index.lookup txIn <$> use (esChainState . E.index)

-- | Resolve the transaction output reference (using Plutus types).
utxoAtTxOutRef :: (MonadEmulator m) => TxOutRef -> m (Maybe DecoratedTxOut)
utxoAtTxOutRef ref = either (const $ pure Nothing) findTxOut (toCardanoTxIn ref)
  where
    findTxOut txIn = do
      mTxOut <- utxoAtTxIn txIn
      pure $ mTxOut >>= toDecoratedTxOut

-- | Query the total value of the unspent transaction outputs at the given address.
fundsAt :: (MonadEmulator m) => CardanoAddress -> m C.Value
fundsAt addr = foldMap cardanoTxOutValue . C.unUTxO <$> utxosAt addr

-- | Resolve a datum hash to an actual datum, if known.
lookupDatum :: (MonadEmulator m) => DatumHash -> m (Maybe Datum)
lookupDatum h = do
  es <- get
  pure $ Map.lookup h (es ^. esDatumMap)

-- | Get the internal ledger state.
getLedgerState :: (MonadEmulator m) => m (LedgerState EmulatorEra)
getLedgerState = do
  es <- get
  pure $ es ^. esChainState . E.ledgerState . E.memPoolState

-- | Get the internal mempool environment.
getMemPoolEnv :: (MonadEmulator m) => m (MempoolEnv EmulatorEra)
getMemPoolEnv = do
  es <- get
  pure $ es ^. esChainState . E.ledgerState . E.ledgerEnv

-- | Sign a transaction with the given signatures.
signTx ::
  (MonadEmulator m, Foldable f) =>
  -- | Signatures
  f C.ShelleyWitnessSigningKey ->
  CardanoTx ->
  m CardanoTx
signTx witnesses tx = do
  logMsg L.Info $ TxBalanceMsg $ SigningTx tx
  pure $ foldr addCardanoTxWitness tx witnesses

-- | Log any message
logMsg :: (MonadEmulator m) => L.LogLevel -> EmulatorMsg -> m ()
logMsg l = tell . pure . L.LogMessage l

-- | Log a message at the 'Debug' level
logDebug :: (ToJSON a, MonadEmulator m) => a -> m ()
logDebug = logMsg L.Debug . GenericMsg . toJSON

-- | Log a message at the 'Info' level
logInfo :: (ToJSON a, MonadEmulator m) => a -> m ()
logInfo = logMsg L.Info . GenericMsg . toJSON

-- | Log a message at the 'Warning' level
logWarn :: (ToJSON a, MonadEmulator m) => a -> m ()
logWarn = logMsg L.Warning . GenericMsg . toJSON

-- | Log a message at the 'Error' level
logError :: (ToJSON a, MonadEmulator m) => a -> m ()
logError = logMsg L.Error . GenericMsg . toJSON
