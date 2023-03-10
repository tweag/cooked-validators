{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.BlockChain
  ( MockChainError (..),
    MCEUnbalanceableError (..),
    MonadBlockChainBalancing (..),
    MonadBlockChainWithoutValidation (..),
    MonadBlockChain (..),
    AsTrans (..),
    allUtxos,
    currentTime,
    waitNSlots,
    utxosAt,
    txOutByRef,
    utxosFromCardanoTx,
    txOutV2FromLedger,
    typedDatumFromTxOutRef,
    valueFromTxOutRef,
    outputDatumFromTxOutRef,
    datumFromTxOutRef,
    resolveDatum,
    resolveValidator,
    resolveReferenceScript,
    getEnclosingSlot,
    awaitEnclosingSlot,
    slotRangeBefore,
    slotRangeAfter,
    slotToTimeInterval,
  )
where

import qualified Cardano.Node.Emulator as Emulator
import Control.Arrow
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Trans.Writer
import Cooked.MockChain.GenerateTx (GenerateTxError)
import Cooked.Output
import Cooked.Skeleton
import Cooked.Wallet
import Data.Kind
import qualified Ledger.Index as Ledger
import qualified Ledger.Slot as Ledger
import qualified Ledger.Tx as Ledger
import qualified Ledger.Tx.CardanoAPI as Ledger
import ListT
import Optics.Core
import qualified Plutus.Script.Utils.Scripts as Pl
import qualified Plutus.V2.Ledger.Api as PV2

-- * BlockChain monad

-- | The errors that can be produced by the 'MockChainT' monad
data MockChainError where
  MCEValidationError :: Ledger.ValidationErrorInPhase -> MockChainError
  MCEUnbalanceable :: MCEUnbalanceableError -> TxSkel -> MockChainError
  -- | Thrown when the balancing wallet owns no output that is pure Ada and
  -- with no datum.
  MCENoSuitableCollateral :: MockChainError
  MCEGenerationError :: GenerateTxError -> MockChainError
  -- | Errors happening at fee calculation time.
  MCECalcFee :: MockChainError -> MockChainError
  -- | Thrown when an output reference should be in the state of the mockchain,
  -- but isn't.
  MCEUnknownOutRefError :: String -> PV2.TxOutRef -> MockChainError
  -- | Same as 'MCEUnknownOutRefError' for validators.
  MCEUnknownValidator :: String -> PV2.ValidatorHash -> MockChainError
  -- | Same as 'MCEUnknownOutRefError' for datums.
  MCEUnknownDatum :: String -> PV2.DatumHash -> MockChainError
  -- | Used to provide 'MonadFail' instances.
  FailWith :: String -> MockChainError
  OtherMockChainError :: (Show err, Eq err) => err -> MockChainError

data MCEUnbalanceableError
  = -- | The balancing wallet misses some value to pay what is needed to balance
    -- the transaction.
    MCEUnbalNotEnoughFunds Wallet PV2.Value
  | -- | There is value to return to the balancing wallet but not enough to
    -- fullfill the min ada requirement and there is not enough in additional
    -- inputs to make it possible.
    MCEUnbalNotEnoughReturning
      (PV2.Value, [PV2.TxOutRef]) -- What was spent
      (PV2.Value, [PV2.TxOutRef]) -- What is left to spend
      PV2.Value -- What cannot be given back
  deriving (Show)

deriving instance Show MockChainError

instance Eq MockChainError where
  (==) = undefined

-- | Contains methods needed for balancing.
class (MonadFail m, MonadError MockChainError m) => MonadBlockChainBalancing m where
  -- | Returns the parameters of the chain.
  getParams :: m Emulator.Params

  -- | Returns a list of all UTxOs at a certain address.
  utxosAtLedger :: PV2.Address -> m [(PV2.TxOutRef, Ledger.TxOut)]

  -- | Returns the datum with the given hash or 'Nothing' if there is none
  datumFromHash :: PV2.DatumHash -> m (Maybe PV2.Datum)

  -- | Returns the full validator corresponding to hash, if that validator owns
  -- something or if it is stored in the reference script field of some UTxO.
  validatorFromHash :: PV2.ValidatorHash -> m (Maybe (Pl.Versioned PV2.Validator))

  -- | Returns an output given a reference to it
  txOutByRefLedger :: PV2.TxOutRef -> m (Maybe Ledger.TxOut)

class MonadBlockChainBalancing m => MonadBlockChainWithoutValidation m where
  -- | Returns a list of all currently known outputs.
  allUtxosLedger :: m [(PV2.TxOutRef, Ledger.TxOut)]

  -- | Returns the current slot number
  currentSlot :: m Ledger.Slot

  -- | Waits until the current slot becomes greater or equal to the given slot,
  -- and returns the current slot after waiting.
  --
  -- Note that it might not wait for anything if the current slot is large
  -- enough.
  awaitSlot :: Ledger.Slot -> m Ledger.Slot

-- | The main abstraction of the blockchain.
class MonadBlockChainWithoutValidation m => MonadBlockChain m where
  -- | Generates, balances and validates a transaction from a skeleton.
  -- It returns the validated transaction and updates the state of the
  -- blockchain. In 'MockChainT', this means:
  -- - deletes the consumed outputs from 'mcstIndex'
  -- - adds the produced outputs to 'msctIndex'
  -- - deletes the consumed datums from 'mcstDatums'
  -- - adds the produced datums to 'mcstDatums'
  -- - adds the validators on outputs to the 'mcstValidators'.
  validateTxSkel :: TxSkel -> m Ledger.CardanoTx

allUtxos :: MonadBlockChainWithoutValidation m => m [(PV2.TxOutRef, PV2.TxOut)]
allUtxos = fmap (second txOutV2FromLedger) <$> allUtxosLedger

utxosAt :: MonadBlockChainBalancing m => PV2.Address -> m [(PV2.TxOutRef, PV2.TxOut)]
utxosAt address = fmap (second txOutV2FromLedger) <$> utxosAtLedger address

txOutByRef :: MonadBlockChainBalancing m => PV2.TxOutRef -> m (Maybe PV2.TxOut)
txOutByRef oref = fmap txOutV2FromLedger <$> txOutByRefLedger oref

-- | Retrieve the ordered list of outputs of the given "CardanoTx".
--
-- This is useful when writing endpoints and/or traces to fetch utxos of
-- interest right from the start and avoid querying the chain for them
-- afterwards using 'allUtxos' or similar functions.
utxosFromCardanoTx :: Ledger.CardanoTx -> [(PV2.TxOutRef, PV2.TxOut)]
utxosFromCardanoTx =
  map (\(txOut, txOutRef) -> (txOutRef, txOutV2FromLedger txOut)) . Ledger.getCardanoTxOutRefs

txOutV2FromLedger :: Ledger.TxOut -> PV2.TxOut
txOutV2FromLedger = Ledger.fromCardanoTxOutToPV2TxInfoTxOut . Ledger.getTxOut

-- | Try to resolve the datum on the output: If there's an inline datum, take
-- that; if there's a datum hash, look the corresponding datum up (with
-- 'datumFromHash'), returning @Nothing@ if it can't be found; if there's no
-- datum or hash at all, return @Nothing@.
resolveDatum ::
  ( IsAbstractOutput out,
    ToOutputDatum (DatumType out),
    MonadBlockChainBalancing m
  ) =>
  out ->
  m (Maybe (ConcreteOutput (OwnerType out) PV2.Datum (ValueType out) (ReferenceScriptType out)))
resolveDatum out =
  case outputOutputDatum out of
    PV2.OutputDatumHash datumHash -> do
      mDatum <- datumFromHash datumHash
      case mDatum of
        Nothing -> return Nothing
        Just datum ->
          return . Just $
            ConcreteOutput
              (out ^. outputOwnerL)
              (out ^. outputStakingCredentialL)
              (out ^. outputValueL)
              datum
              (out ^. outputReferenceScriptL)
    PV2.OutputDatum datum ->
      return . Just $
        ConcreteOutput
          (out ^. outputOwnerL)
          (out ^. outputStakingCredentialL)
          (out ^. outputValueL)
          datum
          (out ^. outputReferenceScriptL)
    PV2.NoOutputDatum -> return Nothing

-- | Try to resolve the validator that owns an output: If the output is owned by
-- a public key, or if the validator's hash is not known (i.e. if
-- 'validatorFromHash' returns @Nothing@) return @Nothing@.
resolveValidator ::
  ( IsAbstractOutput out,
    ToCredential (OwnerType out),
    MonadBlockChainBalancing m
  ) =>
  out ->
  m (Maybe (ConcreteOutput (Pl.Versioned PV2.Validator) (DatumType out) (ValueType out) (ReferenceScriptType out)))
resolveValidator out =
  case toCredential (out ^. outputOwnerL) of
    PV2.PubKeyCredential _ -> return Nothing
    PV2.ScriptCredential valHash -> do
      mVal <- validatorFromHash valHash
      case mVal of
        Nothing -> return Nothing
        Just val ->
          return . Just $
            ConcreteOutput
              val
              (out ^. outputStakingCredentialL)
              (out ^. outputValueL)
              (out ^. outputDatumL)
              (out ^. outputReferenceScriptL)

-- | Try to resolve the reference script on an output: If the output has no
-- reference script, or if the reference script's hash is not known (i.e. if
-- 'validatorFromHash' returns @Nothing@), this function will return @Nothing@.
resolveReferenceScript ::
  ( IsAbstractOutput out,
    ToScriptHash (ReferenceScriptType out),
    MonadBlockChainBalancing m
  ) =>
  out ->
  m (Maybe (ConcreteOutput (OwnerType out) (DatumType out) (ValueType out) (Pl.Versioned PV2.Validator)))
resolveReferenceScript out =
  case outputReferenceScriptHash out of
    Nothing -> return Nothing
    Just (PV2.ScriptHash hash) -> do
      mVal <- validatorFromHash (PV2.ValidatorHash hash)
      case mVal of
        Nothing -> return Nothing
        Just val ->
          return . Just $
            ConcreteOutput
              (out ^. outputOwnerL)
              (out ^. outputStakingCredentialL)
              (out ^. outputValueL)
              (out ^. outputDatumL)
              (Just val)

outputDatumFromTxOutRef :: MonadBlockChainWithoutValidation m => PV2.TxOutRef -> m (Maybe PV2.OutputDatum)
outputDatumFromTxOutRef oref = do
  mOut <- txOutByRef oref
  case mOut of
    Nothing -> return Nothing
    Just out -> return . Just $ outputOutputDatum out

datumFromTxOutRef :: MonadBlockChainWithoutValidation m => PV2.TxOutRef -> m (Maybe PV2.Datum)
datumFromTxOutRef oref = do
  mOutputDatum <- outputDatumFromTxOutRef oref
  case mOutputDatum of
    Nothing -> return Nothing
    Just PV2.NoOutputDatum -> return Nothing
    Just (PV2.OutputDatum datum) -> return $ Just datum
    Just (PV2.OutputDatumHash datumHash) -> do
      mDatum <- datumFromHash datumHash
      case mDatum of
        Just datum -> return $ Just datum
        Nothing -> return Nothing

typedDatumFromTxOutRef :: (PV2.FromData a, MonadBlockChainWithoutValidation m) => PV2.TxOutRef -> m (Maybe a)
typedDatumFromTxOutRef oref = do
  mDatum <- datumFromTxOutRef oref
  case mDatum of
    Nothing -> return Nothing
    Just (PV2.Datum datum) -> return $ PV2.fromBuiltinData datum

valueFromTxOutRef :: MonadBlockChainWithoutValidation m => PV2.TxOutRef -> m (Maybe PV2.Value)
valueFromTxOutRef oref = do
  mOut <- txOutByRef oref
  case mOut of
    Nothing -> return Nothing
    Just out -> return . Just $ outputValue out

-- ** Slot and Time Management

-- $slotandtime
-- #slotandtime#
--
-- Slots are integers that monotonically increase and model the passage of time. By looking
-- at the current slot, a validator gets to know that it is being executed within a certain
-- window of wall-clock time. Things can get annoying pretty fast when trying to mock traces
-- and trying to exercise certain branches of certain validators; make sure you also read
-- the docs on 'autoSlotIncrease' to be able to simulate sending transactions in parallel.

-- | Moves n slots fowards
waitNSlots :: (MonadBlockChainWithoutValidation m) => Integer -> m Ledger.Slot
waitNSlots n =
  if n < 0
    then fail "waitNSlots: negative argument"
    else currentSlot >>= awaitSlot . (+ fromIntegral n)

-- | Returns the closed ms interval corresponding to the current slot
currentTime :: (MonadBlockChainWithoutValidation m) => m (PV2.POSIXTime, PV2.POSIXTime)
currentTime = slotToTimeInterval =<< currentSlot

-- | Returns the closed ms interval corresponding to the slot with the given
-- number. It holds that
--
-- > slotToTimeInterval (getEnclosingSlot t) == (a, b)    ==>   a <= t <= b
--
-- and
--
-- > slotToTimeInterval n == (a, b)   ==>   getEnclosingSlot a == n && getEnclosingSlot b == n
--
-- and
--
-- > slotToTimeInterval n == (a, b)   ==>   getEnclosingSlot (a-1) == n-1 && getEnclosingSlot (b+1) == n+1
slotToTimeInterval :: (MonadBlockChainWithoutValidation m) => Ledger.Slot -> m (PV2.POSIXTime, PV2.POSIXTime)
slotToTimeInterval slot = do
  slotConfig <- Emulator.pSlotConfig <$> getParams
  case Emulator.slotToPOSIXTimeRange slotConfig slot of
    PV2.Interval
      (PV2.LowerBound (PV2.Finite l) leftclosed)
      (PV2.UpperBound (PV2.Finite r) rightclosed) ->
        return
          ( if leftclosed then l else l + 1,
            if rightclosed then r else r - 1
          )
    _ -> error "The time interval corresponding to a slot should be finite on both ends."

-- | Return the slot that contains the given time. See 'slotToTimeInterval' for
-- some equational properties this function satisfies.
getEnclosingSlot :: (MonadBlockChainWithoutValidation m) => PV2.POSIXTime -> m Ledger.Slot
getEnclosingSlot t = do
  slotConfig <- Emulator.pSlotConfig <$> getParams
  return $ Emulator.posixTimeToEnclosingSlot slotConfig t

-- | Waits until the current slot becomes greater or equal to the slot containing the given POSIX time.
--  Note that that it might not wait for anything if the current slot is large enough.
awaitEnclosingSlot :: (MonadBlockChainWithoutValidation m) => PV2.POSIXTime -> m Ledger.Slot
awaitEnclosingSlot = awaitSlot <=< getEnclosingSlot

-- | The infinite range of slots ending before or at the given POSIX time
slotRangeBefore :: MonadBlockChainWithoutValidation m => PV2.POSIXTime -> m Ledger.SlotRange
slotRangeBefore t = do
  n <- getEnclosingSlot t
  (_, b) <- slotToTimeInterval n
  -- If the given time @t@ happens to be the last millisecond of its slot, we
  -- can include the whole slot. Otherwise, the only way to be sure that the
  -- returned slot range contains no time after @t@ is to go to the preceding
  -- slot.
  if t == b
    then return $ PV2.to n
    else return $ PV2.to (n - 1)

-- | The infinite range of slots starting after or at the given POSIX time
slotRangeAfter :: MonadBlockChainWithoutValidation m => PV2.POSIXTime -> m Ledger.SlotRange
slotRangeAfter t = do
  n <- getEnclosingSlot t
  (a, _) <- slotToTimeInterval n
  if t == a
    then return $ PV2.from n
    else return $ PV2.from (n + 1)

-- ** Deriving further 'MonadBlockChain' instances

-- | A newtype wrapper to be used with '-XDerivingVia' to derive instances of 'MonadBlockChain'
-- for any 'MonadTransControl'.
--
-- For example, to derive 'MonadBlockChain m => MonadBlockChain (ReaderT r m)', you'd write
--
-- > deriving via (AsTrans (ReaderT r) m) instance MonadBlockChain m => MonadBlockChain (ReaderT r m)
--
-- and avoid the boilerplate of defining all the methods of the class yourself.
newtype AsTrans t (m :: Type -> Type) a = AsTrans {getTrans :: t m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadTransControl)

instance (MonadTrans t, MonadFail m, Monad (t m)) => MonadFail (AsTrans t m) where
  fail = lift . fail

instance (MonadTransControl t, MonadError MockChainError m, Monad (t m)) => MonadError MockChainError (AsTrans t m) where
  throwError = lift . throwError
  catchError act f = liftWith (\run -> catchError (run act) (run . f)) >>= restoreT . return

instance (MonadTrans t, MonadBlockChainBalancing m, Monad (t m), MonadError MockChainError (AsTrans t m)) => MonadBlockChainBalancing (AsTrans t m) where
  getParams = lift getParams
  validatorFromHash = lift . validatorFromHash
  utxosAtLedger = lift . utxosAtLedger
  txOutByRefLedger = lift . txOutByRefLedger
  datumFromHash = lift . datumFromHash

instance (MonadTrans t, MonadBlockChainWithoutValidation m, Monad (t m), MonadError MockChainError (AsTrans t m)) => MonadBlockChainWithoutValidation (AsTrans t m) where
  allUtxosLedger = lift allUtxosLedger
  currentSlot = lift currentSlot
  awaitSlot = lift . awaitSlot

instance (MonadTrans t, MonadBlockChain m, MonadBlockChainWithoutValidation (AsTrans t m)) => MonadBlockChain (AsTrans t m) where
  validateTxSkel = lift . validateTxSkel

deriving via (AsTrans (WriterT w) m) instance (Monoid w, MonadBlockChainBalancing m) => MonadBlockChainBalancing (WriterT w m)

deriving via (AsTrans (WriterT w) m) instance (Monoid w, MonadBlockChainWithoutValidation m) => MonadBlockChainWithoutValidation (WriterT w m)

deriving via (AsTrans (WriterT w) m) instance (Monoid w, MonadBlockChain m) => MonadBlockChain (WriterT w m)

deriving via (AsTrans (ReaderT r) m) instance MonadBlockChainBalancing m => MonadBlockChainBalancing (ReaderT r m)

deriving via (AsTrans (ReaderT r) m) instance MonadBlockChainWithoutValidation m => MonadBlockChainWithoutValidation (ReaderT r m)

deriving via (AsTrans (ReaderT r) m) instance MonadBlockChain m => MonadBlockChain (ReaderT r m)

deriving via (AsTrans (StateT s) m) instance MonadBlockChainBalancing m => MonadBlockChainBalancing (StateT s m)

deriving via (AsTrans (StateT s) m) instance MonadBlockChainWithoutValidation m => MonadBlockChainWithoutValidation (StateT s m)

deriving via (AsTrans (StateT s) m) instance MonadBlockChain m => MonadBlockChain (StateT s m)

-- 'ListT' has no 'MonadTransControl' instance, so the @deriving via ...@
-- machinery is unusable here. However, there is
--
-- > MonadError e m => MonadError e (ListT m)
--
-- so I decided to go with a bit of code duplication to implement the
-- 'MonadBlockChainWithoutValidation' and 'MonadBlockChain' instances for
-- 'ListT', instead of more black magic...

instance MonadBlockChainBalancing m => MonadBlockChainBalancing (ListT m) where
  getParams = lift getParams
  validatorFromHash = lift . validatorFromHash
  utxosAtLedger = lift . utxosAtLedger
  txOutByRefLedger = lift . txOutByRefLedger
  datumFromHash = lift . datumFromHash

instance MonadBlockChainWithoutValidation m => MonadBlockChainWithoutValidation (ListT m) where
  allUtxosLedger = lift allUtxosLedger
  currentSlot = lift currentSlot
  awaitSlot = lift . awaitSlot

instance MonadBlockChain m => MonadBlockChain (ListT m) where
  validateTxSkel = lift . validateTxSkel
