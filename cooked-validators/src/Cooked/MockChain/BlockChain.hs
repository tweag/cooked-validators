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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.BlockChain where

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
import Data.Kind
import Data.Maybe
import qualified Ledger.Index as Ledger
import qualified Ledger.Slot as Ledger
import qualified Ledger.Tx as Ledger
import qualified Ledger.Tx.CardanoAPI as Ledger
import ListT
import Optics.Core
import qualified Plutus.Script.Utils.Scripts as Pl
import qualified Plutus.V2.Ledger.Api as PV2
import qualified Plutus.V2.Ledger.Api as Pl
import Prettyprinter (Doc)

-- * BlockChain Monad

-- | The errors that can be produced by the 'MockChainT' monad
data MockChainError where
  MCEValidationError :: Ledger.ValidationErrorInPhase -> MockChainError
  MCEUnbalanceable :: String -> BalanceStage -> TxSkel -> MockChainError
  MCENoSuitableCollateral :: MockChainError
  MCEGenerationError :: GenerateTxError -> MockChainError
  MCECalcFee :: MockChainError -> MockChainError
  MCEUnknownOutRefError :: String -> PV2.TxOutRef -> MockChainError
  MCEUnknownValidator :: String -> PV2.ValidatorHash -> MockChainError
  MCEUnknownDatum :: String -> PV2.DatumHash -> MockChainError
  FailWith :: String -> MockChainError
  OtherMockChainError :: (Show err, Eq err) => err -> MockChainError

deriving instance Show MockChainError

instance Eq MockChainError where
  (==) = undefined

-- | Describes us which stage of the balancing process are we at. This is needed
--  to distinguish the successive calls to balancing while computing fees from
--  the final call to balancing
data BalanceStage
  = BalCalcFee
  | BalFinalizing
  deriving (Show, Eq)

class (MonadFail m, MonadError MockChainError m) => MonadBlockChainBalancing m where
  -- | Returns the paramters of the chain.
  getParams :: m Emulator.Params

  -- | Return a list of all UTxOs at a certain address.
  utxosAtLedger :: PV2.Address -> m [(PV2.TxOutRef, Ledger.TxOut)]

  -- | Returns the datum with the given hash, along with its pretty-printed
  -- representation, or 'Nothing' if there is none
  datumFromHash :: PV2.DatumHash -> m (Maybe (PV2.Datum, Doc ()))

  -- | Returns the full validator corresponding to hash, if that validator is
  -- currently the owner of some UTxO. (If it is not, there's no guarantee that
  -- this will return @Just@.)
  validatorFromHash :: PV2.ValidatorHash -> m (Maybe (Pl.Versioned PV2.Validator))

  -- | Returns an output given a reference to it
  txOutByRefLedger :: PV2.TxOutRef -> m (Maybe Ledger.TxOut)

class MonadBlockChainBalancing m => MonadBlockChainWithoutValidation m where
  -- | Returns a list of all currently known outputs.
  allUtxosLedger :: m [(PV2.TxOutRef, Ledger.TxOut)]

  -- | Returns the hash of our own public key. When running in the "Plutus.Contract.Contract" monad,
  --  this is a proxy to 'PV2.ownPubKey'; when running in mock mode, the return value can be
  --  controlled with 'signingWith': the head of the non-empty list will be considered as the "ownPubkey".
  ownPaymentPubKeyHash :: m PV2.PubKeyHash

  -- | Returns the current slot number
  currentSlot :: m Ledger.Slot

  -- | Returns the current time
  currentTime :: m Pl.POSIXTime

  -- | Either waits until the given slot or returns the current slot.
  --  Note that that it might not wait for anything if the current slot
  --  is larger than the argument.
  awaitSlot :: Ledger.Slot -> m Ledger.Slot

  -- | Wait until the slot where the given time falls into and return latest time
  -- we know has passed.
  --
  -- Example: if starting time is 0 and slot length is 3s, then `awaitTime 4`
  -- waits until slot 2 and returns the value `POSIXTime 5`.
  awaitTime :: Pl.POSIXTime -> m Pl.POSIXTime

class MonadBlockChainWithoutValidation m => MonadBlockChain m where
  -- | Generates and balances a transaction from a skeleton, then attemps to validate such
  --  transaction. A balanced transaction is such that @inputs + mints == outputs + fees@.
  --  To balance a transaction, we need access to the current UTxO state to choose
  --  which inputs to add in case the output-side of the balancing equation is bigger.
  --
  --  The 'TxSkel' receives a 'TxOpts' record with a number of options to customize how validation works.
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

-- | Helper function to filter the output of 'allUtxos' and 'utxosFromCardanoTx'
filterUtxos :: (o1 -> Maybe o2) -> [(PV2.TxOutRef, o1)] -> [(PV2.TxOutRef, o2)]
filterUtxos predicate = mapMaybe (\(oref, out) -> (oref,) <$> predicate out)

-- | Return all UTxOs belonging to a particular pubkey that have no datum on
-- them.
pkUtxos :: MonadBlockChainWithoutValidation m => PV2.PubKeyHash -> m [(PV2.TxOutRef, PKOutput)]
pkUtxos pkh = filterUtxos (isOutputWithoutDatum <=< isPKOutputFrom pkh) <$> allUtxos

-- | Like 'allUtxos', but on every 'OutputDatumHash', try to resolve the
-- complete datum from the state
allUtxosWithDatums :: MonadBlockChainWithoutValidation m => m [(PV2.TxOutRef, PV2.TxOut)]
allUtxosWithDatums =
  allUtxos
    >>= mapM
      ( \(oref, out) -> case outputOutputDatum out of
          PV2.OutputDatumHash datumHash -> do
            mDatum <- datumFromHash datumHash
            case mDatum of
              Nothing -> return (oref, out)
              Just (datum, _) -> return (oref, out & outputDatumL .~ PV2.OutputDatum datum)
          _ -> return (oref, out)
      )

filteredUtxos :: MonadBlockChainWithoutValidation m => (PV2.TxOut -> Maybe output) -> m [(PV2.TxOutRef, output)]
filteredUtxos predicate = filterUtxos predicate <$> allUtxos

-- | Like 'filteredUtxos', but will all resolvable datum hashes resolved. This
-- means that the outputs are presented differently from how a script would see
-- them; the information on whether there are inline datums or datum hashes is
-- lost.
filteredUtxosWithDatums ::
  MonadBlockChainWithoutValidation m =>
  (PV2.TxOut -> Maybe output) ->
  m [(PV2.TxOutRef, output)]
filteredUtxosWithDatums predicate = filterUtxos predicate <$> allUtxosWithDatums

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
        Just (datum, _) -> return $ Just datum
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
  ownPaymentPubKeyHash = lift ownPaymentPubKeyHash
  currentSlot = lift currentSlot
  currentTime = lift currentTime
  awaitSlot = lift . awaitSlot
  awaitTime = lift . awaitTime

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
  ownPaymentPubKeyHash = lift ownPaymentPubKeyHash
  currentSlot = lift currentSlot
  currentTime = lift currentTime
  awaitSlot = lift . awaitSlot
  awaitTime = lift . awaitTime

instance MonadBlockChain m => MonadBlockChain (ListT m) where
  validateTxSkel = lift . validateTxSkel
