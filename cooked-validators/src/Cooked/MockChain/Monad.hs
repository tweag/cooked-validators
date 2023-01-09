{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.Monad where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Trans.Writer
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints.Type
import Data.Kind
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Ledger as Pl
import qualified Ledger.TimeSlot as Pl
import qualified Ledger.Tx.CardanoAPI as Pl
import qualified Plutus.V2.Ledger.Api as PV2

-- * BlockChain Monad

class (MonadFail m) => MonadTweakChain m where
  -- | Returns a list of all currently known outputs
  allUtxos :: m [(Pl.TxOutRef, PV2.TxOut)]

  -- | Returns the datum with the given hash, or 'Nothing' if there is none
  datumFromHash :: Pl.DatumHash -> m (Maybe Pl.Datum)

  -- | Returns an output given a reference to it
  txOutByRef :: Pl.TxOutRef -> m (Maybe PV2.TxOut)

  -- | Returns the hash of our own public key. When running in the "Plutus.Contract.Contract" monad,
  --  this is a proxy to 'Pl.ownPubKey'; when running in mock mode, the return value can be
  --  controlled with 'signingWith': the head of the non-empty list will be considered as the "ownPubkey".
  ownPaymentPubKeyHash :: m Pl.PubKeyHash

  -- | Returns the current slot number
  currentSlot :: m Pl.Slot

  -- | Returns the current time
  currentTime :: m Pl.POSIXTime

  -- | Either waits until the given slot or returns the current slot.
  --  Note that that it might not wait for anything if the current slot
  --  is larger than the argument.
  awaitSlot :: Pl.Slot -> m Pl.Slot

  -- | Wait until the slot where the given time falls into and return latest time
  -- we know has passed.
  --
  -- Example: if starting time is 0 and slot length is 3s, then `awaitTime 4`
  -- waits until slot 2 and returns the value `POSIXTime 5`.
  awaitTime :: Pl.POSIXTime -> m Pl.POSIXTime

class MonadTweakChain m => MonadBlockChain m where
  -- | Generates and balances a transaction from a skeleton, then attemps to validate such
  --  transaction. A balanced transaction is such that @inputs + mints == outputs + fees@.
  --  To balance a transaction, we need access to the current UTxO state to choose
  --  which inputs to add in case the output-side of the balancing equation is bigger.
  --
  --  The 'TxSkel' receives a 'TxOpts' record with a number of options to customize how validation works.
  validateTxSkel :: TxSkel -> m Pl.CardanoTx

-- | Retrieve the ordered list of outputs of the given "CardanoTx".
--
-- This is useful when writing endpoints and/or traces to fetch utxos of
-- interest right from the start and avoid querying the chain for them
-- afterwards using 'allUtxos' or similar functions.
spOutsFromCardanoTx :: Pl.CardanoTx -> [(Pl.TxOutRef, PV2.TxOut)]
spOutsFromCardanoTx =
  map (\(txOut, txOutRef) -> (txOutRef, txOutV2fromV1 txOut)) . Pl.getCardanoTxOutRefs

txOutV2fromV1 :: Pl.TxOut -> PV2.TxOut
txOutV2fromV1 = Pl.fromCardanoTxOutToPV2TxInfoTxOut . Pl.getTxOut

-- | Return all UTxOs belonging to a particular pubkey, no matter their datum or
-- value.
pkUtxosMaybeDatum :: MonadTweakChain m => Pl.PubKeyHash -> m [(Pl.TxOutRef, PKOutputMaybeDatum)]
pkUtxosMaybeDatum pkh =
  mapMaybe
    (secondMaybe (isPKOutputFrom pkh))
    <$> allUtxos

-- | Return all UTxOs belonging to a particular pubkey that have no datum on
-- them.
pkUtxos :: MonadTweakChain m => Pl.PubKeyHash -> m [(Pl.TxOutRef, PKOutput)]
pkUtxos pkh =
  mapMaybe
    (secondMaybe (isOutputWithoutDatum <=< isPKOutputFrom pkh))
    <$> allUtxos

-- | A little helper for all of the "utxosSuchThat"-like functions. Why is
-- (something more general than) this not in Control.Arrow or somewhere similar?
secondMaybe :: (b -> Maybe c) -> (a, b) -> Maybe (a, c)
secondMaybe f (x, y) = (x,) <$> f y

-- ** Slot and Time Management

-- $slotandtime
-- #slotandtime#
--
-- Slots are integers that monotonically increase and model the passage of time. By looking
-- at the current slot, a validator gets to know that it is being executed within a certain
-- window of wall-clock time. Things can get annoying pretty fast when trying to mock traces
-- and trying to exercise certain branches of certain validators; make sure you also read
-- the docs on 'autoSlotIncrease' to be able to simulate sending transactions in parallel.

waitNSlots :: (MonadBlockChain m) => Integer -> m Pl.Slot
waitNSlots n = do
  when (n < 0) $ fail "waitNSlots: negative argument"
  c <- currentSlot
  awaitSlot $ c + fromIntegral n

waitNMilliSeconds :: (MonadBlockChain m) => Pl.DiffMilliSeconds -> m Pl.POSIXTime
waitNMilliSeconds n = do
  t <- currentTime
  awaitTime $ t + Pl.fromMilliSeconds n

-- -- ** Deriving further 'MonadBlockChain' instances

-- -- | A newtype wrapper to be used with '-XDerivingVia' to derive instances of 'MonadBlockChain'
-- -- for any 'MonadTrans'.
-- --
-- -- For example, to derive 'MonadBlockChain m => MonadBlockChain (ReaderT r m)', you'd write
-- --
-- -- > deriving via (AsTrans (ReaderT r) m) instance MonadBlockChain m => MonadBlockChain (ReaderT r m)
-- --
-- -- and avoid the boilerplate of defining all the methods of the class yourself.
-- newtype AsTrans t (m :: Type -> Type) a = AsTrans {getTrans :: t m a}
--   deriving newtype (Functor, Applicative, Monad, MonadFail, MonadTrans)

-- instance (MonadTrans t, MonadBlockChain m, MonadFail (t m)) => MonadBlockChain (AsTrans t m) where
--   validateTxSkel = lift . validateTxSkel
--   allUtxos = lift allUtxos
--   txOutByRef = lift . txOutByRef
--   datumFromHash = lift . datumFromHash
--   ownPaymentPubKeyHash = lift ownPaymentPubKeyHash
--   currentSlot = lift currentSlot
--   currentTime = lift currentTime
--   awaitSlot = lift . awaitSlot
--   awaitTime = lift . awaitTime

-- -- This might be assigned a more general type,
-- -- but this type is more inference-friendly, and it also seems to communicate the idea better.
-- unliftOn :: (MonadTransControl t, Monad m, Monad (t m)) => (m (StT t a) -> m (StT t a)) -> t m a -> t m a
-- f `unliftOn` act = liftWith (\run -> f (run act)) >>= restoreT . pure

-- instance (MonadTransControl t, MonadMockChain m, MonadFail (t m)) => MonadMockChain (AsTrans t m) where
--   signingWith wallets (AsTrans act) = AsTrans $ signingWith wallets `unliftOn` act
--   askSigners = lift askSigners
--   askParams = lift askParams
--   localParams f (AsTrans act) = AsTrans $ localParams f `unliftOn` act

-- deriving via (AsTrans (WriterT w) m) instance (Monoid w, MonadBlockChain m) => MonadBlockChain (WriterT w m)

-- deriving via (AsTrans (WriterT w) m) instance (Monoid w, MonadMockChain m) => MonadMockChain (WriterT w m)

-- deriving via (AsTrans (ReaderT r) m) instance MonadBlockChain m => MonadBlockChain (ReaderT r m)

-- deriving via (AsTrans (ReaderT r) m) instance MonadMockChain m => MonadMockChain (ReaderT r m)

-- deriving via (AsTrans (StateT s) m) instance MonadBlockChain m => MonadBlockChain (StateT s m)

-- deriving via (AsTrans (StateT s) m) instance MonadMockChain m => MonadMockChain (StateT s m)
