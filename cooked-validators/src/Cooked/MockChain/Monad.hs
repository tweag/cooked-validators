{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.Monad where

import Control.Arrow (second)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Trans.Writer
import Cooked.MockChain.Misc
import Cooked.MockChain.UtxoPredicate
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints.Type
import Data.Function (on)
import Data.Kind
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import qualified Ledger as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.TimeSlot as Pl
import qualified Ledger.Typed.Scripts as Pl (DatumType, TypedValidator, validatorAddress)
import Optics.Core
import qualified PlutusTx as Pl (FromData (fromBuiltinData))

-- * BlockChain Monad

-- | Base methods for interacting with a UTxO graph through "Cooked.Tx.Constraints".
--  The operations within 'MonadBlockChain' have an implementation over the 'Plutus.Contract.Contract'
--  monad and can be used to build the first layer of off-chain code that
--  needs to interact with code in the @Contract@ monad.
--
-- It is strongly recomended that you write your code polymorphic for some monad @m@:
--
-- > f :: (MonadBlockChain m) => m ()
--
-- This enables you to choose different interpretations at run time. The two immediately interesting
-- interpretations are running your code within the
-- 'Plutus.Contract.Contract' monad to generate and submit transactions and to
-- test the code by running it in the 'Cooked.MockChain.Monad.Direct.MockChain' monad, provided in this library.
class (MonadFail m) => MonadBlockChain m where
  -- | Generates and balances a transaction from a skeleton, then attemps to validate such
  --  transaction. A balanced transaction is such that @inputs + mints == outputs + fees@.
  --  To balance a transaction, we need access to the current UTxO state to choose
  --  which inputs to add in case the output-side of the balancing equation is bigger.
  --
  --  The 'TxSkel' receives a 'TxOpts' record with a number of options to customize how validation works.
  validateTxSkel :: TxSkel -> m Pl.CardanoTx

  -- | Returns a list of spendable outputs that belong to a given address and satisfy a given predicate;
  --  Additionally, return the datum present in there if it happened to be a script output. It is important
  --  to use @-XTypeApplications@ and pass a value for type variable @a@ below.
  utxosSuchThat ::
    (Pl.FromData a) =>
    Pl.Address ->
    UtxoPredicate a ->
    m [(SpendableOut, Maybe a)]

  utxosSuchThisAndThat ::
    (Pl.FromData a) =>
    (Pl.Address -> Bool) ->
    UtxoPredicate a ->
    m [(SpendableOut, Maybe a)]

  -- | Returns the datum with the given hash, or 'Nothing' if there is none
  datumFromHash :: Pl.DatumHash -> m (Maybe Pl.Datum)

  -- | Returns an output given a reference to it
  txOutByRef :: Pl.TxOutRef -> m (Maybe Pl.TxOut)

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

-- spendableRef :: (MonadBlockChain m) => Pl.TxOutRef -> m SpendableOut
-- spendableRef txORef = do
--   Just txOut <- txOutByRef txORef
--   return (txORef, fromJust (Pl.fromTxOut txOut))

-- | Some values of type "SpendableOut" have no explicit datum in their
-- "ChainIndexTxOut" but the datum hash instead. When used in cooked
-- constraints, this results in a runtime error. This function modifies the
-- value to replace the datum hash by the datum by looking it up in the state
-- of the block chain.
--
-- Outputs obtained from a "CardanoTx" (using "getCardanoTxOutRefs") have datum
-- hashes instead of datums. If you want to use them in spend constraints, you
-- first have to preprocess them with this function.
--
-- As a user though, when writing endpoints and traces, you will prefer to use
-- the "spOutsFromCardanoTx" helper if you need to extract a "SpendableOut"
-- from a "CardanoTx" to use it in a spend constraint.
spOutResolveDatum ::
  MonadBlockChain m =>
  SpendableOut ->
  m SpendableOut
spOutResolveDatum o =
  case o ^? spOutDatumOrHash of
    Just (datumHash, Nothing) ->
      do
        mDatum <- datumFromHash datumHash
        case mDatum of
          Nothing -> fail "datum hash not found in block chain state"
          Just datum -> return $ o & spOutDatumOrHash .~ (datumHash, Just datum)
    _ -> return o

-- | Retrieve the ordered list of "SpendableOutput" corresponding to each
-- output of the given "CardanoTx". These "SpendableOutput" are processed to
-- remove unresolved datum hashes with "spOutResolveDatum" so that they can be
-- used in spending constraints.
--
-- This is useful when writing endpoints and/or traces to fetch utxos of
-- interest right from the start and avoid querying the chain for them
-- afterwards using "utxosSuchThat" functions.
spOutsFromCardanoTx :: MonadBlockChain m => Pl.CardanoTx -> m [SpendableOut]
spOutsFromCardanoTx cardanoTx =
  forM (Pl.getCardanoTxOutRefs cardanoTx) $
    \(txOut, txOutRef) ->
      case cTxOutToCito txOut of
        Just chainIndexTxOut -> spOutResolveDatum (SpendableOut txOutRef chainIndexTxOut)
        Nothing -> fail "could not extract ChainIndexTxOut"

-- | Retrieve a typed datum from a 'SpendableOut'. This function is useful if you
-- know the type of datum you expect on a 'SpendableOut' and you want to extract
-- the datum directly to the correct 'DatumType' for your validator.
--
-- This function has an ambiguous type, so it is probably necessary to
-- type-apply it to the type of your validator.
spOutGetDatum :: (Pl.FromData (Pl.DatumType a), MonadBlockChain m) => SpendableOut -> m (Pl.DatumType a)
spOutGetDatum spOut = do
  mDatum <- datumFromSpOut spOut
  case mDatum of
    Nothing -> fail "there is no datum on this output"
    Just datum -> parseDatum datum
  where
    parseDatum (Pl.Datum builtinData) =
      case Pl.fromBuiltinData builtinData of
        Just d -> return d
        Nothing -> fail "could not parse datum to correct type"

datumFromSpOut :: MonadBlockChain m => SpendableOut -> m (Maybe Pl.Datum)
datumFromSpOut (SpendableOut _ chainIndexTxOut) = datumFromTxOut chainIndexTxOut

datumFromTxOut :: MonadBlockChain m => Pl.ChainIndexTxOut -> m (Maybe Pl.Datum)
datumFromTxOut o =
  case o ^? chainIndexTxOutDatumOrHash of
    Nothing -> return Nothing
    Just (_, Just datum) -> return $ Just datum
    Just (datumHash, Nothing) -> datumFromHash datumHash

-- | Select public-key UTxOs that might contain some datum but no staking address.
-- This is just a simpler variant of 'utxosSuchThat'. If you care about staking credentials
-- you must use 'utxosSuchThat' directly.
pkUtxosSuchThat ::
  forall a m.
  (MonadBlockChain m, Pl.FromData a) =>
  Pl.PubKeyHash ->
  UtxoPredicate a ->
  m [(SpendableOut, Maybe a)]
pkUtxosSuchThat pkh = utxosSuchThat (Pl.Address (Pl.PubKeyCredential pkh) Nothing)

-- | Select public-key UTxOs that do not contain some datum nor staking address.
-- This is just a simpler variant of 'pkUtxosSuchThat'.
pkUtxosSuchThatValue ::
  (MonadBlockChain m) =>
  Pl.PubKeyHash ->
  (Pl.Value -> Bool) ->
  m [SpendableOut]
pkUtxosSuchThatValue pkh predi =
  map fst <$> pkUtxosSuchThat @() pkh (valueSat predi)

-- | Script UTxOs always have a datum, hence, can be selected easily with
--  a simpler variant of 'utxosSuchThat'. It is important to pass a value for type variable @a@
--  with an explicit type application to make sure the conversion to and from 'Pl.Datum' happens correctly.
scriptUtxosSuchThat ::
  (MonadBlockChain m, Pl.FromData (Pl.DatumType tv)) =>
  Pl.TypedValidator tv ->
  -- | Slightly different from 'UtxoPredicate': here we're guaranteed to have a datum present.
  (Pl.DatumType tv -> Pl.Value -> Bool) ->
  m [(SpendableOut, Pl.DatumType tv)]
scriptUtxosSuchThat v predicate =
  map (second fromJust)
    <$> utxosSuchThat
      (Pl.validatorAddress v)
      (maybe (const False) predicate)

-- | Same as above, but ignoring the StakingCredential in the address after filtering.
scriptUtxosSuchThatIgnoringSCred ::
  (MonadBlockChain m, Pl.FromData (Pl.DatumType tv)) =>
  Pl.TypedValidator tv ->
  -- | Slightly different from 'UtxoPredicate': here we're guaranteed to have a datum present.
  (Pl.DatumType tv -> Pl.Value -> Bool) ->
  m [(SpendableOut, Pl.DatumType tv)]
scriptUtxosSuchThatIgnoringSCred v predicate =
  map (second fromJust)
    <$> utxosSuchThisAndThat
      (((==) `on` Pl.addressCredential) (Pl.validatorAddress v))
      (maybe (const False) predicate)

-- | Returns the output associated with a given reference
outFromOutRef :: (MonadBlockChain m) => Pl.TxOutRef -> m Pl.TxOut
outFromOutRef outref = do
  mo <- txOutByRef outref
  case mo of
    Just o -> return o
    Nothing -> fail ("No output associated with: " ++ show outref)

-- | Return all UTxOs belonging to a pubkey
pkUtxos :: (MonadBlockChain m) => Pl.PubKeyHash -> m [SpendableOut]
pkUtxos pkh = pkUtxosSuchThatValue pkh (const True)

-- | Return all UTxOs belonging to a pubkey, but keep them as 'Pl.TxOut'. This is
--  for internal use.
pkUtxos' :: (MonadBlockChain m) => Pl.PubKeyHash -> m [(Pl.TxOutRef, Pl.TxOut)]
pkUtxos' pkh = map (\o -> (o ^. spOutTxOutRef, go $ o ^. spOutChainIndexTxOut)) <$> pkUtxos pkh
  where
    go (Pl.PublicKeyChainIndexTxOut a v datum _) = toPlTxOut a v (datum >>= snd)
    go _ = error "pkUtxos must return only Pl.PublicKeyChainIndexTxOut's"

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

-- ** Monad /Mock/ Chain: multiple identities

-- | The 'Plutus.Contract.Contract' monad has certain design decisions
-- that make it suboptimal for testing. Therefore, we divided the abstract
-- interface into those functions that have an interpretation in
-- 'Plutus.Contract.Contract' and those that do not.
--
-- For example, a function returning a value of type @Contract@ can be thought
-- of as an interaction with a plutus contract from a /single user/ point of view.
-- When testing, however, we might want to interact with a contract from multiple different users.
-- Changing the set of wallets that sign a transaction has no interpretation
-- in 'Plutus.Contract.Contract' and can only be used with the testing monads.
class (MonadBlockChain m) => MonadMockChain m where
  -- | Sets a list of wallets that will sign every transaction emitted
  --  in the respective block
  signingWith :: NE.NonEmpty Wallet -> m a -> m a

  -- | Returns the current set of signing wallets.
  askSigners :: m (NE.NonEmpty Wallet)

  -- | Returns the protocol parameters of the mock chain, which includes
  -- the slot config.
  params :: m Pl.Params

  -- | Modify the parameters according to some function
  localParams :: (Pl.Params -> Pl.Params) -> m a -> m a

-- | Runs a given block of computations signing transactions as @w@.
as :: (MonadMockChain m) => m a -> Wallet -> m a
as ma w = signingWith (w NE.:| []) ma

-- | Flipped version of 'as'
signs :: (MonadMockChain m) => Wallet -> m a -> m a
signs = flip as

-- | Return the 'Pl.SlotConfig' contained within the current 'Pl.Params'
slotConfig :: (MonadMockChain m) => m Pl.SlotConfig
slotConfig = Pl.pSlotConfig <$> params

-- | Set higher limits on transaction size and execution units.
-- This can be used to work around @MaxTxSizeUTxO@ and @ExUnitsTooBigUTxO@ errors.
-- Note that if you need this your Plutus script will probably not validate on Mainnet.
allowBigTransactions :: (MonadMockChain m) => m a -> m a
allowBigTransactions = localParams Pl.increaseTransactionLimits

-- ** Deriving further 'MonadBlockChain' instances

-- | A newtype wrapper to be used with '-XDerivingVia' to derive instances of 'MonadBlockChain'
-- for any 'MonadTrans'.
--
-- For example, to derive 'MonadBlockChain m => MonadBlockChain (ReaderT r m)', you'd write
--
-- > deriving via (AsTrans (ReaderT r) m) instance MonadBlockChain m => MonadBlockChain (ReaderT r m)
--
-- and avoid the boilerplate of defining all the methods of the class yourself.
newtype AsTrans t (m :: Type -> Type) a = AsTrans {getTrans :: t m a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadTrans)

instance (MonadTrans t, MonadBlockChain m, MonadFail (t m)) => MonadBlockChain (AsTrans t m) where
  validateTxSkel = lift . validateTxSkel
  utxosSuchThat addr f = lift $ utxosSuchThat addr f
  utxosSuchThisAndThat addrPred datumPred = lift $ utxosSuchThisAndThat addrPred datumPred
  datumFromHash = lift . datumFromHash
  ownPaymentPubKeyHash = lift ownPaymentPubKeyHash
  txOutByRef = lift . txOutByRef
  currentSlot = lift currentSlot
  currentTime = lift currentTime
  awaitSlot = lift . awaitSlot
  awaitTime = lift . awaitTime

-- This might be assigned a more general type,
-- but this type is more inference-friendly, and it also seems to communicate the idea better.
unliftOn :: (MonadTransControl t, Monad m, Monad (t m)) => (m (StT t a) -> m (StT t a)) -> t m a -> t m a
f `unliftOn` act = liftWith (\run -> f (run act)) >>= restoreT . pure

instance (MonadTransControl t, MonadMockChain m, MonadFail (t m)) => MonadMockChain (AsTrans t m) where
  signingWith wallets (AsTrans act) = AsTrans $ signingWith wallets `unliftOn` act
  askSigners = lift askSigners
  params = lift params
  localParams f (AsTrans act) = AsTrans $ localParams f `unliftOn` act

deriving via (AsTrans (WriterT w) m) instance (Monoid w, MonadBlockChain m) => MonadBlockChain (WriterT w m)

deriving via (AsTrans (WriterT w) m) instance (Monoid w, MonadMockChain m) => MonadMockChain (WriterT w m)

deriving via (AsTrans (ReaderT r) m) instance MonadBlockChain m => MonadBlockChain (ReaderT r m)

deriving via (AsTrans (ReaderT r) m) instance MonadMockChain m => MonadMockChain (ReaderT r m)

deriving via (AsTrans (StateT s) m) instance MonadBlockChain m => MonadBlockChain (StateT s m)

deriving via (AsTrans (StateT s) m) instance MonadMockChain m => MonadMockChain (StateT s m)
