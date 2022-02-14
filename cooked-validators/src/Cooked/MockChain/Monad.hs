{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Control.Monad.Trans.Control
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints
import Data.Kind (Type)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import Data.Void
import qualified Ledger as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Typed.Scripts as Pl (DatumType, TypedValidator, validatorAddress)
import qualified PlutusTx as Pl (FromData)

-- * BlokChain Monad

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
-- test the code by running it in the 'Cooked.MockChain.Monad.Direct.MockChain' monad, provided in this library,
class (MonadFail m) => MonadBlockChain m where
  -- | Generates and balances a transaction from a skeleton, then attemps to validate such
  --  transaction. A balanced transaction is such that @inputs + mints == outputs + fees@.
  --  To balance a transaction, we need access to the current UTxO state to choose
  --  which inputs to add in case the output-side of the balancing equation is bigger.
  --
  --  The 'TxSkel' receives a 'TxOpts' record with a number of options to customize how validation works.
  validateTxSkel :: TxSkel -> m Pl.TxId

  -- | Returns a list of spendable outputs that belong to a given credential and satisfy a given predicate;
  --  Additionally, return the datum present in there. It is important to use @-XTypeApplications@ and
  --  pass a value for type variable @a@ below.
  utxosSuchThat ::
    (Pl.FromData a) =>
    Pl.Credential ->
    (Maybe Pl.StakingCredential -> Maybe a -> Pl.Value -> Bool) ->
    m [(SpendableOut, Maybe a)]

  -- | Search for UTxOs belonging to a given address, which includes a specific staking credential.
  --  To search for addresses irrespective of their staking credential use one of:
  --  'utxosSuchThat', 'pkUtxosSuchThat' or 'scriptUtxosSuchThat'.
  addrUtxosSuchThat ::
    (Pl.FromData a) =>
    Pl.Address ->
    (Maybe a -> Pl.Value -> Bool) ->
    m [(SpendableOut, Maybe a)]
  -- This is here because the Contract monad is inflexible and doesn't allow us to search
  -- for credentials irrespective of their staking credential; so 'utxosSuchThat' crashes in
  -- the contract monad...
  addrUtxosSuchThat (Pl.Address cred stak) predi =
    utxosSuchThat cred (\stak' -> if stak == stak' then predi else \_ _ -> False)

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

  -- | Either waits until the given slot then returns the current slot.
  --  Note that that it might not wait for anything if the current slot
  --  is larger than the argument.
  awaitSlot :: Pl.Slot -> m Pl.Slot

  -- | Wait until the slot where the given time falls into and return latest time
  -- we know has passed.
  --
  -- Example: if starting time is 0 and slot length is 3s, then `awaitTime 4`
  -- waits until slot 2 and returns the value `POSIXTime 5`.
  awaitTime :: Pl.POSIXTime -> m Pl.POSIXTime

-- | Calls 'validateTxSkel' with a skeleton that is set with some specific options.
validateTxConstrOpts :: (MonadBlockChain m) => TxOpts -> [Constraint] -> m Pl.TxId
validateTxConstrOpts opts = validateTxSkel . txSkelOpts opts

-- | Calls 'validateTx' with the default set of options and no label.
validateTxConstr :: (MonadBlockChain m) => [Constraint] -> m Pl.TxId
validateTxConstr = validateTxSkel . txSkel

-- | Calls 'validateTxSkel' with the default set of options but passes an arbitrary showable label to it.
validateTxConstrLbl :: (Show lbl, MonadBlockChain m) => lbl -> [Constraint] -> m Pl.TxId
validateTxConstrLbl lbl = validateTxSkel . txSkelLbl lbl

-- | Returns the output associated with a given reference
outFromOutRef :: (MonadBlockChain m) => Pl.TxOutRef -> m Pl.TxOut
outFromOutRef outref = do
  mo <- txOutByRef outref
  case mo of
    Just o -> return o
    Nothing -> fail ("No output associated with: " ++ show outref)

spendableRef :: (MonadBlockChain m) => Pl.TxOutRef -> m SpendableOut
spendableRef txORef = do
  Just txOut <- txOutByRef txORef
  return (txORef, fromJust (Pl.fromTxOut txOut))

-- ** Selecting UTxOs

-- | Script UTxO's datum, hence, can be selected easily with
--  a simpler variant of 'utxosSuchThat'. It is important to pass a value for type variable @a@
--  with an explicit type application to make sure the conversion to and from 'Pl.Datum' happens correctly.
scriptUtxosSuchThat ::
  (MonadBlockChain m, Pl.FromData (Pl.DatumType tv)) =>
  Pl.TypedValidator tv ->
  (Pl.DatumType tv -> Pl.Value -> Bool) ->
  m [(SpendableOut, Pl.DatumType tv)]
scriptUtxosSuchThat v predicate =
  map (second fromJust)
    <$> addrUtxosSuchThat
      (Pl.validatorAddress v)
      (maybe (const False) predicate)

-- | Public-key UTxO's that satisfy a given predicate. Simpler variant of 'utxosSuchThat';
--
--  If you don't care about staking credentials, refer to the simpler 'pkUtxosSuchThat''
--  or even 'pkUtxosWithVal' if you also don't care for the datums.
pkUtxosSuchThat ::
  (MonadBlockChain m, Pl.FromData a) =>
  Pl.PubKeyHash ->
  (Maybe Pl.StakingCredential -> Maybe a -> Pl.Value -> Bool) ->
  m [(SpendableOut, Maybe a)]
pkUtxosSuchThat pkh = utxosSuchThat (Pl.PubKeyCredential pkh)

-- | Analogous to 'pkUtxosSuchThat', but ignores the staking credential.
pkUtxosSuchThat' ::
  (MonadBlockChain m, Pl.FromData a) =>
  Pl.PubKeyHash ->
  (Maybe a -> Pl.Value -> Bool) ->
  m [(SpendableOut, Maybe a)]
pkUtxosSuchThat' pkh = pkUtxosSuchThat pkh . const

-- | Return all utxos belonging to a pubkey containing a value that satisfies the
--   provided predicate. Ignores staking credentials and datums. Check 'pkUtxosSuchThat'
--   for a more fine grained selection.
pkUtxosWithVal :: (MonadBlockChain m) => Pl.PubKeyHash -> (Pl.Value -> Bool) -> m [SpendableOut]
pkUtxosWithVal pk predi = map fst <$> pkUtxosSuchThat' @_ @Void pk (const predi)

-- | Return all utxos belonging to a pubkey
pkUtxos :: (MonadBlockChain m) => Pl.PubKeyHash -> m [SpendableOut]
pkUtxos = flip pkUtxosWithVal (const True)

-- | Return all utxos belonging to a pubkey, but keep them as 'Pl.TxOut'. This is
--  for internal use.
pkUtxos' :: (MonadBlockChain m) => Pl.PubKeyHash -> m [(Pl.TxOutRef, Pl.TxOut)]
pkUtxos' pkh = map (second go) <$> pkUtxos pkh
  where
    go (Pl.PublicKeyChainIndexTxOut a v) = Pl.TxOut a v Nothing
    go _ = error "pkUtxos must return only Pl.PublicKeyChainIndexTxOut's"

-- ** Slot and Time Management

-- $slotandtime
-- #slotandtime#
--
-- Slots are integers that monotonically increase and model the passage of time. By looking
-- at the current slot, a validator gets to know that it is being executed within a certain
-- window of wall-clock time. Things can get annoying pretty fast when trying to mock traces
-- and trying to exercise certain branches of certain validators, make sure you also see
-- read the docs on 'autoSlotIncrease' to be able to simulate sending transactions in parallel.

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

-- | Runs a given block of computations signing transactions as @w@.
as :: (MonadMockChain m) => m a -> Wallet -> m a
as ma w = signingWith (w NE.:| []) ma

-- | Flipped version of 'as'
signs :: (MonadMockChain m) => Wallet -> m a -> m a
signs = flip as

-- ** Modalities

-- | A modal mock chain is a mock chain that also supports modal modifications of transactions.
-- Hence, modal actions are TxSkel's.
type MonadModalMockChain m = (MonadBlockChain m, MonadMockChain m, MonadModal m, Action m ~ TxSkel)

-- | Monads supporting modifying a certain type of actions with modalities. The 'somewhere'
-- and 'everywhere' functions receive an argument of type @Action m -> Maybe (Action m)@ because
-- we want (a) all branches of @somewhere f tree@ to have a guarantee that they had exactly one action
-- modified by @f@ and (b) we want 'everywhere' to be the dual of 'somewhere', so its type must be the same.
class (Monad m) => MonadModal m where
  type Action m :: Type

  -- | Applies a modification to all possible actions in a tree. If a modification
  -- cannot be applied anywhere, this is the identity: @everywhere (const Nothing) x == x@.
  everywhere :: (Action m -> Maybe (Action m)) -> m a -> m a

  -- | Applies a modification to some transactions in a tree, note that
  -- @somewhere (const Nothing) x == empty@, because 'somewhere' implies
  -- progress, hence if it is not possible to apply the transformation anywhere
  -- in @x@, there would be no progress.
  somewhere :: (Action m -> Maybe (Action m)) -> m a -> m a

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

instance (MonadTransControl t, MonadModal m, Monad (t m), StT t () ~ ()) => MonadModal (AsTrans t m) where
  type Action (AsTrans t m) = Action m
  everywhere f (AsTrans act) = AsTrans $ everywhere f `unliftOn` act
  somewhere f (AsTrans act) = AsTrans $ somewhere f `unliftOn` act

deriving via (AsTrans (ReaderT r) m) instance MonadBlockChain m => MonadBlockChain (ReaderT r m)

deriving via (AsTrans (ReaderT r) m) instance MonadMockChain m => MonadMockChain (ReaderT r m)

deriving via (AsTrans (ReaderT r) m) instance MonadModal m => MonadModal (ReaderT r m)
