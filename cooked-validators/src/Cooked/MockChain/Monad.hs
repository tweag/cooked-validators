{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.MockChain.Monad where

import Control.Arrow (second)
import Control.Monad.Reader
import Cooked.MockChain.Time
import Cooked.Tx.Constraints
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Void
import qualified Ledger as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Typed.Scripts as Pl (DatumType, TypedValidator, validatorScript)
import qualified PlutusTx as Pl (FromData)
import Test.QuickCheck.GenT

-- * MockChain Monad

-- $mockchainmonad
-- #mockchainanchor#
--
-- The 'MonadMockChain' class provides the basic interface
-- to write traces for validator scripts using the transaction generator from
-- the constraints from "Cooked.Tx.Constraints".
--
-- A /trace/ is a sequence of transactions that bring the system into a given state:
--
-- > tr :: (MonadMockChain m) => m ()
--
-- It is strongly recomended that you write your code polymorphic over the monad @m@,
-- in order to be able to choose different interpretations at run time.
-- The two immediately interesting interpretations can be found at "Cooked.MockChain.Base"
-- and "Cooked.MockChain.Staged". The main difference is that under /Base/ you will find
-- a direct interpretation based on a combination of @StateT UtxoIntex@ and @Except@, whereas
-- in /Staged/ there is a staged interpertation that produces an AST which can later be modified.
-- For example, say you want to look at 'UtxoState's that result from executing variations over @tr@
-- above, where of the transactions is omitted. You can:
--
-- > omitOne :: StagedMockChain a -> [StagedMockChain a]
-- >
-- > allVariations :: [Either MockChainError ((), UtxoState)]
-- > allVariations = map runMockChain (interpret $ omitOne tr)

-- | Base methods for interacting with a UTxO graph through "Cooked.Tx.Constraints".
--  See [here](#mockchainanchor) for more details.
class (MonadFail m) => MonadMockChain m where
  -- | Generates and balances a transaction from a skeleton, then attemps to
  -- validate such transaction.
  --  A balanced transaction is such that @inputs + mints == outputs + fees@.
  --  To balance a transaction, we need
  --  access to the current UTxO state to chose which inputs to add in case
  --  the output-side of the balancing equation is bigger.
  --
  --  If you want to work manually or skip balancing, check the helpers
  -- from 'generateTx' and "Cooked.Tx.Generator".
  validateTxSkel :: TxSkel -> m ()

  -- | Returns a list of spendable outputs that belong to a given address and satisfy a given predicate;
  --  Additionally, return the datum present in there if it happened to be a script output. It is important
  --  to use @-XTypeApplications@ and pass a value for type variable @a@ below.
  utxosSuchThat ::
    (Pl.FromData a) =>
    Pl.Address ->
    (Maybe a -> Pl.Value -> Bool) ->
    m [(SpendableOut, Maybe a)]

  -- | Returns an output given a reference to it
  txOutByRef :: Pl.TxOutRef -> m (Maybe Pl.TxOut)

  -- | Returns the current slot counter, which is responsible for mocking the passage of time
  slotCounter :: m SlotCounter

  -- | Modifies the slot counter
  modifySlotCounter :: (SlotCounter -> SlotCounter) -> m ()

-- | Monads supporting modifying transaction skeletons with modalities.
class (Monad m) => MonadModal m where
  -- | Applies a modification to all transactions in a tree
  everywhere :: (TxSkel -> TxSkel) -> m () -> m ()

  -- | Applies a modification to some transactions in a tree, note that
  -- @somewhere (const Nothing) x == empty@, because 'somewhere' implies
  -- progress, hence if it is not possible to apply the transformation anywhere
  -- in @x@, there would be no progress.
  somewhere :: (TxSkel -> Maybe TxSkel) -> m () -> m ()

-- | A modal mock chain is a mock chain that also supports modal modifications of transactions.
type MonadModalMockChain m = (MonadMockChain m, MonadModal m)

-- | Generates, balances and validates a transaction from a 'TxSkel'
validateTxFromSkeleton :: (MonadMockChain m) => TxSkel -> m ()
validateTxFromSkeleton = validateTxSkel

spendableRef :: (MonadMockChain m) => Pl.TxOutRef -> m SpendableOut
spendableRef txORef = do
  Just txOut <- txOutByRef txORef
  return (txORef, fromJust (Pl.fromTxOut txOut))

-- | Public-key UTxO's have no datum, hence, can be selected easily with
--  a simpler variant of 'utxosSuchThat'
pkUtxosSuchThat :: (MonadMockChain m) => Pl.PubKeyHash -> (Pl.Value -> Bool) -> m [SpendableOut]
pkUtxosSuchThat pkh predicate =
  map fst
    <$> utxosSuchThat @_ @Void
      (Pl.Address (Pl.PubKeyCredential pkh) Nothing)
      (maybe predicate absurd)

-- | Script UTxO's always have a datum, hence, can be selected easily with
--  a simpler variant of 'utxosSuchThat'. It is important to pass a value for type variable @a@
--  with an explicit type application to make sure the conversion to and from 'Pl.Datum' happens correctly.
scriptUtxosSuchThat ::
  (MonadMockChain m, Pl.FromData (Pl.DatumType tv)) =>
  Pl.TypedValidator tv ->
  (Pl.DatumType tv -> Pl.Value -> Bool) ->
  m [(SpendableOut, Pl.DatumType tv)]
scriptUtxosSuchThat v predicate =
  map (second fromJust)
    <$> utxosSuchThat
      (Pl.Address (Pl.ScriptCredential $ Pl.validatorHash $ Pl.validatorScript v) Nothing)
      (maybe (const False) predicate)

-- | Returns the output associated with a given reference
outFromOutRef :: (MonadMockChain m) => Pl.TxOutRef -> m Pl.TxOut
outFromOutRef outref = do
  mo <- txOutByRef outref
  case mo of
    Just o -> return o
    Nothing -> fail ("No output associated with: " ++ show outref)

-- | Return all utxos belonging to a pubkey
pkUtxos :: (MonadMockChain m) => Pl.PubKeyHash -> m [SpendableOut]
pkUtxos = flip pkUtxosSuchThat (const True)

-- | Return all utxos belonging to a pubkey, but keep them as 'Pl.TxOut'. This is
--  for internal use.
pkUtxos' :: (MonadMockChain m) => Pl.PubKeyHash -> m [(Pl.TxOutRef, Pl.TxOut)]
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
-- and trying to exercise certain branches of certain validators. The functions below attempt
-- to make that a little easier.

-- | Stops increasing time altogether.
freezeTime :: (MonadMockChain m) => m ()
freezeTime = modifySlotCounter scFreezeTime

-- TODO: Finish documentation when issue #34 is solved

waitSlots :: (MonadMockChain m) => Integer -> m ()
waitSlots n = modifySlotCounter (scWaitSlots n)

waitTime :: (MonadMockChain m) => Pl.POSIXTime -> m ()
waitTime mSec = modifySlotCounter (scWait mSec)

slotIs :: (MonadMockChain m) => Integer -> m ()
slotIs n = modifySlotCounter (scSlotIs n)

timeIs :: (MonadMockChain m) => Pl.POSIXTime -> m ()
timeIs t = modifySlotCounter (scTimeIs t)

-- ** Deriving further 'MonadMockChain' instances

-- | A newtype wrapper to be used with '-XDerivingVia' to derive instances of 'MonadMockChain'
-- for any 'MonadTrans'.
--
-- For example, to derive 'MonadMockChain m => MonadMockChain (ReaderT r m)', you'd write
--
-- > deriving via (AsTrans (ReaderT r) m) instance MonadMockChain m => MonadMockChain (ReaderT r m)
--
-- and avoid the boilerplate of defining all the methods of the class yourself.
newtype AsTrans t (m :: * -> *) a = AsTrans {getTrans :: t m a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadTrans)

instance (MonadTrans t, MonadMockChain m, MonadFail (t m)) => MonadMockChain (AsTrans t m) where
  validateTxSkel = lift . validateTxSkel
  utxosSuchThat addr f = lift $ utxosSuchThat addr f
  txOutByRef = lift . txOutByRef
  slotCounter = lift slotCounter
  modifySlotCounter = lift . modifySlotCounter

deriving via (AsTrans (ReaderT r) m) instance MonadMockChain m => MonadMockChain (ReaderT r m)

deriving via (AsTrans GenT m) instance MonadMockChain m => MonadMockChain (GenT m)

instance MonadModal m => MonadModal (ReaderT r m) where
  everywhere f m = ReaderT (everywhere f . runReaderT m)
  somewhere f m = ReaderT (somewhere f . runReaderT m)

instance MonadModal m => MonadModal (GenT m) where
  everywhere f m = GenT (\r i -> everywhere f (unGenT m r i))
  somewhere f m = GenT (\r i -> somewhere f (unGenT m r i))
