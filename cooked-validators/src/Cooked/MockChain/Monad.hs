{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.MockChain.Monad where

import Control.Arrow (second)
import Control.Monad
import Cooked.MockChain.Time
import Cooked.Tx.Constraints
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Void
import qualified Ledger as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Typed.Scripts as Pl (DatumType, TypedValidator, validatorScript)
import qualified PlutusTx as Pl (FromData)

-- * MockChain Monad

-- $mockchainmonad
-- #mockchainanchor#
--
-- The 'MonadMockChain' class provides the basic interface that we need in order
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
  -- | Generates a balanced transaction, that is, a transaction where
  --  @inputs + mints == outputs + fees@. To balance a transaction, we need
  --  access to the current UTxO state to chose which inputs to add in case
  --  the output-side of the balancing equation is bigger.
  generateTx :: TxSkel -> m Pl.Tx

  -- | Validates a transaction and, upon success, updates the utxo map; You can generate
  --  transactions with the helpers from 'generateTx' and "Cooked.Tx.Generator".
  --  Most of the times you will want to use 'validateTxFromSkeleton'.
  validateTx :: Pl.Tx -> m ()

  -- | Returns a list of spendable outputs that belong to a given address and satisfy a given predicate;
  --  Additionally, return the datum present in there if it happened to be a script output. It is important
  --  to use @-XTypeApplications@ and pass a value for type variable @a@ below.
  utxosSuchThat ::
    (Pl.FromData a) =>
    Pl.Address ->
    (Maybe a -> Pl.Value -> Bool) ->
    m [(SpendableOut, Maybe a)]

  -- | Returns the current map of unspent outputs, called the 'Pl.UtxoIndex' in Plutus.
  --  By default, the slots are increased in a constant rate. If you want more control over
  --  that please check
  index :: m (M.Map Pl.TxOutRef Pl.TxOut)

  -- | Returns the current slot counter, which is responsible for mocking the passage of time
  slotCounter :: m SlotCounter

  -- | Modifies the slot counter
  modifySlotCounter :: (SlotCounter -> SlotCounter) -> m ()

-- | Generates, balances and validates a transaction from a 'TxSkel'
validateTxFromSkeleton :: (MonadMockChain m) => TxSkel -> m ()
validateTxFromSkeleton = generateTx >=> validateTx

spendableRef :: (MonadMockChain m) => Pl.TxOutRef -> m SpendableOut
spendableRef txORef = do
  Just txOut <- M.lookup txORef <$> index
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
  mo <- M.lookup outref <$> index
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
