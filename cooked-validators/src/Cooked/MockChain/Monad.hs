{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cooked.MockChain.Monad where

import Control.Arrow (second)
import Control.Monad.Reader
import Cooked.Tx.Constraints
import Data.Default
import Data.Maybe (fromJust)
import Data.Void
import qualified Data.List.NonEmpty as NE
import qualified Ledger as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Typed.Scripts as Pl (DatumType, TypedValidator, validatorScript)
import qualified PlutusTx as Pl (FromData)
import Test.QuickCheck.GenT
import Cooked.MockChain.Wallet

-- * MockChain Monad

-- $mockchainmonad
-- #mockchainanchor#
--
-- The 'MonadBlockChain' class provides the basic interface
-- to write traces for validator scripts using the transaction generator from
-- the constraints from "Cooked.Tx.Constraints".
--
-- A /trace/ is a sequence of transactions that bring the system into a given state:
--
-- > tr :: (MonadBlockChain m) => m ()
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
class (MonadFail m) => MonadBlockChain m where
  -- | Generates and balances a transaction from a skeleton, then attemps to validate such
  --  transaction. A balanced transaction is such that @inputs + mints == outputs + fees@.
  --  To balance a transaction, we need access to the current UTxO state to choose
  --  which inputs to add in case the output-side of the balancing equation is bigger.
  --
  --  Most of the times, you will want to use 'validateTxSkel', which passes the
  -- default set of options around.
  validateTxSkelOpts :: ValidateTxOpts -> TxSkel -> m Pl.TxId

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

-- | Monads supporting modifying transaction skeletons with modalities.
class (Monad m) => MonadModal m where
  -- | Applies a modification to all transactions in a tree
  everywhere :: (TxSkel -> TxSkel) -> m () -> m ()

  -- | Applies a modification to some transactions in a tree, note that
  -- @somewhere (const Nothing) x == empty@, because 'somewhere' implies
  -- progress, hence if it is not possible to apply the transformation anywhere
  -- in @x@, there would be no progress.
  somewhere :: (TxSkel -> Maybe TxSkel) -> m () -> m ()

-- |Monad supporting mock operations such as changing the signing wallets and
-- retreiving 'SlotConfig'
class (MonadBlockChain m) => MonadMockChain m where
  -- |Sets a list of wallets that will sign every transaction emitted
  -- in the respective block
  signingWith :: NE.NonEmpty Wallet -> m a -> m a

  -- |Returns the current set of signing wallets.
  askSigners :: m (NE.NonEmpty Wallet)

-- |Runs a given block of computations signing transactions as @w@.
as :: (MonadMockChain m) => m a -> Wallet -> m a
as ma w = signingWith (w NE.:| []) ma

-- |Flipped version of 'as'
signs :: (MonadMockChain m) => Wallet -> m a -> m a
signs = flip as

data ValidateTxOpts = ValidateTxOpts
  { -- | Performs an adjustment to unbalanced txs, making sure every UTxO that is produced
    --  has the necessary minimum amount of Ada. Check https://github.com/tweag/audit-plutus-libs/issues/37
    --  for further discussion on this.
    --
    -- By default, this is set to @True@.
    adjustUnbalTx :: Bool,
    -- | When submitting a transaction for real (i.e., running in the 'Plutus.Contract.Contract' monad),
    --  repeatedely calls 'Plutus.Contract.Request.awaitTxConfirmed'.
    --
    --  /This has NO effect when running outside of the @Contract@ monad/.
    --  By default, this is set to @True@.
    awaitTxConfirmed :: Bool,
    -- | Whether to increase the slot counter automatically on this submission.
    -- This is useful for modelling transactions that could be submitted in parallel in reality, so there
    -- should be no explicit ordering of what comes first. One good example is in the Crowdfunding use case contract.
    -- This has no effect when running in 'Plutus.Contract.Contract'.
    autoSlotIncrease :: Bool
  }
  deriving (Eq, Show)

instance Default ValidateTxOpts where
  def =
    ValidateTxOpts
      { adjustUnbalTx = True,
        awaitTxConfirmed = True,
        autoSlotIncrease = True
      }

-- | Calls 'validateTxSkelOpts' with the default set of options
validateTxSkel :: (MonadBlockChain m) => TxSkel -> m Pl.TxId
validateTxSkel = validateTxSkelOpts def

validateTxConstr :: (MonadBlockChain m) => [Constraint] -> m Pl.TxId
validateTxConstr = validateTxSkel . txSkel

validateTxConstr' :: (Show lbl , MonadBlockChain m) => lbl -> [Constraint] -> m Pl.TxId
validateTxConstr' lbl = validateTxSkel . txSkelLbl lbl

-- | A modal mock chain is a mock chain that also supports modal modifications of transactions.
type MonadModalMockChain m = (MonadBlockChain m, MonadMockChain m, MonadModal m)

spendableRef :: (MonadBlockChain m) => Pl.TxOutRef -> m SpendableOut
spendableRef txORef = do
  Just txOut <- txOutByRef txORef
  return (txORef, fromJust (Pl.fromTxOut txOut))

-- | Public-key UTxO's have no datum, hence, can be selected easily with
--  a simpler variant of 'utxosSuchThat'
pkUtxosSuchThat :: (MonadBlockChain m) => Pl.PubKeyHash -> (Pl.Value -> Bool) -> m [SpendableOut]
pkUtxosSuchThat pkh predicate =
  map fst
    <$> utxosSuchThat @_ @Void
      (Pl.Address (Pl.PubKeyCredential pkh) Nothing)
      (maybe predicate absurd)

-- | Script UTxO's always have a datum, hence, can be selected easily with
--  a simpler variant of 'utxosSuchThat'. It is important to pass a value for type variable @a@
--  with an explicit type application to make sure the conversion to and from 'Pl.Datum' happens correctly.
scriptUtxosSuchThat ::
  (MonadBlockChain m, Pl.FromData (Pl.DatumType tv)) =>
  Pl.TypedValidator tv ->
  (Pl.DatumType tv -> Pl.Value -> Bool) ->
  m [(SpendableOut, Pl.DatumType tv)]
scriptUtxosSuchThat v predicate =
  map (second fromJust)
    <$> utxosSuchThat
      (Pl.Address (Pl.ScriptCredential $ Pl.validatorHash $ Pl.validatorScript v) Nothing)
      (maybe (const False) predicate)

-- | Returns the output associated with a given reference
outFromOutRef :: (MonadBlockChain m) => Pl.TxOutRef -> m Pl.TxOut
outFromOutRef outref = do
  mo <- txOutByRef outref
  case mo of
    Just o -> return o
    Nothing -> fail ("No output associated with: " ++ show outref)

-- | Return all utxos belonging to a pubkey
pkUtxos :: (MonadBlockChain m) => Pl.PubKeyHash -> m [SpendableOut]
pkUtxos = flip pkUtxosSuchThat (const True)

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
-- and trying to exercise certain branches of certain validators. The functions below attempt
-- to make that a little easier.

-- TODO: Finish documentation when issue #34 is solved

waitNSlots :: (MonadBlockChain m) => Integer -> m Pl.Slot
waitNSlots n = do
  when (n < 0) $ fail "waitNSlots: negative argument"
  c <- currentSlot
  awaitSlot $ c + fromIntegral n

waitNMilliSeconds :: (MonadBlockChain m) => Pl.DiffMilliSeconds -> m Pl.POSIXTime
waitNMilliSeconds n = do
  t <- currentTime
  awaitTime $ t + Pl.fromMilliSeconds n

-- ** Deriving further 'MonadBlockChain' instances

-- | A newtype wrapper to be used with '-XDerivingVia' to derive instances of 'MonadBlockChain'
-- for any 'MonadTrans'.
--
-- For example, to derive 'MonadBlockChain m => MonadBlockChain (ReaderT r m)', you'd write
--
-- > deriving via (AsTrans (ReaderT r) m) instance MonadBlockChain m => MonadBlockChain (ReaderT r m)
--
-- and avoid the boilerplate of defining all the methods of the class yourself.
newtype AsTrans t (m :: * -> *) a = AsTrans {getTrans :: t m a}
  deriving newtype (Functor, Applicative, Monad, MonadFail, MonadTrans)

instance (MonadTrans t, MonadBlockChain m, MonadFail (t m)) => MonadBlockChain (AsTrans t m) where
  validateTxSkelOpts opts = lift . validateTxSkelOpts opts
  utxosSuchThat addr f = lift $ utxosSuchThat addr f
  ownPaymentPubKeyHash = lift ownPaymentPubKeyHash
  txOutByRef = lift . txOutByRef
  currentSlot = lift currentSlot
  currentTime = lift currentTime
  awaitSlot = lift . awaitSlot
  awaitTime = lift . awaitTime

{- TODO would be great to have this and MonadModal too
instance (MonadTrans t, MonadHasWallets m, Monad (t m)) => MonadHasWallets (AsTrans t m) where
  type MWallet (AsTrans t m) = MWallet m
  findWalletByPKH = lift . findWalletByPKH
  withWallets ws act = lift $ withWallets _ act
-}

deriving via (AsTrans (ReaderT r) m) instance MonadBlockChain m => MonadBlockChain (ReaderT r m)

--deriving via (AsTrans (ReaderT r) m) instance MonadHasWallets m => MonadHasWallets (ReaderT r m)

deriving via (AsTrans GenT m) instance MonadBlockChain m => MonadBlockChain (GenT m)

-- deriving via (AsTrans GenT m) instance MonadMockChain m => MonadMockChain (GenT m)

instance MonadModal m => MonadModal (ReaderT r m) where
  everywhere f m = ReaderT (everywhere f . runReaderT m)
  somewhere f m = ReaderT (somewhere f . runReaderT m)

instance MonadModal m => MonadModal (GenT m) where
  everywhere f m = GenT (\r i -> everywhere f (unGenT m r i))
  somewhere f m = GenT (\r i -> somewhere f (unGenT m r i))

instance MonadMockChain m => MonadMockChain (GenT m) where
  signingWith w ma = GenT (\r i -> signingWith w (unGenT ma r i))
  askSigners = GenT (\_ _ -> askSigners)
