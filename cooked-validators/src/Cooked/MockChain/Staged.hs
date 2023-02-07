{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.Staged where

import qualified Cardano.Node.Emulator as Emulator
import Control.Applicative
import Control.Arrow hiding ((<+>))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer.Strict hiding (Alt)
import Cooked.Ltl
import Cooked.MockChain.BlockChain
import Cooked.MockChain.Direct
import Cooked.MockChain.UtxoState
import Cooked.Pretty.Class
import Cooked.Skeleton
import Cooked.Tweak.Common
import Data.Default
import qualified Ledger.Slot as Ledger
import qualified Ledger.Tx as Ledger
import qualified Ledger.Typed.Scripts as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import qualified Plutus.V2.Ledger.Tx as Pl

-- * Interpreting and running 'StagedMockChain'

-- | Interprets the staged mockchain then runs the resulting computation
-- with a custom function. This can be used, for example, to supply
-- a custom 'InitialDistribution' by providing 'runMockChainTFrom'.
interpretAndRunWith ::
  (forall m. Monad m => MockChainT m a -> m res) ->
  StagedMockChain a ->
  [(res, MockChainLog)]
interpretAndRunWith f smc = runWriterT $ f $ interpret smc

interpretAndRun ::
  StagedMockChain a ->
  [(Either MockChainError (a, UtxoState), MockChainLog)]
interpretAndRun = interpretAndRunWith runMockChainT

data MockChainLogEntry
  = MCLogSubmittedTxSkel SkelContext TxSkel
  | MCLogNewTx Pl.TxId
  | MCLogFail String

type MockChainLog = [MockChainLogEntry]

-- | The semantic domain in which 'StagedMockChain' gets interpreted; see
--  the 'interpret' function for more.
type InterpMockChain = MockChainT (WriterT MockChainLog [])

-- | The 'interpret' function gives semantics to our traces. One
--  'StagedMockChain' computation yields a potential list of 'MockChainT'
--  computations, which emit a description of their operation. Recall a
--  'MockChainT' is a state and except monad composed:
--
--  >     MockChainT (WriterT TraceDescr []) a
--  > =~= st -> (WriterT TraceDescr []) (Either err (a, st))
--  > =~= st -> [(Either err (a, st) , TraceDescr)]
interpret :: StagedMockChain a -> InterpMockChain a
interpret = flip evalStateT [] . interpLtlAndPruneUnfinished

-- * 'StagedMockChain': An AST for 'MonadMockChain' computations

data MockChainBuiltin a where
  -- methods of 'MonadBlockChain'

  GetParams :: MockChainBuiltin Emulator.Params
  ValidateTxSkel :: TxSkel -> MockChainBuiltin Ledger.CardanoTx
  TxOutByRefLedger :: Pl.TxOutRef -> MockChainBuiltin (Maybe Ledger.TxOut)
  GetCurrentSlot :: MockChainBuiltin Ledger.Slot
  AwaitSlot :: Ledger.Slot -> MockChainBuiltin Ledger.Slot
  GetCurrentTime :: MockChainBuiltin Pl.POSIXTime
  AwaitTime :: Pl.POSIXTime -> MockChainBuiltin Pl.POSIXTime
  DatumFromHash :: Pl.DatumHash -> MockChainBuiltin (Maybe (Pl.Datum, DocCooked))
  OwnPubKey :: MockChainBuiltin Pl.PubKeyHash
  AllUtxosLedger :: MockChainBuiltin [(Pl.TxOutRef, Ledger.TxOut)]
  UtxosAtLedger :: Pl.Address -> MockChainBuiltin [(Pl.TxOutRef, Ledger.TxOut)]
  ValidatorFromHash :: Pl.ValidatorHash -> MockChainBuiltin (Maybe (Pl.Versioned Pl.Validator))
  -- | The empty set of traces
  Empty :: MockChainBuiltin a
  -- | The union of two sets of traces
  Alt ::
    StagedMockChain a ->
    StagedMockChain a ->
    MockChainBuiltin a
  -- for the 'MonadFail' instance
  Fail :: String -> MockChainBuiltin a
  -- for the 'MonadError MockChainError' instance
  ThrowError :: MockChainError -> MockChainBuiltin a
  CatchError :: StagedMockChain a -> (MockChainError -> StagedMockChain a) -> MockChainBuiltin a

type MockChainOp = LtlOp (UntypedTweak InterpMockChain) MockChainBuiltin

type StagedMockChain = Staged MockChainOp

instance Alternative StagedMockChain where
  empty = Instr (Builtin Empty) Return
  a <|> b = Instr (Builtin (Alt a b)) Return

instance MonadFail StagedMockChain where
  fail msg = Instr (Builtin (Fail msg)) Return

-- * 'InterpLtl' instance

instance MonadPlus m => MonadPlus (MockChainT m) where
  mzero = lift mzero
  mplus = combineMockChainT mplus

instance InterpLtl (UntypedTweak InterpMockChain) MockChainBuiltin InterpMockChain where
  interpBuiltin GetParams = lift $ asks mceParams
  interpBuiltin (ValidateTxSkel skel) =
    get
      >>= msum
        . map (uncurry interpretAndTell)
        . nowLaterList
    where
      interpretAndTell ::
        UntypedTweak InterpMockChain ->
        [Ltl (UntypedTweak InterpMockChain)] ->
        StateT [Ltl (UntypedTweak InterpMockChain)] InterpMockChain Ledger.CardanoTx
      interpretAndTell (UntypedTweak now) later = do
        mcst <- lift get
        let managedTxOuts = utxoIndexToTxOutMap . mcstIndex $ mcst
            managedDatums = mcstDatums mcst
        (_, skel') <- lift $ runTweakInChain now skel
        lift $
          lift $
            tell
              [ MCLogSubmittedTxSkel
                  (SkelContext managedTxOuts managedDatums)
                  skel'
              ]
        tx <- validateTxSkel skel'
        lift $
          lift $
            tell
              [MCLogNewTx (Ledger.getCardanoTxId tx)]
        put later
        return tx
  interpBuiltin (TxOutByRefLedger o) = txOutByRefLedger o
  interpBuiltin GetCurrentSlot = currentSlot
  interpBuiltin (AwaitSlot s) = awaitSlot s
  interpBuiltin GetCurrentTime = currentTime
  interpBuiltin (AwaitTime t) = awaitTime t
  interpBuiltin (DatumFromHash h) = datumFromHash h
  interpBuiltin (ValidatorFromHash h) = validatorFromHash h
  interpBuiltin OwnPubKey = ownPaymentPubKeyHash
  interpBuiltin AllUtxosLedger = allUtxosLedger
  interpBuiltin (UtxosAtLedger address) = utxosAtLedger address
  interpBuiltin Empty = mzero
  interpBuiltin (Alt l r) = interpLtl l `mplus` interpLtl r
  interpBuiltin (Fail msg) = do
    lift $ lift $ tell [MCLogFail msg]
    fail msg
  interpBuiltin (ThrowError err) = throwError err
  interpBuiltin (CatchError act handler) = catchError (interpLtl act) (interpLtl . handler)

-- ** Helpers to run tweaks for use in tests for tweaks

runTweak :: Tweak InterpMockChain a -> TxSkel -> [Either MockChainError (a, TxSkel)]
runTweak = runTweakFrom def def

runTweakFrom :: MockChainEnv -> MockChainSt -> Tweak InterpMockChain a -> TxSkel -> [Either MockChainError (a, TxSkel)]
runTweakFrom mcenv mcst tweak skel =
  map (right fst . fst)
    . runWriterT
    . runMockChainTRaw mcenv mcst
    $ runTweakInChain tweak skel

-- ** Modalities

-- | A modal mock chain is a mock chain that allows us to use LTL modifications with 'Tweak's
type MonadModalBlockChain m = (MonadBlockChain m, MonadModal m, Modification m ~ UntypedTweak InterpMockChain)

-- | Apply a 'Tweak' to some transaction in the given Trace. The tweak must
-- apply at least once.
somewhere :: MonadModalBlockChain m => Tweak InterpMockChain b -> m a -> m a
somewhere x = modifyLtl (LtlTruth `LtlUntil` LtlAtom (UntypedTweak x))

-- | Apply a 'Tweak' to every transaction in a given trace. This is also
-- successful if there are no transactions at all.
everywhere :: MonadModalBlockChain m => Tweak InterpMockChain b -> m a -> m a
everywhere x = modifyLtl (LtlFalsity `LtlRelease` LtlAtom (UntypedTweak x))

-- | Apply a 'Tweak' to the next transaction in the given trace. The order of
-- arguments is reversed compared to 'somewhere' and 'everywhere', because that
-- enables an idiom like
--
-- > do ...
-- >    endpoint arguments `withTweak` someModification
-- >    ...
--
-- where @endpoint@ builds and validates a single transaction depending on the
-- given @arguments@. Then `withTweak` says "I want to modify the transaction
-- returned by this endpoint in the following way".
withTweak :: MonadModalBlockChain m => m x -> Tweak InterpMockChain a -> m x
withTweak trace tweak = modifyLtl (LtlAtom $ UntypedTweak tweak) trace

-- * 'MonadBlockChain' and 'MonadMockChain' instances

singletonBuiltin :: builtin a -> Staged (LtlOp modification builtin) a
singletonBuiltin b = Instr (Builtin b) Return

instance MonadError MockChainError StagedMockChain where
  throwError = singletonBuiltin . ThrowError
  catchError act handler = singletonBuiltin $ CatchError act handler

instance MonadBlockChainBalancing StagedMockChain where
  getParams = singletonBuiltin GetParams
  datumFromHash = singletonBuiltin . DatumFromHash
  txOutByRefLedger = singletonBuiltin . TxOutByRefLedger
  utxosAtLedger = singletonBuiltin . UtxosAtLedger
  validatorFromHash = singletonBuiltin . ValidatorFromHash

instance MonadBlockChainWithoutValidation StagedMockChain where
  allUtxosLedger = singletonBuiltin AllUtxosLedger
  ownPaymentPubKeyHash = singletonBuiltin OwnPubKey
  currentSlot = singletonBuiltin GetCurrentSlot
  currentTime = singletonBuiltin GetCurrentTime
  awaitSlot = singletonBuiltin . AwaitSlot
  awaitTime = singletonBuiltin . AwaitTime

instance MonadBlockChain StagedMockChain where
  validateTxSkel = singletonBuiltin . ValidateTxSkel
