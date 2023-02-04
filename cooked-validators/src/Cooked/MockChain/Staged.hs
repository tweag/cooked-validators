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

import Control.Applicative
import Control.Arrow hiding ((<+>))
import Control.Monad.Except
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
import qualified Ledger as Pl
import qualified Plutus.V2.Ledger.Api as PV2

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
  ValidateTxSkel :: TxSkel -> MockChainBuiltin Pl.CardanoTx
  TxOutByRef :: Pl.TxOutRef -> MockChainBuiltin (Maybe PV2.TxOut)
  GetCurrentSlot :: MockChainBuiltin Pl.Slot
  AwaitSlot :: Pl.Slot -> MockChainBuiltin Pl.Slot
  DatumFromHash :: Pl.DatumHash -> MockChainBuiltin (Maybe (Pl.Datum, DocCooked))
  OwnPubKey :: MockChainBuiltin Pl.PubKeyHash
  AllUtxos :: MockChainBuiltin [(Pl.TxOutRef, PV2.TxOut)]
  -- the following are not strictly blockchain specific, but they allow us to
  -- combine several traces into one and to signal failure.

  -- | The empty set of traces
  Empty :: MockChainBuiltin a
  -- | The union of two sets of traces
  Alt ::
    StagedMockChain a ->
    StagedMockChain a ->
    MockChainBuiltin a
  -- | The failing operation
  Fail :: String -> MockChainBuiltin a

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
  interpBuiltin (ValidateTxSkel skel) =
    get
      >>= msum
        . map (uncurry interpretAndTell)
        . nowLaterList
    where
      interpretAndTell ::
        UntypedTweak InterpMockChain ->
        [Ltl (UntypedTweak InterpMockChain)] ->
        StateT [Ltl (UntypedTweak InterpMockChain)] InterpMockChain Pl.CardanoTx
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
              [MCLogNewTx (Pl.getCardanoTxId tx)]
        put later
        return tx
  interpBuiltin (TxOutByRef o) = txOutByRef o
  interpBuiltin GetCurrentSlot = currentSlot
  interpBuiltin (AwaitSlot s) = awaitSlot s
  interpBuiltin (DatumFromHash h) = datumFromHash h
  interpBuiltin OwnPubKey = ownPaymentPubKeyHash
  interpBuiltin AllUtxos = allUtxos
  interpBuiltin Empty = mzero
  interpBuiltin (Alt l r) = interpLtl l `mplus` interpLtl r
  interpBuiltin (Fail msg) = do
    lift $ lift $ tell [MCLogFail msg]
    fail msg

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

instance MonadBlockChainWithoutValidation StagedMockChain where
  datumFromHash = singletonBuiltin . DatumFromHash
  allUtxos = singletonBuiltin AllUtxos
  txOutByRef = singletonBuiltin . TxOutByRef
  ownPaymentPubKeyHash = singletonBuiltin OwnPubKey
  currentSlot = singletonBuiltin GetCurrentSlot
  awaitSlot = singletonBuiltin . AwaitSlot

instance MonadBlockChain StagedMockChain where
  validateTxSkel = singletonBuiltin . ValidateTxSkel
