{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.Monad.Staged where

import Control.Applicative
import Control.Arrow hiding ((<+>))
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer.Strict hiding (Alt)
import Cooked.Attack.Tweak.Common
import Cooked.Ltl
import Cooked.MockChain.Monad
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.UtxoState
import Cooked.Pretty
import Cooked.Tx.Constraints.Type
import Data.Default
import Data.Map (Map)
import qualified Ledger as Pl
import qualified Plutus.V2.Ledger.Api as PV2
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PP

-- * Interpreting and running 'StagedMockChain'

-- | Interprets the staged mockchain then runs the resulting computation
-- with a custom function. This can be used, for example, to supply
-- a custom 'InitialDistribution' by providing 'runMockChainTFrom'.
interpretAndRunWith ::
  (forall m. Monad m => MockChainT m a -> m res) ->
  StagedMockChain a ->
  [(res, TraceDescr)]
interpretAndRunWith f smc = runWriterT $ f $ interpret smc

interpretAndRun ::
  StagedMockChain a ->
  [(Either MockChainError (a, UtxoState), TraceDescr)]
interpretAndRun = interpretAndRunWith runMockChainT

-- | The semantic domain in which 'StagedMockChain' gets interpreted; see
--  the 'interpret' function for more.
type InterpMockChain = MockChainT (WriterT TraceDescr [])

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
  GetCurrentTime :: MockChainBuiltin Pl.POSIXTime
  AwaitTime :: Pl.POSIXTime -> MockChainBuiltin Pl.POSIXTime
  DatumFromHash :: Pl.DatumHash -> MockChainBuiltin (Maybe (Pl.Datum, String))
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
        lift $ lift $ tell $ prettyMockChainOp managedTxOuts managedDatums $ Builtin $ ValidateTxSkel skel'
        tx <- validateTxSkel skel'
        put later
        return tx
  interpBuiltin (TxOutByRef o) = txOutByRef o
  interpBuiltin GetCurrentSlot = currentSlot
  interpBuiltin (AwaitSlot s) = awaitSlot s
  interpBuiltin GetCurrentTime = currentTime
  interpBuiltin (AwaitTime t) = awaitTime t
  interpBuiltin (DatumFromHash h) = datumFromHash h
  interpBuiltin OwnPubKey = ownPaymentPubKeyHash
  interpBuiltin AllUtxos = allUtxos
  interpBuiltin Empty = mzero
  interpBuiltin (Alt l r) = interpLtl l `mplus` interpLtl r
  interpBuiltin (Fail msg) = do
    mcst <- lift get
    let managedTxOuts = utxoIndexToTxOutMap . mcstIndex $ mcst
        managedDatums = mcstDatums mcst
    lift $ lift $ tell $ prettyMockChainOp managedTxOuts managedDatums $ Builtin $ Fail msg
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
  currentTime = singletonBuiltin GetCurrentTime
  awaitSlot = singletonBuiltin . AwaitSlot
  awaitTime = singletonBuiltin . AwaitTime

instance MonadBlockChain StagedMockChain where
  validateTxSkel = singletonBuiltin . ValidateTxSkel

-- * Human Readable Traces

-- | Generates a 'TraceDescr'iption for the given operation; we're mostly interested in seeing
--  the transactions that were validated, so many operations have no description.
prettyMockChainOp :: Map Pl.TxOutRef PV2.TxOut -> Map Pl.DatumHash (Pl.Datum, String) -> MockChainOp a -> TraceDescr
prettyMockChainOp managedTxOuts managedDatums (Builtin (ValidateTxSkel skel)) =
  trSingleton $
    PP.hang 2 $
      PP.vsep ["ValidateTxSkel", prettyTxSkel managedTxOuts managedDatums skel]
prettyMockChainOp _ _ (Builtin (Fail reason)) =
  trSingleton $ PP.hang 2 $ PP.vsep ["Fail", PP.pretty reason]
prettyMockChainOp _ _ _ = mempty

-- | A 'TraceDescr' is a list of 'Doc' encoded as a difference list for
--  two reasons (check 'ShowS' if you're confused about how this works, its the same idea).
--    1) Naturally, these make for efficient concatenation
--    2) More importantly, this makes it easy to define the empty 'TraceDescr'
--       as @TraceDescr id@ instead of relying on 'PP.emptyDoc', which generates
--       empty lines when used with 'PP.vsep'. This avoids generating these empty lines
newtype TraceDescr = TraceDescr {trApp :: [Doc ()] -> [Doc ()]}

trSingleton :: Doc ann -> TraceDescr
trSingleton d = TraceDescr (void d :)

instance Show TraceDescr where
  show (TraceDescr gen) =
    let tr = gen []
        numbered = zipWith (\n d -> PP.pretty n <> ")" <+> PP.align d) [1 :: Integer ..] tr
     in PP.renderString . PP.layoutPretty PP.defaultLayoutOptions $ PP.vsep numbered

instance Semigroup TraceDescr where
  x <> y = TraceDescr $ trApp x . trApp y

instance Monoid TraceDescr where
  mempty = TraceDescr id
