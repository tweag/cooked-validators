{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cooked.MockChain.QuickCheck where

import Control.Monad.Writer
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.Monad.Staged
import Cooked.MockChain.UtxoState
import Data.Either (isLeft, isRight)
import Test.QuickCheck (Gen, Property)
import qualified Test.QuickCheck as QC
import Test.QuickCheck.GenT

-- TODO: Write doc about traces; specifically targetting their
-- semantics since there are two potentially confusing layers:
--
-- 1. GenT: providing a distribution of lists of traces
-- 2. everywhere/somewhere: providing lists of traces
--
-- OLD COMMENT; incorporate later:
--  Enables the user to quantify over possible traces from a distribution of traces
--  written through 'GenT' and 'MonadBlockChain'. In case of failure, the user will
--  see a descripion of the transactions that were issued by the generated trace.
--
--  This generator does /not/ shrink the trace. The reason for that is that
--  shrinking the trace might cause the underlying computation in 'MockChain' to fail,
--  making quickcheck believe it found a smaller value that falsifies the test where,
--  in reality, the test failed for entirely different reasons.

-- | The type of trace generators parameterized by some values.
type GenTraceParm parm a = parm -> GenT StagedMockChain a

-- | The type of trace generators with no paramters
type GenTrace a = GenT StagedMockChain a

-- | Very general quantification mechanism for traces. This function is mostly used
--  internally as the implementation for its simpler cousins like 'forAllTr', 'forSomeTr',
--  'forAllTrP' and 'forSomeP'.
quantifyOverTraces ::
  forall parm a.
  Gen parm ->
  (parm -> String) ->
  GenTraceParm parm a ->
  (parm -> Either MockChainError (a, UtxoState) -> Property) ->
  ([Property] -> Property) ->
  Property
quantifyOverTraces gParm showParm gTr prop collect =
  QC.forAllBlind gParm $ \parm ->
    showIfNotNull (showParm parm) $
      QC.forAllBlind (runGenT $ gTr parm) (go parm)
  where
    showIfNotNull :: String -> QC.Property -> QC.Property
    showIfNotNull [] = id
    showIfNotNull s = QC.counterexample s

    go :: parm -> StagedMockChain a -> QC.Property
    go parm smc =
      let traces = runWriterT $ runMockChainT $ interpret smc
       in collect $ map (\(res, descr) -> QC.counterexample (show descr) (prop parm res)) traces

-- | Universal quantification over traces. We expect the property to hold for
--  all traces generated by the provided generator. This is probably the flavour
--  of property based test you will need the most.
forAllTr ::
  GenTrace a ->
  (Either MockChainError (a, UtxoState) -> Property) ->
  Property
forAllTr gTr prop =
  quantifyOverTraces
    (return ())
    (const "")
    (const gTr)
    (const prop)
    QC.conjoin

forAllTrP ::
  (Show parm) =>
  Gen parm ->
  GenTraceParm parm a ->
  (parm -> Either MockChainError (a, UtxoState) -> Property) ->
  Property
forAllTrP gParm gTr prop =
  quantifyOverTraces
    gParm
    show
    gTr
    prop
    QC.conjoin

forSomeTr ::
  GenTrace a ->
  (Either MockChainError (a, UtxoState) -> Property) ->
  Property
forSomeTr gTr prop =
  quantifyOverTraces
    (return ())
    (const "")
    (const gTr)
    (const prop)
    QC.disjoin

forSomeTrP ::
  (Show parm) =>
  Gen parm ->
  GenTraceParm parm a ->
  (parm -> Either MockChainError (a, UtxoState) -> Property) ->
  Property
forSomeTrP gParm gTr prop =
  quantifyOverTraces
    gParm
    show
    gTr
    prop
    QC.disjoin

traceSucceeds :: GenTrace a -> Property
traceSucceeds = (`forAllTr` (QC.property . isRight))

traceFails :: GenTrace a -> Property
traceFails = (`forAllTr` (QC.property . isLeft))
