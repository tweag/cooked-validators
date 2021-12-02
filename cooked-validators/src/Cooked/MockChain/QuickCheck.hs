{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cooked.MockChain.QuickCheck where

import Control.Monad.Writer
import Cooked.MockChain.Monad
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.Monad.Staged
import Cooked.MockChain.UtxoState
import Cooked.Tx.Constraints.Type
import Data.Either (isLeft, isRight)
import QuickCheck.GenT
import Test.QuickCheck (Property)
import qualified Test.QuickCheck as QC

type GenTrace a = forall m. (MonadMockChain m) => GenT m a

-- | Enables the user to quantify over possible traces from a distribution of traces
--  written through 'GenT' and 'MonadMockChain'. In case of failure, the user will
--  see a descripion of the transactions that were issued by the generated trace.
--
--  This generator does /not/ shrink the trace. The reason for that is that
--  shrinking the trace might cause the underlying computation in 'MockChain' to fail,
--  making quickcheck believe it found a smaller value that falsifies the test where,
--  in reality, the test failed for entirely different reasons.
after ::
  forall a.
  GenTrace a ->
  (Either MockChainError (a, UtxoState) -> Property) ->
  Property
after trGen prop = after' (pure ()) (const trGen) (const prop)

afterMod ::
  forall a.
  GenTrace a ->
  (TxSkel -> TxSkel) ->
  (Either MockChainError (a, UtxoState) -> Property) ->
  Property
afterMod trGen f prop =
  QC.forAllShrinkBlind (runGenT trGen) (const []) go
  where
    go :: StagedMockChain a -> QC.Property
    go smc =
      let traces = runWriterT $ runMockChainT (onOne f smc)
       in QC.conjoin $ map (\(res, descr) -> QC.counterexample (show descr) (prop res)) traces

-- | Analogous to 'after', but also generates some setup information; this can be useful
--  when we need the information about the setup to decide whether the test should pass or fail.
after' ::
  forall a setup.
  Show setup =>
  Gen setup ->
  (setup -> GenTrace a) ->
  (setup -> Either MockChainError (a, UtxoState) -> Property) ->
  Property
after' genSetup trGen prop =
  QC.forAll genSetup $ \s ->
    QC.forAllShrinkBlind (runGenT $ trGen s) (const []) (go s)
  where
    go :: setup -> StagedMockChain a -> QC.Property
    go s smc =
      let (res, descr) = runWriter $ runMockChainT (interpretWithDescr smc)
       in QC.counterexample (show descr) (prop s res)

traceSucceeds :: GenTrace a -> Property
traceSucceeds = (`after` (QC.property . isRight))

traceFails :: GenTrace a -> Property
traceFails = (`after` (QC.property . isLeft))
