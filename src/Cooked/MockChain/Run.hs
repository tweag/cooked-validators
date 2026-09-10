-- | This module exposes the infrastructure to execute mockchain runs
module Cooked.MockChain.Run
  ( -- * Running mockchains
    RunnableMockChain (..),
    runMockChainFromConf,
    runMockChainFromInitDist,
    runMockChainFromInitDistTemplate,
    runMockChainDef,
  )
where

import Cooked.Effect.Override
import Cooked.MockChain.Config
import Cooked.Runtime.State
import Polysemy

-- | The class of effects that represent a mockchain run
class RunnableMockChain effs where
  -- | Runs a mockchain computation
  runMockChain :: EmulatorState -> ChainIndex -> Sem effs a -> [RawMockChainReturn a]

-- | Runs a `RunnableMockChain` from an initial `MockChainConf`
runMockChainFromConf ::
  ( RunnableMockChain effs,
    Member Override effs
  ) =>
  MockChainConf a b ->
  Sem effs a ->
  [b]
runMockChainFromConf (MockChainConf emInitState ciInitState initDist funOnResult) currentRun =
  fmap funOnResult $
    runMockChain emInitState ciInitState $
      forceOutputs initDist >> currentRun

-- | Runs a `RunnableMockChain` from an initial distribution
runMockChainFromInitDist ::
  ( RunnableMockChain effs,
    Member Override effs
  ) =>
  InitialDistribution ->
  Sem effs a ->
  [MockChainReturn a]
runMockChainFromInitDist initDist =
  runMockChainFromConf $ mockChainConfTemplate {mccInitialDistribution = initDist}

-- | Same as `runMockChainFromInitDist` using the `initialDistributionTemplate`
runMockChainFromInitDistTemplate ::
  ( RunnableMockChain effs,
    Member Override effs
  ) =>
  Sem effs a ->
  [MockChainReturn a]
runMockChainFromInitDistTemplate = runMockChainFromInitDist initialDistributionTemplate

-- | Runs a `RunnableMockChain` from a default configuration
runMockChainDef ::
  ( RunnableMockChain effs,
    Member Override effs
  ) =>
  Sem effs a ->
  [MockChainReturn a]
runMockChainDef = runMockChainFromConf mockChainConfTemplate
