-- | This module exposes the infrastructure to execute mockchain runs. In
-- particular:
--
-- - The return types of the runs (raw and refined)
--
-- - The initial configuration with which to execute a run
--
-- - The notion of `RunnableMockChain` to actually execution computations
module Cooked.MockChain.Runnable where

import Cooked.InitialDistribution
import Cooked.MockChain.Error
import Cooked.MockChain.Journal
import Cooked.MockChain.Log
import Cooked.MockChain.State
import Cooked.MockChain.Write
import Cooked.Skeleton.Output
import Data.Default
import Data.Map (Map)
import PlutusLedgerApi.V3 qualified as Api
import Polysemy

-- | Raw return type of running a mockchain
type RawMockChainReturn a = (MockChainJournal, (MockChainState, Either MockChainError a))

-- | The returned type when running a mockchain. This is both a reorganizing and
-- filtering of the natural returned type `RawMockChainReturn`.
data MockChainReturn a where
  MockChainReturn ::
    { -- | The value returned by the computation, or an error
      mcrValue :: Either MockChainError a,
      -- | The outputs at the end of the run
      mcrOutputs :: Map Api.TxOutRef (TxSkelOut, Bool),
      -- | The 'UtxoState' at the end of the run
      mcrUtxoState :: UtxoState,
      -- | The final journal emitted during the run
      mcrLog :: [MockChainLogEntry],
      -- | The map of aliases defined during the run
      mcrAliases :: Map Api.BuiltinByteString String,
      -- | The notes taken by the user during the run
      mcrNoteBook :: [String]
    } ->
    MockChainReturn a
  deriving (Functor)

-- | The type of functions transforming an element of type @RawMockChainReturn a@
-- into an element of type @b@
type FunOnMockChainResult a b = RawMockChainReturn a -> b

-- | Building a `MockChainReturn` from a `RawMockChainReturn`
unRawMockChainReturn :: FunOnMockChainResult a (MockChainReturn a)
unRawMockChainReturn (MockChainJournal journal aliases notes, (st, val)) =
  MockChainReturn val (mcstOutputs st) (mcstToUtxoState st) journal aliases notes

-- | Configuration from which to run a mockchain
data MockChainConf a b where
  MockChainConf ::
    { -- | The initial state from which to run the mockchain
      mccInitialState :: MockChainState,
      -- | The initial payments to issue in the run
      mccInitialDistribution :: InitialDistribution,
      -- | The function to apply on the results of the run
      mccFunOnResult :: FunOnMockChainResult a b
    } ->
    MockChainConf a b

-- | The default `MockChainConf`, which uses the default initial state and
-- initial distribution, and returns a refined `MockChainReturn`
mockChainConfTemplate :: MockChainConf a (MockChainReturn a)
mockChainConfTemplate = MockChainConf def def unRawMockChainReturn

-- | The class of effects that represent a mockchain run
class RunnableMockChain effs where
  -- | Runs a computation from an initial `MockChainState`, while returning a
  -- list of `RawMockChainReturn`
  runMockChain :: MockChainState -> Sem effs a -> [RawMockChainReturn a]

-- | Runs a `RunnableMockChain` from an initial `MockChainConf`
runMockChainFromConf ::
  ( RunnableMockChain effs,
    Member MockChainWrite effs
  ) =>
  MockChainConf a b ->
  Sem effs a ->
  [b]
runMockChainFromConf (MockChainConf initState initDist funOnResult) currentRun =
  funOnResult <$> runMockChain initState (forceOutputs (unInitialDistribution initDist) >> currentRun)

-- | Runs a `RunnableMockChain` from an initial distribution
runMockChainFromInitDist ::
  ( RunnableMockChain effs,
    Member MockChainWrite effs
  ) =>
  InitialDistribution ->
  Sem effs a ->
  [MockChainReturn a]
runMockChainFromInitDist initDist =
  runMockChainFromConf $ mockChainConfTemplate {mccInitialDistribution = initDist}

-- | Runs a `RunnableMockChain` from a default configuration
runMockChainDef ::
  ( RunnableMockChain effs,
    Member MockChainWrite effs
  ) =>
  Sem effs a ->
  [MockChainReturn a]
runMockChainDef = runMockChainFromConf mockChainConfTemplate
