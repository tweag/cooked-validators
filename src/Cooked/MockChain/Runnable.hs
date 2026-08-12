-- | This module exposes the infrastructure to execute mockchain and blockchain
-- runs, in particular initial configurations, results, and running functions.
module Cooked.MockChain.Runnable
  ( -- * Initial distributions
    InitialDistribution,
    initialDistributionTemplate,
    distributionFromList,

    -- * Initial mockchain configurations
    MockChainConf (..),
    mockChainConfTemplate,

    -- * Mockchain run return type
    RawMockChainReturn,
    MockChainReturn (..),
    FunOnMockChainResult,
    unRawMockChainReturn,

    -- * Running mockchains
    RunnableMockChain (..),
    runMockChainFromConf,
    runMockChainFromInitDist,
    runMockChainFromInitDistTemplate,
    runMockChainDef,
  )
where

import Cooked.Effect.Override
import Cooked.Runtime.Error
import Cooked.Runtime.Journal
import Cooked.Runtime.State
import Cooked.Skeleton.Output
import Cooked.Utilities.Wallet
import Data.Default
import Data.List (foldl')
import Data.Map (Map)
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy

-- | Describes the initial distribution of UTxOs per user.
--
--  The following specifies a starting state where @wallet 1@ owns two UTxOs,
--  one with 42 Ada and one with 2 Ada and one "TOK" token; @wallet 2@ owns a
--  single UTxO with 10 Ada and @wallet 3@ has 10 Ada and a permanent value
--
--  > i0 = distributionFromList $
--  >        [ (wallet 1 , [ ada 42 , ada 2 <> quickValue "TOK" 1 ]
--  >        , (wallet 2 , [ ada 10 ])
--  >        , (wallet 3 , [ ada 10 <> permanentValue "XYZ" 10])
--  >        ]
--
-- Note that payment issued through an initial distribution will be attached
-- enough ADA to sustain themselves unless a fixed value is explicitly required.
type InitialDistribution = [TxSkelOut]

-- | 4 UTxOs with 100 Ada each, for each of the first 4 'knownWallets'
initialDistributionTemplate :: InitialDistribution
initialDistributionTemplate =
  distributionFromList
    . zip (take 4 knownWallets)
    . repeat
    . replicate 4
    $ Script.ada 100

-- | Creating a initial distribution with simple values assigned to owners
distributionFromList :: (IsTxSkelOutAllowedOwner owner) => [(owner, [Api.Value])] -> InitialDistribution
distributionFromList = foldl' (\x (user, values) -> x <> map (receives user . Value) values) []

-- | Raw return type of running a mockchain
type RawMockChainReturn a =
  (ChainJournal, (ChainIndex, (EmulatorState, Either ChainError a)))

-- | The returned type when running a mockchain. This is both a reorganizing and
-- filtering of the natural returned type `RawMockChainReturn`.
data MockChainReturn a where
  MockChainReturn ::
    { -- | The value returned by the computation, or an error
      mcrValue :: Either ChainError a,
      -- | The outputs at the end of the run
      mcrOutputs :: Map Api.TxOutRef (TxSkelOut, Bool),
      -- | The 'UtxoState' at the end of the run
      mcrUtxoState :: UtxoState,
      -- | The final journal emitted during the run
      mcrJournal :: ChainJournal
    } ->
    MockChainReturn a
  deriving (Functor)

-- | The type of functions transforming an element of type @RawMockChainReturn a@
-- into an element of type @b@
type FunOnMockChainResult a b = RawMockChainReturn a -> b

-- | Building a `MockChainReturn` from a `RawMockChainReturn`
unRawMockChainReturn :: FunOnMockChainResult a (MockChainReturn a)
unRawMockChainReturn (journal, (chainIndex, (_emulatorState, val))) =
  MockChainReturn val (chainIndexOutputs chainIndex) (chainIndexToUtxoState chainIndex) journal

-- | Configuration from which to run a mockchain
data MockChainConf a b where
  MockChainConf ::
    { -- | The initial emulator state from which to run the mockchain
      mccInitialEmulatorState :: EmulatorState,
      -- | The initial chain index from which to run the mockchain
      mccInitialChainIndex :: ChainIndex,
      -- | The initial payments to issue in the run
      mccInitialDistribution :: InitialDistribution,
      -- | The function to apply on the results of the run
      mccFunOnResult :: FunOnMockChainResult a b
    } ->
    MockChainConf a b

-- | The default `MockChainConf`, which uses the default initial states and
-- initial distribution, and returns a refined `MockChainReturn`
mockChainConfTemplate :: MockChainConf a (MockChainReturn a)
mockChainConfTemplate = MockChainConf def def def unRawMockChainReturn

-- | The class of effects that represent a mockchain run
class RunnableMockChain effs where
  -- | Runs a computation from an initial `EmulatorState` and `ChainIndex`,
  -- while returning a list of `RawMockChainReturn`
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
