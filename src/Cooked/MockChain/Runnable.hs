-- | This module exposes the infrastructure to execute mockchain runs. In
-- particular:
--
-- - The notion of initial distribution (a list of payments)
--
-- - The return types of the runs (raw and refined)
--
-- - The initial configuration with which to execute a run
--
-- - The notion of `RunnableMockChain` to actually execute computations
module Cooked.MockChain.Runnable where

import Cooked.MockChain.Error
import Cooked.MockChain.Journal
import Cooked.MockChain.State
import Cooked.MockChain.Write
import Cooked.Skeleton.Output
import Cooked.Wallet
import Data.Default
import Data.List (foldl')
import Data.Map (Map)
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy

-- * Initial distribution of funds

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
-- enough ADA to sustain themselves.
data InitialDistribution where
  InitialDistribution ::
    {unInitialDistribution :: [TxSkelOut]} ->
    InitialDistribution

-- | 4 UTxOs with 100 Ada each, for each of the first 4 'knownWallets'
instance Default InitialDistribution where
  def =
    distributionFromList
      . zip (take 4 knownWallets)
      . repeat
      . replicate 4
      $ Script.ada 100

instance Semigroup InitialDistribution where
  i <> j = InitialDistribution (unInitialDistribution i <> unInitialDistribution j)

instance Monoid InitialDistribution where
  mempty = InitialDistribution mempty

-- | Creating a initial distribution with simple values assigned to owners
distributionFromList :: (IsTxSkelOutAllowedOwner owner) => [(owner, [Api.Value])] -> InitialDistribution
distributionFromList =
  InitialDistribution
    . foldl' (\x (user, values) -> x <> map (receives user . Value) values) []

-- | Raw return type of running a mockchain
type RawMockChainReturn a =
  (MockChainJournal, (MockChainState, Either MockChainError a))

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
      mcrJournal :: MockChainJournal
    } ->
    MockChainReturn a
  deriving (Functor)

-- | The type of functions transforming an element of type @RawMockChainReturn a@
-- into an element of type @b@
type FunOnMockChainResult a b = RawMockChainReturn a -> b

-- | Building a `MockChainReturn` from a `RawMockChainReturn`
unRawMockChainReturn :: FunOnMockChainResult a (MockChainReturn a)
unRawMockChainReturn (journal, (st, val)) =
  MockChainReturn val (mcstOutputs st) (mcstToUtxoState st) journal

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
  fmap funOnResult $
    runMockChain initState $
      forceOutputs (unInitialDistribution initDist) >> currentRun

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
