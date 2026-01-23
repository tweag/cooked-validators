module Cooked.MockChain.Instances where

import Cooked.InitialDistribution
import Cooked.Ltl
import Cooked.MockChain.Error
import Cooked.MockChain.Log
import Cooked.MockChain.Misc
import Cooked.MockChain.MockChainState
import Cooked.MockChain.Read
import Cooked.MockChain.UtxoState
import Cooked.MockChain.Write
import Cooked.Skeleton.Output
import Data.Default
import Data.Map (Map)
import Ledger.Tx qualified as Ledger
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.NonDet
import Polysemy.State
import Polysemy.Writer

-- * 'MockChain' return types

-- | The returned type when running a 'MockChainT'. This is both a reorganizing
-- and filtering of the natural returned type @((Either MockChainError a,
-- MockChainState), MockChainBook)@, which is much easier to query.
data MockChainReturn a where
  MockChainReturn ::
    { -- | The value returned by the computation, or an error
      mcrValue :: Either MockChainError a,
      -- | The outputs at the end of the run
      mcrOutputs :: Map Api.TxOutRef (TxSkelOut, Bool),
      -- | The 'UtxoState' at the end of the run
      mcrUtxoState :: UtxoState,
      -- | The final journal emitted during the run
      mcrJournal :: [MockChainLogEntry],
      -- | The map of aliases defined during the run
      mcrAliases :: Map Api.BuiltinByteString String
    } ->
    MockChainReturn a
  deriving (Functor)

-- | Raw return type of running a 'MockChainT'
type RawMockChainReturn a =
  ( Map Api.BuiltinByteString String,
    ( [MockChainLogEntry],
      ( MockChainState,
        Either MockChainError a
      )
    )
  )

-- | The type of functions transforming an element of type @RawMockChainReturn a@
-- into an element of type @b@
type FunOnMockChainResult a b = RawMockChainReturn a -> b

-- | Building a `MockChainReturn` from a `RawMockChainReturn`
unRawMockChainReturn :: FunOnMockChainResult a (MockChainReturn a)
unRawMockChainReturn (aliases, (journal, (st, val))) =
  MockChainReturn val (mcstOutputs st) (mcstToUtxoState st) journal aliases

-- | Retrieving the `MockChainState` from a `RawMockChainReturn`
stateFromMockChainReturn :: FunOnMockChainResult a MockChainState
stateFromMockChainReturn = fst . snd . snd

-- | Configuration to run a mockchain
data MockChainConf effs a b where
  MockChainConf ::
    { -- | The initial state from which to run the 'MockChainT'
      mccInitialState :: MockChainState,
      -- | The initial payments to issue in the run
      mccInitialDistribution :: InitialDistribution,
      -- | The function to apply on the results of the run
      mccFunOnResult :: FunOnMockChainResult a b,
      -- | The actual run to execute
      mccRun :: Sem effs a,
      -- | The interpreter for the run. We always expect several possible
      -- outcomes for a run, even when the effect stack does not make use of
      -- `NonDet` in which case the list will be a singleton.
      mccRunner :: forall a'. MockChainState -> Sem effs a' -> [RawMockChainReturn a']
    } ->
    MockChainConf effs a b

-- | Running a mockchain conf to get a list of results of the expected type
runMockChainConf ::
  (Member MockChainWrite effs) =>
  MockChainConf effs a b ->
  [b]
runMockChainConf (MockChainConf initialState initialDist funOnRes currentRun runner) =
  funOnRes <$> runner initialState (forceOutputs (unInitialDistribution initialDist) >> currentRun)

type DirectEffs =
  '[ MockChainWrite,
     MockChainRead,
     MockChainMisc,
     Fail
   ]

-- | A possible stack of effects to handle a direct interpretation of the
-- mockchain, that is without any tweaks nor branching.
type MockChainDirect a = Sem DirectEffs a

runMockChainDirect :: MockChainState -> MockChainDirect a -> [RawMockChainReturn a]
runMockChainDirect mcst =
  (: [])
    . run
    . runWriter
    . runWriter
    . runMockChainLog
    . runState mcst
    . runError
    . runToCardanoErrorInMockChainError
    . runFailInMockChainError
    . runMockChainMisc
    . runMockChainRead
    . runMockChainWrite
    . insertAt @4
      @[ Error Ledger.ToCardanoError,
         Error MockChainError,
         State MockChainState,
         MockChainLog,
         Writer [MockChainLogEntry],
         Writer (Map Api.BuiltinByteString String)
       ]

-- | A default configuration to run a direct mockchain run. The intended usage
-- is @runMockChainConf $ mockChainConfDirectTemplate myDirectRun@.
mockChainConfDirectTemplate ::
  MockChainDirect a ->
  MockChainConf DirectEffs a (MockChainReturn a)
mockChainConfDirectTemplate currentRun =
  MockChainConf def def unRawMockChainReturn currentRun runMockChainDirect

type TweakEffs = '[MockChainRead, Fail, NonDet]

type FullEffs =
  '[ ModifyGlobally (UntypedTweak TweakEffs),
     MockChainWrite,
     MockChainMisc,
     MockChainRead,
     Fail,
     NonDet
   ]

-- | A possible stack of effects to handle staged interpretation of the
-- mockchain, that is with tweaks and branching.
type StagedMockChain a = Sem FullEffs a

runStagedMockChain ::
  MockChainState ->
  StagedMockChain a ->
  [RawMockChainReturn a]
runStagedMockChain mcst =
  run
    . runNonDet
    . runWriter
    . runWriter
    . runMockChainLog
    . runState mcst
    . runError
    . runToCardanoErrorInMockChainError
    . runFailInMockChainError
    . runMockChainRead
    . runMockChainMisc
    . evalState []
    . runModifyLocally
    . runMockChainWrite
    . insertAt @6
      @[ Error Ledger.ToCardanoError,
         Error MockChainError,
         State MockChainState,
         MockChainLog,
         Writer [MockChainLogEntry],
         Writer (Map Api.BuiltinByteString String)
       ]
    . reinterpretMockChainWriteWithTweak
    . runModifyGlobally
    . insertAt @2
      @[ ModifyLocally (UntypedTweak TweakEffs),
         State [Ltl (UntypedTweak TweakEffs)]
       ]

-- | A default configuration to run a staged mockchain run. The intended usage
-- is @runMockChainConf $ mockChainConfFullTemplate myFullRun@.
mockChainConfFullTemplate ::
  StagedMockChain a ->
  MockChainConf FullEffs a (MockChainReturn a)
mockChainConfFullTemplate currentRun =
  MockChainConf def def unRawMockChainReturn currentRun runStagedMockChain
