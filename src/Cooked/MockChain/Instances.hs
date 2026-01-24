-- | This module exposes concrete instances to run a mockchain. There are 3 of
-- them :
--
-- - `DirectMockChain` exposes the minimal set of effects required to run a
--   mockchain, without the ability to branch or modify runs. Use this only if
--   you specifically want to disallow Ltl modifications (which behaves the same
--   in the absence of modifications). In should also perform somewhat better,
--   also in most cases this will be insignificant.
--
-- - `StagedMockChain` exposes all the primitives required to run a mockchain,
--   with the addition of branching and Ltl modifications using tweaks. This
--   should be the environement to use in 99% of the cases.
--
-- - `FullMockChain` exposes all the effects used to process a mockchain run,
--   including intermediate effects usually hidden. This should only be used
--   when the users requires to manually execute internal primitives of cooked,
--   such as balancing.
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
import Cooked.Tweak.Common
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

class MockChain effs where
  runMockChain :: MockChainState -> Sem effs a -> [RawMockChainReturn a]

runMockChainDef :: (MockChain effs) => Sem effs a -> [RawMockChainReturn a]
runMockChainDef = runMockChain def

-- | A default configuration to run a mockchain run.
mockChainConfTemplate ::
  (MockChain effs) =>
  Sem effs a ->
  MockChainConf effs a (MockChainReturn a)
mockChainConfTemplate currentRun =
  MockChainConf def def unRawMockChainReturn currentRun runMockChain

type DirectEffs =
  '[ MockChainWrite,
     MockChainRead,
     MockChainMisc,
     Fail
   ]

-- | A possible stack of effects to handle a direct interpretation of the
-- mockchain, that is without any tweaks nor branching.
type DirectMockChain a = Sem DirectEffs a

instance MockChain DirectEffs where
  runMockChain mcst =
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

type StagedTweakEffs = '[MockChainRead, Fail, NonDet]

type StagedTweak a = Sem (Tweak : NonDet : StagedTweakEffs) a

type StagedEffs =
  '[ ModifyGlobally (UntypedTweak StagedTweakEffs),
     MockChainWrite,
     MockChainMisc,
     MockChainRead,
     Fail,
     NonDet
   ]

-- | A possible stack of effects to handle staged interpretation of the
-- mockchain, that is with tweaks and branching.
type StagedMockChain a = Sem StagedEffs a

instance MockChain StagedEffs where
  runMockChain mcst =
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
        @[ ModifyLocally (UntypedTweak StagedTweakEffs),
           State [Ltl (UntypedTweak StagedTweakEffs)]
         ]

type FullTweakEffs =
  '[ MockChainRead,
     Fail,
     Error Ledger.ToCardanoError,
     Error MockChainError,
     State MockChainState,
     MockChainLog,
     Writer [MockChainLogEntry],
     Writer (Map Api.BuiltinByteString String),
     NonDet
   ]

type FullTweak a = Sem (Tweak : NonDet : FullTweakEffs) a

type FullEffs =
  '[ ModifyGlobally (UntypedTweak FullTweakEffs),
     MockChainWrite,
     ModifyLocally (UntypedTweak FullTweakEffs),
     State [Ltl (UntypedTweak FullTweakEffs)],
     MockChainMisc,
     MockChainRead,
     Fail,
     Error Ledger.ToCardanoError,
     Error MockChainError,
     State MockChainState,
     MockChainLog,
     Writer [MockChainLogEntry],
     Writer (Map Api.BuiltinByteString String),
     NonDet
   ]

type FullMockChain a = Sem FullEffs a

instance MockChain FullEffs where
  runMockChain mcst =
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
      . reinterpretMockChainWriteWithTweak
      . runModifyGlobally
