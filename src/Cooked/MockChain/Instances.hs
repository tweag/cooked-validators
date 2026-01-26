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
import Cooked.MockChain.Journal
import Cooked.MockChain.Log
import Cooked.MockChain.Misc
import Cooked.MockChain.Read
import Cooked.MockChain.State
import Cooked.MockChain.Tweak
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
      mcrLog :: [MockChainLogEntry],
      -- | The map of aliases defined during the run
      mcrAliases :: Map Api.BuiltinByteString String,
      -- | The notes taken by the user during the run
      mcrNoteBook :: [String]
    } ->
    MockChainReturn a
  deriving (Functor)

-- | Raw return type of running a 'MockChainT'
type RawMockChainReturn a = (MockChainJournal, (MockChainState, Either MockChainError a))

-- | The type of functions transforming an element of type @RawMockChainReturn a@
-- into an element of type @b@
type FunOnMockChainResult a b = RawMockChainReturn a -> b

-- | Building a `MockChainReturn` from a `RawMockChainReturn`
unRawMockChainReturn :: FunOnMockChainResult a (MockChainReturn a)
unRawMockChainReturn (MockChainJournal journal aliases notes, (st, val)) =
  MockChainReturn val (mcstOutputs st) (mcstToUtxoState st) journal aliases notes

-- | Configuration to run a mockchain
data MockChainConf a b where
  MockChainConf ::
    { -- | The initial state from which to run the 'MockChainT'
      mccInitialState :: MockChainState,
      -- | The initial payments to issue in the run
      mccInitialDistribution :: InitialDistribution,
      -- | The function to apply on the results of the run
      mccFunOnResult :: FunOnMockChainResult a b
    } ->
    MockChainConf a b

mockChainConfTemplate :: MockChainConf a (MockChainReturn a)
mockChainConfTemplate = MockChainConf def def unRawMockChainReturn

class IsMockChain effs where
  runMockChain :: MockChainState -> Sem effs a -> [RawMockChainReturn a]

runMockChainFromConf ::
  ( IsMockChain effs,
    Member MockChainWrite effs
  ) =>
  MockChainConf a b ->
  Sem effs a ->
  [b]
runMockChainFromConf (MockChainConf initState initDist funOnResult) currentRun =
  funOnResult <$> runMockChain initState (forceOutputs (unInitialDistribution initDist) >> currentRun)

runMockChainFromInitDist ::
  ( IsMockChain effs,
    Member MockChainWrite effs
  ) =>
  InitialDistribution ->
  Sem effs a ->
  [MockChainReturn a]
runMockChainFromInitDist initDist =
  runMockChainFromConf $ mockChainConfTemplate {mccInitialDistribution = initDist}

runMockChainDef ::
  ( IsMockChain effs,
    Member MockChainWrite effs
  ) =>
  Sem effs a ->
  [MockChainReturn a]
runMockChainDef = runMockChainFromConf mockChainConfTemplate

type DirectEffs =
  '[ MockChainWrite,
     MockChainRead,
     MockChainMisc,
     Fail
   ]

-- | A possible stack of effects to handle a direct interpretation of the
-- mockchain, that is without any tweaks nor branching.
type DirectMockChain a = Sem DirectEffs a

instance IsMockChain DirectEffs where
  runMockChain mcst =
    (: [])
      . run
      . runWriter
      . runMockChainLog fromLogEntry
      . runState mcst
      . runError
      . runToCardanoErrorInMockChainError
      . runFailInMockChainError
      . runMockChainMisc fromAlias fromNote
      . runMockChainRead
      . runMockChainWrite
      . insertAt @4
        @[ Error Ledger.ToCardanoError,
           Error MockChainError,
           State MockChainState,
           MockChainLog,
           Writer MockChainJournal
         ]

type StagedTweakEffs = '[MockChainRead, Fail, NonDet]

type StagedTweak a = TypedTweak StagedTweakEffs a

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

instance IsMockChain StagedEffs where
  runMockChain mcst =
    run
      . runNonDet
      . runWriter
      . runMockChainLog fromLogEntry
      . runState mcst
      . runError
      . runToCardanoErrorInMockChainError
      . runFailInMockChainError
      . runMockChainRead
      . runMockChainMisc fromAlias fromNote
      . evalState []
      . runModifyLocally
      . runMockChainWrite
      . insertAt @6
        @[ Error Ledger.ToCardanoError,
           Error MockChainError,
           State MockChainState,
           MockChainLog,
           Writer MockChainJournal
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
     Writer MockChainJournal,
     NonDet
   ]

type FullTweak a = TypedTweak FullTweakEffs a

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
     Writer MockChainJournal,
     NonDet
   ]

type FullMockChain a = Sem FullEffs a

instance IsMockChain FullEffs where
  runMockChain mcst =
    run
      . runNonDet
      . runWriter
      . runMockChainLog fromLogEntry
      . runState mcst
      . runError
      . runToCardanoErrorInMockChainError
      . runFailInMockChainError
      . runMockChainRead
      . runMockChainMisc fromAlias fromNote
      . evalState []
      . runModifyLocally
      . runMockChainWrite
      . reinterpretMockChainWriteWithTweak
      . runModifyGlobally
