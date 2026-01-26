{-# OPTIONS_GHC -Wno-orphans #-}

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

import Cooked.Ltl
import Cooked.MockChain.Error
import Cooked.MockChain.Journal
import Cooked.MockChain.Log
import Cooked.MockChain.Misc
import Cooked.MockChain.Read
import Cooked.MockChain.Runnable
import Cooked.MockChain.State
import Cooked.MockChain.Tweak
import Cooked.MockChain.Write
import Ledger.Tx qualified as Ledger
import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.NonDet
import Polysemy.State
import Polysemy.Writer

type DirectEffs =
  '[ MockChainWrite,
     MockChainRead,
     MockChainMisc,
     Fail
   ]

-- | A possible stack of effects to handle a direct interpretation of the
-- mockchain, that is without any tweaks nor branching.
type DirectMockChain a = Sem DirectEffs a

instance RunnableMockChain DirectEffs where
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

instance RunnableMockChain StagedEffs where
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

instance RunnableMockChain FullEffs where
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
