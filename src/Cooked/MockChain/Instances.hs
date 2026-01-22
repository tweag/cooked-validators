module Cooked.MockChain.Instances where

import Cooked.MockChain.Misc
import Cooked.MockChain.Read
import Cooked.MockChain.Write

-- * MockChainDirect

-- | A possible stack of effects to handle a direct interpretation of the
-- mockchain, that is without any tweaks nor branching.
type MockChainDirect a =
  Sem
    '[ MockChainWrite,
       MockChainRead,
       MockChainMisc,
       Fail
     ]
    a

runMockChainDirect :: MockChainDirect a -> (MockChainBook, (MockChainState, Either MockChainError a))
runMockChainDirect =
  run
    . runWriter
    . runMockChainLog
    . runState def
    . runError
    . runToCardanoError
    . runFailInMockChainError
    . runMockChainMisc
    . runMockChainRead
    . runMockChainWrite
    . insertAt @4 @[Error Ledger.ToCardanoError, Error MockChainError, State MockChainState, MockChainLog, Writer MockChainBook]

-- * MockChainFull

type TweakStack = '[MockChainRead, Fail, NonDet]

-- | A possible stack of effects to handle staged interpretation of the
-- mockchain, that is with tweaks and branching.
type MockChainFull a =
  Sem
    [ ModifyOnTime (UntypedTweak TweakStack),
      MockChainWrite,
      MockChainMisc,
      MockChainRead,
      Fail,
      NonDet
    ]
    a

runMockChainFull :: MockChainFull a -> [(MockChainBook, (MockChainState, Either MockChainError a))]
runMockChainFull =
  run
    . runNonDet
    . runWriter
    . runMockChainLog
    . runState def
    . runError
    . runToCardanoError
    . runFailInMockChainError
    . runMockChainRead
    . runMockChainMisc
    . evalState []
    . runModifyLocally
    . runMockChainWrite
    . insertAt @6 @[Error Ledger.ToCardanoError, Error MockChainError, State MockChainState, MockChainLog, Writer MockChainBook]
    . interceptMockChainWriteWithTweak
    . runModifyOnTime
    . insertAt @2 @[ModifyLocally (UntypedTweak TweakStack), State [Ltl (UntypedTweak TweakStack)]]
