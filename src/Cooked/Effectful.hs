{-# LANGUAGE TemplateHaskell #-}

module Cooked.Effectful where

import Cardano.Api qualified as Cardano
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Monad (guard, msum, unless)
import Cooked.MockChain.BlockChain (MockChainError (..), MockChainLogEntry)
import Cooked.MockChain.Direct (MockChainBook (..))
import Cooked.MockChain.MockChainState (MockChainState (..), mcstConstitutionL, mcstLedgerStateL)
import Cooked.Pretty.Hashable (ToHash, toHash)
import Cooked.Skeleton (ToVScript, TxSkel, TxSkelOut, VScript)
import Data.Coerce
import Data.Default
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Ledger.Slot qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error (Error (..), mapError, runError, throw)
import Polysemy.Fail (Fail (Fail))
import Polysemy.Internal (Subsume)
import Polysemy.NonDet
import Polysemy.State
import Polysemy.Writer (Writer, runWriter, tell)

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
