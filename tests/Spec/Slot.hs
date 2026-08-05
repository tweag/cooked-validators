module Spec.Slot (tests) where

import Cooked.MockChain.Effect.Read.Chain
import Cooked.MockChain.Runtime.Error
import Cooked.MockChain.Runtime.State
import Data.Default
import Ledger.Slot qualified as P.Ledger
import Ledger.Tx qualified as P.Ledger
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.State
import Test.Tasty
import Test.Tasty.QuickCheck

runSlot ::
  Sem
    '[ MockChainReadChain,
       State MockChainState,
       Fail,
       Error P.Ledger.ToCardanoError,
       Error MockChainError
     ]
    a ->
  Either MockChainError a
runSlot =
  run
    . runError
    . runToCardanoErrorInMockChainError
    . runFailInMockChainError
    . evalState def
    . runMockChainReadChainEmul

tests :: TestTree
tests =
  testGroup
    "time handling"
    [ testProperty "bounds computed by slotToMSRange are included in slot" $
        \n ->
          case runSlot $ do
            (l, r) <- slotToMSRange $ P.Ledger.Slot n
            P.Ledger.Slot nl <- getEnclosingSlot l
            P.Ledger.Slot nr <- getEnclosingSlot r
            return (nl, nr) of
            Left _err -> False
            Right (nl, nr) -> nl == n && nr == n,
      testProperty "bounds computed by slotToMSRange are maximal" $
        \n ->
          case runSlot $ do
            (l, r) <- slotToMSRange $ P.Ledger.Slot n
            P.Ledger.Slot nl <- getEnclosingSlot (l - 1)
            P.Ledger.Slot nr <- getEnclosingSlot (r + 1)
            return (nl, nr) of
            Left _err -> False
            Right (nl, nr) -> nl == n - 1 && nr == n + 1,
      testProperty "time is always included in enclosing slot" $
        \t -> case runSlot $ slotToMSRange =<< getEnclosingSlot (Api.POSIXTime t) of
          Left _err -> False
          Right (Api.POSIXTime a, Api.POSIXTime b) -> a <= t && a <= b
    ]
