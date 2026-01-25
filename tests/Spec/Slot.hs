module Spec.Slot (tests) where

import Cooked.MockChain.Error
import Cooked.MockChain.MockChainState
import Cooked.MockChain.Read
import Data.Default
import Ledger.Slot qualified as Ledger
import Ledger.Tx qualified as Ledger
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.State
import Test.Tasty
import Test.Tasty.QuickCheck

runSlot ::
  Sem
    '[ MockChainRead,
       State MockChainState,
       Fail,
       Error Ledger.ToCardanoError,
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
    . runMockChainRead

tests :: TestTree
tests =
  testGroup
    "time handling"
    [ testProperty "bounds computed by slotToMSRange are included in slot" $
        \n ->
          case runSlot $ do
            (l, r) <- slotToMSRange $ Ledger.Slot n
            Ledger.Slot nl <- getEnclosingSlot l
            Ledger.Slot nr <- getEnclosingSlot r
            return (nl, nr) of
            Left _err -> False
            Right (nl, nr) -> nl == n && nr == n,
      testProperty "bounds computed by slotToMSRange are maximal" $
        \n ->
          case runSlot $ do
            (l, r) <- slotToMSRange $ Ledger.Slot n
            Ledger.Slot nl <- getEnclosingSlot (l - 1)
            Ledger.Slot nr <- getEnclosingSlot (r + 1)
            return (nl, nr) of
            Left _err -> False
            Right (nl, nr) -> nl == n - 1 && nr == n + 1,
      testProperty "time is always included in enclosing slot" $
        \t -> case runSlot $ slotToMSRange =<< getEnclosingSlot (Api.POSIXTime t) of
          Left _err -> False
          Right (Api.POSIXTime a, Api.POSIXTime b) -> a <= t && a <= b
    ]
