module Cooked.SlotSpec (tests) where

import Cooked.MockChain.BlockChain
import Cooked.MockChain.Direct
import Ledger.Slot qualified as Ledger
import PlutusLedgerApi.V3 qualified as Api
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "time handling"
    [ testProperty "bounds computed by slotToTimeInterval are included in slot" $
        \n ->
          case fst $ runMockChain $ do
            (l, r) <- slotToTimeInterval $ Ledger.Slot n
            Ledger.Slot nl <- getEnclosingSlot l
            Ledger.Slot nr <- getEnclosingSlot r
            return (nl, nr) of
            Left _err -> False
            Right ((nl, nr), _) -> nl == n && nr == n,
      testProperty "bounds computed by slotToTimeInterval are maximal" $
        \n ->
          case fst $ runMockChain $ do
            (l, r) <- slotToTimeInterval $ Ledger.Slot n
            Ledger.Slot nl <- getEnclosingSlot (l - 1)
            Ledger.Slot nr <- getEnclosingSlot (r + 1)
            return (nl, nr) of
            Left _err -> False
            Right ((nl, nr), _) -> nl == n - 1 && nr == n + 1,
      testProperty "time is always included in enclosing slot" $
        \t -> case fst $ runMockChain $ slotToTimeInterval =<< getEnclosingSlot (Api.POSIXTime t) of
          Left _err -> False
          Right ((Api.POSIXTime a, Api.POSIXTime b), _) -> a <= t && a <= b
    ]
