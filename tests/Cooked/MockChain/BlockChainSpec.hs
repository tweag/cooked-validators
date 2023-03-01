module Cooked.MockChain.BlockChainSpec (tests) where

import Cooked.MockChain.BlockChain
import Cooked.MockChain.Direct
import qualified Ledger
import qualified Ledger.Slot as Ledger
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "BlockChain"
    [ testGroup
        "time handling"
        [ testProperty "bounds computed by slotToTimeInterval are included in slot" $
            \n ->
              case runMockChain $ do
                (l, r) <- slotToTimeInterval $ Ledger.Slot n
                Ledger.Slot nl <- getEnclosingSlot l
                Ledger.Slot nr <- getEnclosingSlot r
                return (nl, nr) of
                Left _err -> False
                Right ((nl, nr), _) -> nl == n && nr == n,
          testProperty "bounds computed by slotToTimeInterval are maximal" $
            \n ->
              case runMockChain $ do
                (l, r) <- slotToTimeInterval $ Ledger.Slot n
                Ledger.Slot nl <- getEnclosingSlot (l - 1)
                Ledger.Slot nr <- getEnclosingSlot (r + 1)
                return (nl, nr) of
                Left _err -> False
                Right ((nl, nr), _) -> nl == n - 1 && nr == n + 1,
          testProperty "time is always included in enclosing slot" $
            \t -> case runMockChain $ slotToTimeInterval =<< getEnclosingSlot (Ledger.POSIXTime t) of
              Left _err -> False
              Right ((Ledger.POSIXTime a, Ledger.POSIXTime b), _) -> a <= t && a <= b
        ]
    ]
