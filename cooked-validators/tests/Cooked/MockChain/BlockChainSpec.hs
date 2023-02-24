module Cooked.MockChain.BlockChainSpec (tests) where

import Cooked.MockChain.BlockChain
import Cooked.MockChain.Direct
import qualified Ledger
import qualified Ledger.Slot as Ledger
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "BlockChain"
    [ testGroup
        "time handling"
        [ testCase "borders computed by slotToTimeInterval are included in slot" $
            assertBool "...they're not" $
              case runMockChain $ do
                (l, r) <- slotToTimeInterval 42
                Ledger.Slot nl <- getEnclosingSlot l
                Ledger.Slot nr <- getEnclosingSlot r
                return (nl, nr) of
                Left _err -> False
                Right ((nl, nr), _) -> nl == 42 && nr == 42,
          testProperty "time is always included in enclosing slot" $
            \t -> case runMockChain $ slotToTimeInterval =<< getEnclosingSlot (Ledger.POSIXTime t) of
              Left _err -> False
              Right ((Ledger.POSIXTime a, Ledger.POSIXTime b), _) -> a <= t && a <= b
        ]
    ]
