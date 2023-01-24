module Cooked.Tweak.ValidityRangeSpec (tests) where

import Plutus.V1.Ledger.Interval (interval)
import Plutus.V2.Ledger.Api (POSIXTime (POSIXTime))
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  let toTimeRange a b = interval (POSIXTime a) (POSIXTime b)
   in testGroup
        "Validity range tweaks"
        []
