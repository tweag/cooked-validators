module Main where

import qualified AuctionSpec
import qualified CrowdfundingSpec
import qualified PMultiSigStatefulSpec
import qualified SplitSpec
import qualified SplitUPLCSpec
import Test.Tasty
import qualified UseCaseCrowdfundingSpec

main :: IO ()
main = do
  defaultMain $
    testGroup
      "main"
      [ PMultiSigStatefulSpec.tests,
        UseCaseCrowdfundingSpec.tests,
        SplitSpec.tests,
        SplitUPLCSpec.tests,
        AuctionSpec.tests,
        CrowdfundingSpec.tests
      ]
