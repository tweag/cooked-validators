module Main where

import qualified AuctionSpec
import qualified PMultiSigStatefulSpec
import qualified SplitSpec
import qualified SplitUPLCSpec
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
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
        AuctionSpec.tests
      ]
