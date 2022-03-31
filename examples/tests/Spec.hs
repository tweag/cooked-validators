module Main where

import qualified CrowdfundingSpec
import qualified ForgeSpec
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
      [ CrowdfundingSpec.tests,
        PMultiSigStatefulSpec.tests,
        UseCaseCrowdfundingSpec.tests,
        SplitSpec.tests,
        SplitUPLCSpec.tests,
        ForgeSpec.tests
      ]
