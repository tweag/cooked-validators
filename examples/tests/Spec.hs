module Main where

import qualified ForgeSpec
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
        ForgeSpec.tests
      ]
