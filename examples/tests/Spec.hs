module Main where

import qualified ForgeSpec
import qualified PMultiSigStatefulSpec
import qualified SplitSpec
import qualified SplitUPLCSpec
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.Ingredients.MetadataReporter
import qualified UseCaseCrowdfundingSpec

main :: IO ()
main = do
  twauditMain $
    testGroup
      "main"
      [ PMultiSigStatefulSpec.tests,
        UseCaseCrowdfundingSpec.tests,
        SplitSpec.tests,
        SplitUPLCSpec.tests,
        ForgeSpec.tests
      ]
