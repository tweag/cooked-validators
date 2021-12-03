module Main where

import qualified ForgeSpec
import qualified MarketMakerSpec
import qualified PMultiSigSpec
import qualified PMultiSigStatefulSpec
import qualified SplitSpec
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.Ingredients.MetadataReporter
import qualified UseCaseCrowdfundingSpec
import qualified UseCaseMultisigSpec

main :: IO ()
main = do
  spec <- testSpec "Legacy Hspec suite" legacySpec
  twauditMain $
    testGroup
      "main"
      [ spec,
        PMultiSigStatefulSpec.tests
      ]

legacySpec :: Spec
legacySpec = do
  describe "'Split' contract" SplitSpec.spec
  describe "'PMultiSig' contract" PMultiSigSpec.spec
  describe "'Forge' contract" ForgeSpec.spec
  describe "'plutus-use-cases/Crowdfunding' contract" UseCaseCrowdfundingSpec.spec
  describe "'plutus-use-cases/Multisig' contract" UseCaseMultisigSpec.spec
  describe "'MarketMaker' contract" MarketMakerSpec.spec
