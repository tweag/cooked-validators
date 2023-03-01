module Cooked.MockChainSpec (tests) where

import qualified Cooked.MockChain.BlockChainSpec as BlockChainSpec
import Test.Tasty

tests :: TestTree
tests = testGroup "MockChain" [BlockChainSpec.tests]
