module Cooked.MockChainSpec (tests) where

import Cooked.MockChain.BlockChainSpec qualified as BlockChainSpec
import Test.Tasty

tests :: TestTree
tests = testGroup "MockChain" [BlockChainSpec.tests]
