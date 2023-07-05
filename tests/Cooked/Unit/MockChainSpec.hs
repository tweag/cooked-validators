module Cooked.Unit.MockChainSpec (tests) where

import qualified Cooked.Unit.MockChain.BlockChainSpec as BlockChainSpec
import Test.Tasty

tests :: TestTree
tests = testGroup "MockChain" [BlockChainSpec.tests]
