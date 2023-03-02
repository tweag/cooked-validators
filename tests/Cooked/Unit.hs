module Cooked.Unit (tests) where

import qualified Cooked.Unit.AttackSpec as Attack
import qualified Cooked.Unit.LtlSpec as Ltl
import qualified Cooked.Unit.MinAdaSpec as MinAda
import qualified Cooked.Unit.MockChainSpec as MockChain
import qualified Cooked.Unit.TweakSpec as Tweak
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Unit tests"
    [Attack.tests, Ltl.tests, MinAda.tests, MockChain.tests, Tweak.tests]
