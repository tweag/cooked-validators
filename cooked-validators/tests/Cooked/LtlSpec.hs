module Cooked.LtlSpec (tests) where

import Cooked.Ltl.IntegerIdentity (integerIdentityTests)
import Cooked.Ltl.IntegerMaybe (integerMaybeTests)

import Test.Tasty (TestTree, testGroup)

tests :: [TestTree]
tests =
  [ testGroup
      "Testing Ltl over integers and Identity monad."
      integerIdentityTests,
    testGroup
      "Testing Ltl over integers and Maybe monad."
      integerMaybeTests
  ]
