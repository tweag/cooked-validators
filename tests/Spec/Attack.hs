module Spec.Attack (tests) where

import Spec.Attack.DatumHijacking qualified as DatumHijacking
import Spec.Attack.DoubleSat qualified as DoubleSat
import Spec.Attack.DupToken qualified as DupToken
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Attack DSL"
    [ DupToken.tests,
      DatumHijacking.tests,
      DoubleSat.tests
    ]
