module Spec.Attack (tests) where

import Spec.Attack.DatumHijacking qualified as DatumHijacking
import Spec.Attack.DoubleSat qualified as DoubleSat
import Spec.Attack.TamperDatum qualified as TampDat
import Spec.Attack.TamperRedeemer qualified as TampRed
import Spec.Attack.TokenDuplication qualified as DupToken
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Attack DSL"
    [ DupToken.tests,
      DatumHijacking.tests,
      DoubleSat.tests,
      TampDat.tests,
      TampRed.tests
    ]
