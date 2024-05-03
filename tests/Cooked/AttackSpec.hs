module Cooked.AttackSpec (tests) where

import Cooked.Attack.DatumHijackingSpec qualified as DatumHijacking
import Cooked.Attack.DoubleSatSpec qualified as DoubleSat
import Cooked.Attack.DupTokenSpec qualified as DupToken
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Attack DSL"
    [ DupToken.tests,
      DatumHijacking.tests,
      DoubleSat.tests
    ]
