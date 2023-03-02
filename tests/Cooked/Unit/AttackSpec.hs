module Cooked.Unit.AttackSpec (tests) where

import qualified Cooked.Unit.Attack.DatumHijackingSpec as DatumHijacking
import qualified Cooked.Unit.Attack.DoubleSatSpec as DoubleSat
import qualified Cooked.Unit.Attack.DupTokenSpec as DupToken
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Attack DSL"
    [ DupToken.tests,
      DatumHijacking.tests,
      DoubleSat.tests
    ]
