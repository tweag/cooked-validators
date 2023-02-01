module Cooked.AttackSpec (tests) where

import qualified Cooked.Attack.DatumHijackingSpec as DatumHijacking
import qualified Cooked.Attack.DoubleSatSpec as DoubleSat
import qualified Cooked.Attack.DupTokenSpec as DupToken
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Attack DSL"
    [ DupToken.tests,
      DatumHijacking.tests,
      DoubleSat.tests
    ]
