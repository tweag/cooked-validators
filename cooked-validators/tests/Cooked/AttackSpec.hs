module Cooked.AttackSpec (tests) where

import qualified Cooked.AttackSpec.DatumHijacking as DatumHijacking
import qualified Cooked.AttackSpec.DoubleSat as DoubleSat
import qualified Cooked.AttackSpec.DupToken as DupToken
import Test.Tasty

tests :: [TestTree]
tests =
  [ testGroup
      "Attack DSL"
      [ DupToken.tests,
        DatumHijacking.tests,
        DoubleSat.tests
      ]
  ]
