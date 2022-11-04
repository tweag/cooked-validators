module Cooked.AttackSpec (tests) where

import qualified Cooked.AttackSpec.AddConstraints as AddConstraints
import qualified Cooked.AttackSpec.Common as Common
import qualified Cooked.AttackSpec.DatumHijacking as DatumHijacking
import qualified Cooked.AttackSpec.DoubleSat as DoubleSat
import qualified Cooked.AttackSpec.DupToken as DupToken
import qualified Cooked.AttackSpec.OutPermutations as OutPermutations
import Test.Tasty

tests :: [TestTree]
tests =
  [ testGroup
      "Attack DSL"
      [ AddConstraints.tests,
        Common.tests,
        DupToken.tests,
        DatumHijacking.tests,
        DoubleSat.tests,
        OutPermutations.tests
      ]
  ]
