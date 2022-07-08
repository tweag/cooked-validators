module Cooked.AttackSpec (tests) where

import qualified Cooked.AttackSpec.DatumHijackingAttack as DatumHijackingAttack
import qualified Cooked.AttackSpec.DoubleSatAttack as DoubleSatAttack
import qualified Cooked.AttackSpec.DupTokenAttack as DupTokenAttack
import Test.Tasty

tests :: [TestTree]
tests =
  [ testGroup
      "Attack DSL"
      [ DupTokenAttack.tests,
        DatumHijackingAttack.tests,
        DoubleSatAttack.tests
      ]
  ]
