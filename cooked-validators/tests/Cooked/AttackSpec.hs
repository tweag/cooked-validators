module Cooked.AttackSpec (tests) where

-- import qualified Cooked.AttackSpec.AddConstraints as AddConstraints
import qualified Cooked.Attack.Tweak.CommonSpec as Common
-- import qualified Cooked.AttackSpec.DatumHijacking as DatumHijacking
-- import qualified Cooked.AttackSpec.DoubleSat as DoubleSat
-- import qualified Cooked.AttackSpec.DupToken as DupToken
import qualified Cooked.Attack.Tweak.OutPermutationsSpec as OutPermutations
import Test.Tasty

tests :: [TestTree]
tests =
  [ testGroup
      "Attack DSL"
      [ -- AddConstraints.tests,
        Common.tests,
        -- DupToken.tests,
        -- DatumHijacking.tests,
        -- DoubleSat.tests,
        OutPermutations.tests
      ]
  ]
