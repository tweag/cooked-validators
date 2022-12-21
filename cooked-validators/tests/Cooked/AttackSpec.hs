module Cooked.AttackSpec (tests) where

import qualified Cooked.Attack.DatumHijackingSpec as DatumHijacking
import qualified Cooked.Attack.DoubleSatSpec as DoubleSat
import qualified Cooked.Attack.DupTokenSpec as DupToken
import qualified Cooked.Attack.Tweak.CommonSpec as Common
import qualified Cooked.Attack.Tweak.OutPermutationsSpec as OutPermutations
import Test.Tasty

tests :: [TestTree]
tests =
  [ testGroup
      "Attack DSL"
      [ Common.tests,
        DupToken.tests,
        DatumHijacking.tests,
        DoubleSat.tests,
        OutPermutations.tests
      ]
  ]
