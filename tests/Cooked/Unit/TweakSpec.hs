module Cooked.Unit.TweakSpec (tests) where

import qualified Cooked.Unit.Tweak.CommonSpec as CommonSpec
import qualified Cooked.Unit.Tweak.OutPermutationsSpec as OutPermutationsSpec
import qualified Cooked.Unit.Tweak.ValidityRangeSpec as ValidityRangeSpec
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Tweaks"
    [ CommonSpec.tests,
      OutPermutationsSpec.tests,
      ValidityRangeSpec.tests
    ]
