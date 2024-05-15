module Cooked.TweakSpec (tests) where

import Cooked.Tweak.CommonSpec qualified as CommonSpec
import Cooked.Tweak.OutPermutationsSpec qualified as OutPermutationsSpec
import Cooked.Tweak.TamperDatumSpec qualified as TamperDatumSpec
import Cooked.Tweak.ValidityRangeSpec qualified as ValidityRangeSpec
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Tweaks"
    [ CommonSpec.tests,
      OutPermutationsSpec.tests,
      TamperDatumSpec.tests,
      ValidityRangeSpec.tests
    ]
