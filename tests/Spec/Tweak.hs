module Spec.Tweak (tests) where

import Spec.Tweak.Common qualified as Common
import Spec.Tweak.Datums qualified as Datums
import Spec.Tweak.Labels qualified as Labels
import Spec.Tweak.OutPermutations qualified as OutPermutations
import Spec.Tweak.Redeemers qualified as Redeemers
import Spec.Tweak.ValidityRange qualified as ValidityRange
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Tweaks"
    [ Common.tests,
      OutPermutations.tests,
      Redeemers.tests,
      Datums.tests,
      ValidityRange.tests,
      Labels.tests
    ]
