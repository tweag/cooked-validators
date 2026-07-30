module Spec.Tweak (tests) where

import Spec.Tweak.Common qualified as Common
import Spec.Tweak.Labels qualified as Labels
import Spec.Tweak.ValidityRange qualified as ValidityRange
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Tweaks"
    [ Common.tests,
      ValidityRange.tests,
      Labels.tests
    ]
