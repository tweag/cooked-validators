module Spec.Tweak (tests) where

import Spec.Tweak.Common qualified as Common
import Spec.Tweak.Labels qualified as Labels
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Tweaks"
    [ Common.tests,
      Labels.tests
    ]
