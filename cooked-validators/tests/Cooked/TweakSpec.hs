module Cooked.TweakSpec (tests) where

import qualified Cooked.Tweak.CommonSpec as CommonSpec
import qualified Cooked.Tweak.OutPermutationsSpec as OutPermutationsSpec
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Tweaks"
    [ CommonSpec.tests,
      OutPermutationsSpec.tests
    ]
