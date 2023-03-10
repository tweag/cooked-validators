module Cooked.Behaviour (tests) where

import qualified Cooked.Behaviour.Elementary as Elementary
import qualified Cooked.Behaviour.InlineDatumsSpec as InlineDatum
import qualified Cooked.Behaviour.ReferenceInputsSpec as ReferenceInput
import qualified Cooked.Behaviour.ReferenceScriptsSpec as ReferenceScripts
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Behaviour testing"
    [ InlineDatum.tests,
      ReferenceInput.tests,
      ReferenceScripts.tests,
      Elementary.tests
    ]
