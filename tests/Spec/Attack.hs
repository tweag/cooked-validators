module Spec.Attack (tests) where

import Spec.Attack.DatumHijacking qualified as DatumHijacking
import Spec.Attack.DatumTampering qualified as TampDat
import Spec.Attack.OutputsReordering qualified as Reorder
import Spec.Attack.RedeemerTampering qualified as TampRed
import Spec.Attack.TokenDuplication qualified as DupToken
import Spec.Attack.ValidityTampering qualified as Validity
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Attack DSL"
    [ DatumHijacking.tests,
      DupToken.tests,
      Reorder.tests,
      TampDat.tests,
      TampRed.tests,
      Validity.tests
    ]
