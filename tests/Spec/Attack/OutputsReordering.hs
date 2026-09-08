module Spec.Attack.OutputsReordering where

import Cooked
import Optics.Core
import Polysemy
import Polysemy.NonDet
import Test.Tasty
import Test.Tasty.HUnit

manyOutputsSkeleton :: TxSkel
manyOutputsSkeleton =
  txSkelEmulatorTemplate
    { txSkelOutputs = (\n -> wallet n `receives` AdaValue 10) <$> [1 .. 5]
    }

runOutputsReordering :: OutputsReorderingParams -> [[Int]]
runOutputsReordering params =
  fmap
    ( toListOf
        ( txSkelOutputsL
            % traversed
            % txSkelOutOwnerL
            % userEitherPubKeyP
            % userPubKeyHashI
            % to walletPKHashToId
            % _Just
        )
    )
    . run
    . runNonDet
    . execTweak manyOutputsSkeleton
    $ outputsReorderingAttack params

tests :: TestTree
tests =
  testGroup
    "Outputs reordering attack"
    [ testCase "Swapping two outputs" $ [[4, 2, 3, 1, 5]] @=? runOutputsReordering (Swap 0 3),
      testCase "Moving an output forward" $ [[1, 2, 4, 3, 5]] @=? runOutputsReordering (Move 2 4),
      testCase "Moving an output backwards" $ [[1, 2, 5, 3, 4]] @=? runOutputsReordering (Move 4 2),
      testCase "Shuffling outputs any way possible" $ ((5 * 4 * 3 * 2) - 1) @=? length (runOutputsReordering Shuffle),
      testCase "Manually reversing the outputs order" $ [[5, 4, 3, 2, 1]] @=? runOutputsReordering (ManualReordering ((: []) . reverse))
    ]
