module Spec.Tweak.Common (tests) where

import Cooked
import Data.List (subsequences)
import Optics.Core
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import Polysemy
import Polysemy.NonDet
import Test.Tasty
import Test.Tasty.HUnit

alice :: Wallet
alice = wallet 1

mkSkel :: [Integer] -> TxSkel
mkSkel l = set txSkelOutsL (receives alice . Value . Script.lovelace <$> l) txSkelTemplate

tests :: TestTree
tests =
  testGroup
    "building blocks for tweaks"
    [ testGroup "overMaybeSelectingTweak" $
        let skel = mkSkel [123, 234, 345]
         in [ testCase "return empty list and don't change anything if no applicable modifications" $ -- this one is a regression test -- this one is a regression test
        -- this one is a regression test
                [skel]
                  @=? run
                    ( runNonDet $
                        execTweak skel $
                          overMaybeSelectingTweak
                            (txSkelOutsL % traversed % txSkelOutValueL)
                            (const Nothing)
                            (const True)
                    ),
              testCase "select applied modification by index" $
                [(mkSkel [123, 234, 789], [Script.lovelace 345])]
                  @=? run
                    ( runNonDet $
                        runTweak skel $
                          overMaybeSelectingTweak
                            (txSkelOutsL % traversed % txSkelOutValueL)
                            ( \value ->
                                if value `Api.geq` Script.lovelace 200
                                  then Just $ Script.lovelace 789
                                  else Nothing
                            )
                            (== 1)
                    ),
              testCase "return unmodified foci in the right order" $
                [(mkSkel [789, 234, 789], [Script.lovelace 123, Script.lovelace 345])]
                  @=? run
                    ( runNonDet $
                        runTweak skel $
                          overMaybeSelectingTweak
                            (txSkelOutsL % traversed % txSkelOutValueL)
                            (const $ Just $ Script.lovelace 789)
                            (`elem` [0, 2])
                    )
            ],
      testGroup "combineModsTweak" $
        let skelIn = mkSkel [0, 0, 0]
            skelOut x y z = (mkSkel [x, y, z], [0 | x /= 0] ++ [1 | y /= 0] ++ [2 | z /= 0])
         in [ testCase "all combinations of modifications" $
                assertSameSets
                  [ -- one changed focus
                    skelOut 1 0 0,
                    skelOut 2 0 0,
                    skelOut 0 1 0,
                    skelOut 0 2 0,
                    skelOut 0 0 1,
                    skelOut 0 0 2,
                    -- two changed foci
                    skelOut 1 1 0,
                    skelOut 1 2 0,
                    skelOut 2 1 0,
                    skelOut 2 2 0,
                    skelOut 1 0 1,
                    skelOut 1 0 2,
                    skelOut 2 0 1,
                    skelOut 2 0 2,
                    skelOut 0 1 1,
                    skelOut 0 1 2,
                    skelOut 0 2 1,
                    skelOut 0 2 2,
                    -- three changed foci
                    skelOut 1 1 1,
                    skelOut 1 1 2,
                    skelOut 1 2 1,
                    skelOut 1 2 2,
                    skelOut 2 1 1,
                    skelOut 2 1 2,
                    skelOut 2 2 1,
                    skelOut 2 2 2
                  ]
                  ( run $
                      runNonDet $
                        runTweak skelIn $
                          combineModsTweak
                            (tail . subsequences)
                            (txSkelOutsL % itraversed % txSkelOutValueL % valueLovelaceL)
                            (\i x -> return [(x + 1, i), (x + 2, i)])
                  ),
              testCase "separate modifications" $
                assertSameSets
                  [ -- one changed focus
                    skelOut 1 0 0,
                    skelOut 2 0 0,
                    skelOut 0 1 0,
                    skelOut 0 2 0,
                    skelOut 0 0 1,
                    skelOut 0 0 2
                  ]
                  ( run $
                      runNonDet $
                        runTweak skelIn $
                          combineModsTweak
                            (map (: []))
                            (txSkelOutsL % itraversed % txSkelOutValueL % valueLovelaceL)
                            (\i x -> return [(x + 1, i), (x + 2, i)])
                  )
            ]
    ]
