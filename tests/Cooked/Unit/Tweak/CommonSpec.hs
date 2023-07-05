module Cooked.Unit.Tweak.CommonSpec (tests) where

import Cooked
import Cooked.MockChain.Staged
import Cooked.TestUtils
import Data.Default
import Data.List
import Optics.Core
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Plutus.Script.Utils.Value as Pl
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "building blocks for tweaks"
    [ testGroup "overMaybeSelectingTweak" $
        let skel =
              txSkelTemplate
                { txSkelOuts =
                    [ paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 123),
                      paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 234),
                      paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 345)
                    ]
                }
         in [ testCase "return empty list and don't change anything if no applicable modifications" $ -- this one is a regression test
                [Right ([], skel)]
                  @=? runTweak
                    ( overMaybeSelectingTweak
                        (txSkelOutsL % traversed % txSkelOutValueL)
                        (const Nothing)
                        (const True)
                    )
                    skel,
              testCase "select applied modification by index" $
                [ Right
                    ( [Pl.lovelaceValueOf 345],
                      txSkelTemplate
                        { txSkelOuts =
                            [ paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 123),
                              paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 234),
                              paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 789)
                            ]
                        }
                    )
                ]
                  @=? runTweak
                    ( overMaybeSelectingTweak
                        (txSkelOutsL % traversed % txSkelOutValueL)
                        ( \value ->
                            if value `Pl.geq` Pl.lovelaceValueOf 200
                              then Just $ Pl.lovelaceValueOf 789
                              else Nothing
                        )
                        (== 1)
                    )
                    skel,
              testCase "return unmodified foci in the right order" $
                [ Right
                    ( [ Pl.lovelaceValueOf 123,
                        Pl.lovelaceValueOf 345
                      ],
                      txSkelTemplate
                        { txSkelOuts =
                            [ paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 789),
                              paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 234),
                              paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 789)
                            ]
                        }
                    )
                ]
                  @=? runTweak
                    ( overMaybeSelectingTweak
                        (txSkelOutsL % traversed % txSkelOutValueL)
                        (const $ Just $ Pl.lovelaceValueOf 789)
                        (`elem` [0, 2])
                    )
                    skel
            ],
      testGroup "combineModsTweak" $
        let skel x y z =
              txSkelTemplate
                { txSkelOuts =
                    [ paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf x),
                      paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf y),
                      paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf z)
                    ]
                }
            skelIn = skel 0 0 0
            skelOut x y z = Right ([0 | x /= 0] ++ [1 | y /= 0] ++ [2 | z /= 0], skel x y z)
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
                  ( runTweak
                      ( combineModsTweak
                          (tail . subsequences)
                          (txSkelOutsL % itraversed % txSkelOutValueL % adaL)
                          (\i x -> return [(x + 1, i), (x + 2, i)])
                      )
                      skelIn
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
                  ( runTweak
                      ( combineModsTweak
                          (map (: []))
                          (txSkelOutsL % itraversed % txSkelOutValueL % adaL)
                          (\i x -> return [(x + 1, i), (x + 2, i)])
                      )
                      skelIn
                  )
            ]
    ]
