module Cooked.Tweak.CommonSpec (tests) where

import Cooked
import Data.List
import Optics.Core
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import Test.Tasty
import Test.Tasty.HUnit

alice :: Wallet
alice = wallet 1

mkSkel :: [Integer] -> TxSkel
mkSkel l = set txSkelOutsL (paysPK alice . Script.lovelaceValueOf <$> l) txSkelTemplate

tests :: TestTree
tests =
  testGroup
    "building blocks for tweaks"
    [ testGroup "overMaybeSelectingTweak" $
        let skel = mkSkel [123, 234, 345]
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
                [Right ([Script.lovelaceValueOf 345], mkSkel [123, 234, 789])]
                  @=? runTweak
                    ( overMaybeSelectingTweak
                        (txSkelOutsL % traversed % txSkelOutValueL)
                        ( \value ->
                            if value `Script.geq` Script.lovelaceValueOf 200
                              then Just $ Script.lovelaceValueOf 789
                              else Nothing
                        )
                        (== 1)
                    )
                    skel,
              testCase "return unmodified foci in the right order" $
                [Right ([Script.lovelaceValueOf 123, Script.lovelaceValueOf 345], mkSkel [789, 234, 789])]
                  @=? runTweak
                    ( overMaybeSelectingTweak
                        (txSkelOutsL % traversed % txSkelOutValueL)
                        (const $ Just $ Script.lovelaceValueOf 789)
                        (`elem` [0, 2])
                    )
                    skel
            ],
      testGroup "combineModsTweak" $
        let skelIn = mkSkel [0, 0, 0]
            skelOut x y z = Right ([0 | x /= 0] ++ [1 | y /= 0] ++ [2 | z /= 0], mkSkel [x, y, z])
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
