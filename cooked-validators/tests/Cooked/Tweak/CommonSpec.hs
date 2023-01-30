module Cooked.Tweak.CommonSpec (tests) where

import Cooked
import Data.Default
import qualified Ledger.Ada as L
import qualified Ledger.Value as L
import Optics.Core
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
                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 123),
                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 234),
                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 345)
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
                    ( [L.lovelaceValueOf 345],
                      txSkelTemplate
                        { txSkelOuts =
                            [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 123),
                              paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 234),
                              paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789)
                            ]
                        }
                    )
                ]
                  @=? runTweak
                    ( overMaybeSelectingTweak
                        (txSkelOutsL % traversed % txSkelOutValueL)
                        ( \value ->
                            if value `L.geq` L.lovelaceValueOf 200
                              then Just $ L.lovelaceValueOf 789
                              else Nothing
                        )
                        (== 1)
                    )
                    skel,
              testCase "return unmodified foci in the right order" $
                [ Right
                    ( [ L.lovelaceValueOf 123,
                        L.lovelaceValueOf 345
                      ],
                      txSkelTemplate
                        { txSkelOuts =
                            [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789),
                              paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 234),
                              paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789)
                            ]
                        }
                    )
                ]
                  @=? runTweak
                    ( overMaybeSelectingTweak
                        (txSkelOutsL % traversed % txSkelOutValueL)
                        (const $ Just $ L.lovelaceValueOf 789)
                        (`elem` [0, 2])
                    )
                    skel
            ]
    ]
