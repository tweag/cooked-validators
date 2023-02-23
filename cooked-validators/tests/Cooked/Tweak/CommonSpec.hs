module Cooked.Tweak.CommonSpec (tests) where

import Cooked
import Cooked.MockChain.Staged
import Data.Default
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
            ]
    ]
