module Cooked.AttackSpec.Common (tests) where

import Cooked.Attack.Common
import Cooked.MockChain
import Cooked.MockChain.Testing
import Cooked.TestUtils
import Cooked.Tx.Constraints.Optics
import Cooked.Tx.Constraints.Type
import Data.Default
import qualified Ledger.Value as L
import Optics.Core
import qualified Plutus.V1.Ledger.Ada as L
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "building blocks for attacks"
    [ testGroup "mkSelectAttack" $
        let skel =
              txSkel
                [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 123),
                  paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 234),
                  paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 345)
                ]
         in [ testCase "fail if no applicable modifications" $ -- this one is a regression test
                []
                  @=? getAttack
                    ( mkSelectAttack
                        (paysPKWithDatumConstraintsT % valueL)
                        (\_mcst _value -> Nothing)
                        (const True)
                    )
                    def
                    skel,
              testCase "select applied modification by index" $
                [ ( txSkel
                      [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 123),
                        paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 234),
                        paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789)
                      ],
                    [L.lovelaceValueOf 345]
                  )
                ]
                  @=? getAttack
                    ( mkSelectAttack
                        (paysPKWithDatumConstraintsT % valueL)
                        ( \_mcst value ->
                            if value `L.geq` L.lovelaceValueOf 200
                              then Just $ L.lovelaceValueOf 789
                              else Nothing
                        )
                        (== 1)
                    )
                    def
                    skel,
              testCase "return unmodified foci in the right order" $
                [ ( txSkel
                      [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789),
                        paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 234),
                        paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789)
                      ],
                    [ L.lovelaceValueOf 123,
                      L.lovelaceValueOf 345
                    ]
                  )
                ]
                  @=? getAttack
                    ( mkSelectAttack
                        (paysPKWithDatumConstraintsT % valueL)
                        (\_mcst _value -> Just $ L.lovelaceValueOf 789)
                        (`elem` [0, 2])
                    )
                    def
                    skel
            ]
    ]

-- [ testGroup "tests for SplitStrategy implementations" $
--     let example = [(1, [(11, 'a'), (12, 'b')]), (2, [(21, 'c'), (22, 'd'), (23, 'e')]), (3, [])]
--         empties =
--           [ [],
--             [(1, [])],
--             [(1, []), (2, [])]
--           ]
--      in [ testGroup
--             "oneChange"
--             [ testCase "example is correct" $
--                 assertSameSets
--                   (oneChange example)
--                   [ ([11, 2, 3], "a"),
--                     ([12, 2, 3], "b"),
--                     ([1, 21, 3], "c"),
--                     ([1, 22, 3], "d"),
--                     ([1, 23, 3], "e")
--                   ],
--               testCase "empties are correct" $
--                 testConjoin $ map (assertSameSets @([Integer], String) [] . oneChange) empties
--             ],
--           testGroup
--             "allCombinations"
--             [ testCase "example is correct" $
--                 assertSameSets
--                   (allCombinations example)
--                   [ ([11, 2, 3], "a"),
--                     ([12, 2, 3], "b"),
--                     ([1, 21, 3], "c"),
--                     ([1, 22, 3], "d"),
--                     ([1, 23, 3], "e"),
--                     ([11, 21, 3], "ac"),
--                     ([11, 22, 3], "ad"),
--                     ([11, 23, 3], "ae"),
--                     ([12, 21, 3], "bc"),
--                     ([12, 22, 3], "bd"),
--                     ([12, 23, 3], "be")
--                   ],
--               testCase "empties are correct" $
--                 testConjoin $ map (assertSameSets @([Integer], String) [] . allCombinations) empties
--             ]
--         ],
--   testGroup "unit tests for mkSplittingAttack" $
--     let f :: MockChainSt -> PaysPKWithDatumConstraint -> [(PaysPKWithDatumConstraint, Char)]
--         f _ c@(PaysPKWithDatumConstraint h _ _ x) =
--           if h == walletPKHash (wallet 1)
--             then
--               [ ( PaysPKWithDatumConstraint @()
--                     (walletPKHash $ wallet 1)
--                     Nothing
--                     Nothing
--                     (L.lovelaceValueOf 987654321),
--                   '0'
--                 )
--               ]
--             else
--               [ ( PaysPKWithDatumConstraint @()
--                     (walletPKHash $ wallet 1)
--                     Nothing
--                     Nothing
--                     x,
--                   '1'
--                 ),
--                 ( PaysPKWithDatumConstraint @()
--                     (walletPKHash $ wallet 2)
--                     Nothing
--                     Nothing
--                     (L.lovelaceValueOf 123456789),
--                   '2'
--                 )
--               ]

--         g0 :: String -> TxSkel -> [TxSkel]
--         g0 _ = (: [])

--         g1 :: String -> TxSkel -> [TxSkel]
--         g1 str =
--           if '2' `elem` str
--             then const []
--             else (: [])

--         skel =
--           txSkel
--             [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 123),
--               paysPK (walletPKHash $ wallet 2) (L.lovelaceValueOf 456),
--               paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789)
--             ]
--      in [ testCase "OneChange returns correct 'TxSkel's" $
--             mkSplittingAttack OneChange paysPKWithDatumConstraintsT f g0 def skel
--               `assertSameSets` [ txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321),
--                                      paysPK (walletPKHash $ wallet 2) (L.lovelaceValueOf 456),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789)
--                                    ],
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 123),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 456),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789)
--                                    ],
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 123),
--                                      paysPK (walletPKHash $ wallet 2) (L.lovelaceValueOf 123456789),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789)
--                                    ],
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 123),
--                                      paysPK (walletPKHash $ wallet 2) (L.lovelaceValueOf 456),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321)
--                                    ]
--                                ],
--           testCase "AllCombinations returns correct 'TxSkel's" $
--             mkSplittingAttack AllCombinations paysPKWithDatumConstraintsT f g0 def skel
--               `assertSameSets` [ -- one change
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321),
--                                      paysPK (walletPKHash $ wallet 2) (L.lovelaceValueOf 456),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789)
--                                    ],
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 123),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 456),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789)
--                                    ],
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 123),
--                                      paysPK (walletPKHash $ wallet 2) (L.lovelaceValueOf 123456789),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789)
--                                    ],
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 123),
--                                      paysPK (walletPKHash $ wallet 2) (L.lovelaceValueOf 456),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321)
--                                    ],
--                                  -- two changes
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 456),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789)
--                                    ],
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321),
--                                      paysPK (walletPKHash $ wallet 2) (L.lovelaceValueOf 123456789),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789)
--                                    ],
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321),
--                                      paysPK (walletPKHash $ wallet 2) (L.lovelaceValueOf 456),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321)
--                                    ],
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 123),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 456),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321)
--                                    ],
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 123),
--                                      paysPK (walletPKHash $ wallet 2) (L.lovelaceValueOf 123456789),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321)
--                                    ],
--                                  -- three changes
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321),
--                                      paysPK (walletPKHash $ wallet 2) (L.lovelaceValueOf 123456789),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321)
--                                    ],
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 456),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321)
--                                    ]
--                                ],
--           testCase "filtering works with OneChange" $
--             mkSplittingAttack OneChange paysPKWithDatumConstraintsT f g1 def skel
--               `assertSameSets` [ txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321),
--                                      paysPK (walletPKHash $ wallet 2) (L.lovelaceValueOf 456),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789)
--                                    ],
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 123),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 456),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789)
--                                    ],
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 123),
--                                      paysPK (walletPKHash $ wallet 2) (L.lovelaceValueOf 456),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321)
--                                    ]
--                                ],
--           testCase "filtering works with AllCombinations" $
--             mkSplittingAttack AllCombinations paysPKWithDatumConstraintsT f g1 def skel
--               `assertSameSets` [ -- one change
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321),
--                                      paysPK (walletPKHash $ wallet 2) (L.lovelaceValueOf 456),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789)
--                                    ],
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 123),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 456),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789)
--                                    ],
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 123),
--                                      paysPK (walletPKHash $ wallet 2) (L.lovelaceValueOf 456),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321)
--                                    ],
--                                  -- two changes
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 456),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789)
--                                    ],
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321),
--                                      paysPK (walletPKHash $ wallet 2) (L.lovelaceValueOf 456),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321)
--                                    ],
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 123),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 456),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321)
--                                    ],
--                                  -- three changes
--                                  txSkel
--                                    [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 456),
--                                      paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 987654321)
--                                    ]
--                                ]
--         ]
-- ]
