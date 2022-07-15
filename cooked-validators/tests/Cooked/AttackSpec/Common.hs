{-# LANGUAGE TypeApplications #-}

module Cooked.AttackSpec.Common (tests) where

import Cooked.Attack.Common
import Cooked.MockChain.Testing
import Cooked.TestUtils
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "common for all attacks"
    [ testGroup "tests for SplitStrategy implementations" $
        let example = [(1, [(11, 'a'), (12, 'b')]), (2, [(21, 'c'), (22, 'd'), (23, 'e')]), (3, [])]
            empties =
              [ [],
                [(1, [])],
                [(1, []), (2, [])]
              ]
         in [ testGroup
                "oneChange"
                [ testCase "example is correct" $
                    assertSameSets
                      (oneChange example)
                      [ ([11, 2, 3], "a"),
                        ([12, 2, 3], "b"),
                        ([1, 21, 3], "c"),
                        ([1, 22, 3], "d"),
                        ([1, 23, 3], "e")
                      ],
                  testCase "empties are correct" $
                    testConjoin $ map (assertSameSets @([Integer], String) [] . oneChange) empties
                ],
              testGroup
                "allCombinations"
                [ testCase "example is correct" $
                    assertSameSets
                      (allCombinations example)
                      [ ([11, 2, 3], "a"),
                        ([12, 2, 3], "b"),
                        ([1, 21, 3], "c"),
                        ([1, 22, 3], "d"),
                        ([1, 23, 3], "e"),
                        ([11, 21, 3], "ac"),
                        ([11, 22, 3], "ad"),
                        ([11, 23, 3], "ae"),
                        ([12, 21, 3], "bc"),
                        ([12, 22, 3], "bd"),
                        ([12, 23, 3], "be")
                      ],
                  testCase "empties are correct" $
                    testConjoin $ map (assertSameSets @([Integer], String) [] . allCombinations) empties
                ]
            ]
    ]
