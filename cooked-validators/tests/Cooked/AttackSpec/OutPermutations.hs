{-# LANGUAGE TypeApplications #-}

module Cooked.AttackSpec.OutPermutations (tests) where

import Cooked.Attack.OutPermutations
import Cooked.MockChain.Testing
import Cooked.TestUtils
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "output permutation attack"
    [ testGroup
        "tests for 'distinctPermutations'"
        $ let assertPermutEq actual expected =
                assertSameSets actual expected
                  .&&. (length actual @?= length expected)
           in [ testCase "correct result for []" $
                  distinctPermutations @Integer [] @?= [[]],
                testCase "correct result for [1,2,3]" $
                  distinctPermutations [1, 2, 3]
                    `assertPermutEq` [ [1, 2, 3],
                                       [1, 3, 2],
                                       [2, 1, 3],
                                       [2, 3, 1],
                                       [3, 1, 2],
                                       [3, 2, 1]
                                     ],
                testCase "correct result for [1,1]" $
                  distinctPermutations [1, 1]
                    `assertPermutEq` [[1, 1]],
                testCase "correct result for [1,2,1]" $
                  distinctPermutations [1, 2, 1]
                    `assertPermutEq` [ [1, 2, 1],
                                       [2, 1, 1],
                                       [1, 1, 2]
                                     ],
                testCase "correct result for [2,1,3,1]" $
                  distinctPermutations [2, 1, 3, 1]
                    `assertPermutEq` [ [2, 1, 3, 1],
                                       [1, 1, 2, 3],
                                       [1, 1, 3, 2],
                                       [1, 2, 1, 3],
                                       [1, 3, 1, 2],
                                       [1, 2, 3, 1],
                                       [1, 3, 2, 1],
                                       [2, 1, 1, 3],
                                       [3, 1, 1, 2],
                                       [3, 1, 2, 1],
                                       [2, 3, 1, 1],
                                       [3, 2, 1, 1]
                                     ]
              ]
    ]
