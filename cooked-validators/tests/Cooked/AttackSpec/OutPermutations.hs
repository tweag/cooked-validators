{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.AttackSpec.OutPermutations (tests) where

import Cooked.Attack.Common
import Cooked.Attack.OutPermutations
import Cooked.MockChain
import Cooked.MockChain.Testing
import Cooked.TestUtils
import Cooked.Tx.Constraints
import Data.Default
import qualified Plutus.V1.Ledger.Ada as L
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "output permutation attack"
    [ testCase
        "tests for 'distinctPermutations'"
        $ let assertPermutEq actual expected =
                assertSameSets actual expected
                  .&&. (length actual @?= length expected)
           in testConjoin $
                map
                  ( \(input, expected) ->
                      assertPermutEq (distinctPermutations @Int input) expected
                  )
                  [ ( [],
                      [[]]
                    ),
                    ( [1, 2, 3],
                      [ [1, 2, 3],
                        [1, 3, 2],
                        [2, 1, 3],
                        [2, 3, 1],
                        [3, 1, 2],
                        [3, 2, 1]
                      ]
                    ),
                    ( [1, 1],
                      [[1, 1]]
                    ),
                    ( [1, 2, 1],
                      [ [2, 1, 1],
                        [1, 2, 1],
                        [1, 1, 2]
                      ]
                    ),
                    ( [2, 1, 3, 1],
                      [ [1, 1, 2, 3],
                        [1, 1, 3, 2],
                        [1, 2, 1, 3],
                        [1, 3, 1, 2],
                        [1, 2, 3, 1],
                        [1, 3, 2, 1],
                        [2, 1, 1, 3],
                        [3, 1, 1, 2],
                        [2, 1, 3, 1],
                        [3, 1, 2, 1],
                        [2, 3, 1, 1],
                        [3, 2, 1, 1]
                      ]
                    )
                  ],
      testGroup "tests for PermutOutAttackMode" $
        let a = paysPK (walletPKHash $ wallet 1) $ L.lovelaceValueOf 123
            b = paysPK (walletPKHash $ wallet 2) $ L.lovelaceValueOf 123
            c = paysPK (walletPKHash $ wallet 3) $ L.lovelaceValueOf 123
            skel x y z = txSkel ([] :=>: [x, y, z])
         in [ testCase "KeepIdentity (Just 2)" $
                assertSameSets
                  (map (,()) [skel a b c, skel b a c])
                  (getAttack (permutOutAttack $ KeepIdentity $ Just 2) def $ skel a b c),
              testCase "KeepIdentity Nothing" $
                assertSameSets
                  (map (,()) [skel a b c, skel a c b, skel b a c, skel b c a, skel c a b, skel c b a])
                  (getAttack (permutOutAttack $ KeepIdentity Nothing) def $ skel a b c),
              testCase "OmitIdentity (Just 2)" $
                assertSameSets
                  (map (,()) [skel b a c])
                  (getAttack (permutOutAttack $ OmitIdentity $ Just 2) def $ skel a b c),
              testCase "OmitIdentity Nothing" $
                assertSameSets
                  (map (,()) [skel a c b, skel b a c, skel b c a, skel c a b, skel c b a])
                  (getAttack (permutOutAttack $ OmitIdentity Nothing) def $ skel a b c)
            ]
    ]
