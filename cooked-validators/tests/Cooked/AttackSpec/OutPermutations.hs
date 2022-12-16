{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.AttackSpec.OutPermutations (tests) where

import Cooked.Attack
import Cooked.MockChain
import Cooked.MockChain.Testing
import Cooked.TestUtils
import Cooked.Tx.Constraints.Type
import Data.Default
import qualified Ledger.Ada as Pl
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "output permutation tweak"
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
      testGroup "tests for PermutOutTweakMode" $
        let a = paysPK (walletPKHash $ wallet 1) $ Pl.lovelaceValueOf 123
            b = paysPK (walletPKHash $ wallet 2) $ Pl.lovelaceValueOf 123
            c = paysPK (walletPKHash $ wallet 3) $ Pl.lovelaceValueOf 123
            skel x y z = mempty {txSkelOuts = [x, y, z]}
         in [ testCase "KeepIdentity (Just 2)" $
                assertSameSets
                  (map (,()) [skel a b c, skel b a c])
                  (getTweak (permutOutTweak $ KeepIdentity $ Just 2) def $ skel a b c),
              testCase "KeepIdentity Nothing" $
                assertSameSets
                  (map (,()) [skel a b c, skel a c b, skel b a c, skel b c a, skel c a b, skel c b a])
                  (getTweak (permutOutTweak $ KeepIdentity Nothing) def $ skel a b c),
              testCase "OmitIdentity (Just 2)" $
                assertSameSets
                  (map (,()) [skel b a c])
                  (getTweak (permutOutTweak $ OmitIdentity $ Just 2) def $ skel a b c),
              testCase "OmitIdentity Nothing" $
                assertSameSets
                  (map (,()) [skel a c b, skel b a c, skel b c a, skel c a b, skel c b a])
                  (getTweak (permutOutTweak $ OmitIdentity Nothing) def $ skel a b c)
            ]
    ]
