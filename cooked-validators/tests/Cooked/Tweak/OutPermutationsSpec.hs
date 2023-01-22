{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.Tweak.OutPermutationsSpec (tests) where

import Cooked
import Cooked.TestUtils
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
            skel x y z = def {txSkelOuts = [x, y, z]}
         in [ testCase "KeepIdentity (Just 2)" $
                assertSameSets
                  (map (Right . ((),)) [skel a b c, skel b a c])
                  (runTweak (allOutPermutsTweak $ KeepIdentity $ Just 2) $ skel a b c),
              testCase "KeepIdentity Nothing" $
                assertSameSets
                  (map (Right . ((),)) [skel a b c, skel a c b, skel b a c, skel b c a, skel c a b, skel c b a])
                  (runTweak (allOutPermutsTweak $ KeepIdentity Nothing) $ skel a b c),
              testCase "OmitIdentity (Just 2)" $
                assertSameSets
                  (map (Right . ((),)) [skel b a c])
                  (runTweak (allOutPermutsTweak $ OmitIdentity $ Just 2) $ skel a b c),
              testCase "OmitIdentity Nothing" $
                assertSameSets
                  (map (Right . ((),)) [skel a c b, skel b a c, skel b c a, skel c a b, skel c b a])
                  (runTweak (allOutPermutsTweak $ OmitIdentity Nothing) $ skel a b c)
            ]
    ]
