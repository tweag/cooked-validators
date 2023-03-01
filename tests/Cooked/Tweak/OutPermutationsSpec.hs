{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.Tweak.OutPermutationsSpec (tests) where

import Control.Monad (join, replicateM)
import Cooked
import Cooked.MockChain.Staged
import Cooked.TestUtils
import Cooked.Tweak.OutPermutations
import Data.Default
import Data.Either (rights)
import Data.List (group, groupBy)
import qualified Plutus.Script.Utils.Ada as Pl
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
      testGroup "tests for PermutOutTweakMode:" $
        let [a, b, c] = (\i -> paysPK (walletPKHash $ wallet i) $ Pl.lovelaceValueOf 123) <$> [1 .. 3]
            skel x y z = txSkelTemplate {txSkelOuts = [x, y, z]}
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
            ],
      testGroup "tests for a single random outputs permutation:" $
        let l = (\i -> paysPK (walletPKHash $ wallet i) $ Pl.lovelaceValueOf 123) <$> [1 .. 5]
            runs = txSkelOuts . snd <$> rights ((\i -> runTweak (singleOutPermutTweak i) txSkelTemplate {txSkelOuts = l}) =<< [1 .. 5])
         in [ testCase "All permutations contain the correct elements" $
                mapM_ (assertSameSets l) runs,
              testCase "All permutations are different from the initial distribution" $
                mapM_ (assertBool "Lists should be different" . (l /=)) runs,
              testCase "Permutations are different with different seeds" $
                assertBool "There should be at least 2 different permutations" (length (group runs) == 5)
            ]
    ]
