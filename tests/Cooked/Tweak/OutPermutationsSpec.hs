module Cooked.Tweak.OutPermutationsSpec (tests) where

import Cooked
import Cooked.Tweak.OutPermutations
import Plutus.Script.Utils.Value qualified as Script
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
        let a = wallet 1 `receives` Value (Script.lovelace 123)
            b = wallet 2 `receives` Value (Script.lovelace 123)
            c = wallet 3 `receives` Value (Script.lovelace 123)
            skel x y z = txSkelTemplate {txSkelOuts = [x, y, z]}
         in [ testCase "KeepIdentity (Just 2)" $
                assertSameSets
                  (map (Right . ((),)) [skel a b c, skel b a c])
                  (fst <$> runTweak (allOutPermutsTweak $ KeepIdentity $ Just 2) (skel a b c)),
              testCase "KeepIdentity Nothing" $
                assertSameSets
                  (map (Right . ((),)) [skel a b c, skel a c b, skel b a c, skel b c a, skel c a b, skel c b a])
                  (fst <$> runTweak (allOutPermutsTweak $ KeepIdentity Nothing) (skel a b c)),
              testCase "OmitIdentity (Just 2)" $
                assertSameSets
                  [Right ((), skel b a c)]
                  (fst <$> runTweak (allOutPermutsTweak $ OmitIdentity $ Just 2) (skel a b c)),
              testCase "OmitIdentity Nothing" $
                assertSameSets
                  (map (Right . ((),)) [skel a c b, skel b a c, skel b c a, skel c a b, skel c b a])
                  (fst <$> runTweak (allOutPermutsTweak $ OmitIdentity Nothing) (skel a b c))
            ]
    ]
