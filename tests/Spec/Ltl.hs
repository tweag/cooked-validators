module Spec.Ltl where

import Control.Monad
import Control.Monad.Writer
import Cooked.Ltl
import Cooked.MockChain.Testing
import Data.Maybe
import Test.Tasty
import Test.Tasty.HUnit

data TestBuiltin a where
  EmitInteger :: Integer -> TestBuiltin ()
  GetInteger :: TestBuiltin Integer

data TestModification
  = Add Integer
  | Mul Integer
  | Fail
  deriving (Show, Eq)

applyMod :: Integer -> TestModification -> Maybe Integer
applyMod _ Fail = Nothing
applyMod i (Add i') = if i == i' then Nothing else Just $ i + i'
applyMod i (Mul i') = if i == i' then Nothing else Just $ i * i'

type TestStaged = StagedLtl TestModification TestBuiltin

instance (MonadPlus m, MonadWriter [Integer] m) => ModInterpBuiltin TestModification TestBuiltin m where
  modifyAndInterpBuiltin GetInteger = Left (return 42)
  modifyAndInterpBuiltin (EmitInteger i) = Right $ \now ->
    maybe mzero (tell . (: [])) $
      foldl
        ( \acc el -> do
            current <- acc
            case el of
              Apply modif -> applyMod current modif
              EnsureFailure modif -> do
                guard $ isNothing $ applyMod current modif
                return current
        )
        (Just i)
        now

emitInteger :: Integer -> TestStaged ()
emitInteger = singletonBuiltin . EmitInteger

getInteger :: TestStaged Integer
getInteger = singletonBuiltin GetInteger

go :: TestStaged a -> [[Integer]]
go = execWriterT . interpStagedLtl

nonemptyTraces :: [TestStaged ()]
nonemptyTraces =
  [ getInteger >>= emitInteger,
    emitInteger 1 >> emitInteger 2,
    emitInteger 1 >> getInteger >>= emitInteger >> emitInteger 2
  ]

emptyTraces :: [TestStaged ()]
emptyTraces = [return (), void getInteger]

testTraces :: [TestStaged ()]
testTraces = nonemptyTraces ++ emptyTraces

tests :: TestTree
tests =
  testGroup
    "LTL"
    [ let add1 = LtlAtom $ Add 1
          add2 = LtlAtom $ Add 2
          add3 = LtlAtom $ Add 3
          failMod = LtlAtom Fail
          untilDirect = add1 `LtlUntil` add2
          untilIndirect = add2 `LtlOr` (add1 `LtlAnd` LtlNext (add1 `LtlUntil` add2))
          releaseDirect = add1 `LtlRelease` add2
          releaseIndirect = add2 `LtlAnd` (add1 `LtlOr` LtlNext (add1 `LtlRelease` add2))
       in testGroup
            "simple laws"
            [ testCase "LtlFalsity fails on every computation" $
                testAll (\tr -> go (modifyLtl @TestModification LtlFalsity tr) @?= []) testTraces,
              testCase "LtlTruth leaves every computation unchanged" $
                testAll (\tr -> go (modifyLtl @TestModification LtlTruth tr) @?= go tr) testTraces,
              testCase "x `LtlUntil` y == y `LtlOr` (x `LtlAnd` LtlNext (x `LtlUntil` y))" $
                testAll
                  (\tr -> assertSameSets (go $ modifyLtl untilDirect tr) (go $ modifyLtl untilIndirect tr))
                  testTraces,
              testCase "x `LtlRelease` y == y `LtlAnd` (x `LtlOr` LtlNext (x `LtlRelease` y)) for nonempty traces" $
                testAll
                  (\tr -> assertSameSets (go $ modifyLtl releaseDirect tr) (go $ modifyLtl releaseIndirect tr))
                  nonemptyTraces,
              testCase "Negation of a failing atom" $
                go (modifyLtl (LtlNot failMod) (emitInteger 3)) @?= [[3]],
              testCase "Negation of a successful atom" $
                go (modifyLtl (LtlNot add2) (emitInteger 3)) @?= [],
              testCase "Negation of the conjunction of atoms" $
                go . modifyLtl (LtlNot (add2 `LtlAnd` add3)) . emitInteger
                  <$> [ 2, -- add2 will fail, thus it will succeed, unmodified
                        3, -- add3 will fail, thus it will succeed, unmodified
                        4 -- both would succeed, thus it fails
                      ]
                  @?= [ [[2]],
                        [[3]],
                        []
                      ],
              testCase "Negation of the disjunction of atoms" $
                go . modifyLtl (LtlNot (add2 `LtlOr` failMod)) . emitInteger
                  <$> [ 2, -- add2 will fail, and failMod too, thus it succeeds
                        3 -- failMod fails, but not add2, thus it fails
                      ]
                  @?= [ [[2]],
                        []
                      ],
              testCase "Conjunction" $
                go (modifyLtl (add1 `LtlAnd` add2) (emitInteger 3)) @?= [[3 + 1 + 2]],
              testCase "Implication when the first modification does not apply" $
                go (modifyLtl (add1 `ltlImplies` add2) (emitInteger 1)) @?= [[1]],
              testCase "Implication when both modifications apply" $
                go (modifyLtl (add1 `ltlImplies` add2) (emitInteger 3)) @?= [[3 + 1 + 2]],
              testCase "Implication when the first modification applies, but not the second" $
                go (modifyLtl (add1 `ltlImplies` add3) (emitInteger 2)) @?= [],
              testCase "Implication backwards in time" $
                go . modifyLtl (LtlNext add1 `ltlImplies` add3) . mapM_ emitInteger
                  <$> [ [2, 4], -- add1 applies to 4, and add3 to 2, thus they are both performed
                        [2, 1], -- add1 does not apply to 1, thus add3 is not applied to 2, even though it could
                        [3, 1], -- add1 does not apply to 1, thus it does not matter that add3 does not apply to 3
                        [3, 2] -- add1 applies to 2, but add3 does not apply to 3, which is forbidden
                      ]
                  @?= [ [[2 + 3, 4 + 1]],
                        [[2, 1]],
                        [[3, 1]],
                        []
                      ]
            ],
      testGroup
        "unit tests"
        [ testCase "LtlNext changes the second step" $
            let n = 3
                incSeconds :: [[Integer]] -> [[Integer]]
                incSeconds = filter (/= []) . map incSecond
                  where
                    incSecond (a : b : cs) = a : b + n : cs
                    incSecond _ = []
             in testAll
                  ( \tr ->
                      assertSameSets
                        (go $ modifyLtl (LtlNext $ LtlAtom $ Add n) tr)
                        (incSeconds $ go tr)
                  )
                  testTraces,
          testCase "everywhere changes everything" $
            let n = 3
                incAll :: [[Integer]] -> [[Integer]]
                incAll = map (map (+ n))
             in testAll
                  (\tr -> assertSameSets (go $ modifyLtl (ltlAlways' (Add n)) tr) (incAll $ go tr))
                  testTraces,
          testCase "somewhere case-splits" $
            let n = 3
                caseSplit :: [[Integer]] -> [[Integer]]
                caseSplit = concatMap alternatives
                  where
                    alternatives [] = []
                    alternatives (x : xs) = (x + n : xs) : map (x :) (alternatives xs)
             in testAll
                  (\tr -> assertSameSets (go $ modifyLtl (ltlEventually' (Add n)) tr) (caseSplit $ go tr))
                  testTraces,
          testCase "somewhere is exponential in branch number" $
            let tr = emitInteger 42 >> emitInteger 3
             in assertSameSets
                  (go $ modifyLtl (ltlEventually' (Add 1)) $ modifyLtl (ltlEventually' (Add 2)) tr)
                  [ [42 + 1 + 2, 3],
                    [42, 3 + 1 + 2],
                    [42 + 1, 3 + 2],
                    [42 + 2, 3 + 1]
                  ],
          testCase "Modification order using 'LtlAnd' is respected (left to right)" $
            assertSameSets (go $ modifyLtl (LtlAtom (Add 1) `LtlAnd` LtlAtom (Mul 4)) $ emitInteger 2) [[2 * 4 + 1]],
          testCase "Modification order using modalities is respected (inner to outer)" $
            assertSameSets (go $ modifyLtl (LtlAtom (Add 1)) $ modifyLtl (LtlAtom (Mul 4)) $ emitInteger 2) [[9]],
          testCase "nested everywhere combines modifications" $
            assertSameSets
              ( go $ do
                  modifyLtl (ltlAlways' (Add 1)) $ do
                    emitInteger 42
                    modifyLtl (ltlAlways' (Add 2)) $ do
                      emitInteger 43
                      modifyLtl (ltlAlways' (Add 3)) $ do
                        emitInteger 44
                      emitInteger 45
                    emitInteger 46
                  emitInteger 47
              )
              [[42 + 1, 43 + 1 + 2, 44 + 1 + 2 + 3, 45 + 1 + 2, 46 + 1, 47]]
        ],
      testGroup
        "LTL Combinators"
        $ let traceSolo = emitInteger 24
              traceDuo = emitInteger 24 >> emitInteger 13
           in [ testCase "ltlAny" $
                  assertSameSets
                    (go $ modifyLtl (ltlAny' [Add 5, Mul 5]) traceSolo)
                    [ [24 + 5],
                      [24 * 5]
                    ],
                testCase "ltlAny [ltlAlways, ltlEventually]" $
                  assertSameSets
                    (go $ modifyLtl (ltlAny [ltlAlways' (Add 5), ltlEventually' (Mul 5)]) traceDuo)
                    [ [24 + 5, 13 + 5],
                      [24 * 5, 13],
                      [24, 13 * 5]
                    ],
                testCase "ltlAny [ltlAlways ltlAny, ltlEventually ltlAny]" $
                  assertSameSets
                    (go $ modifyLtl (ltlAny [ltlAlways (ltlAny' [Add 5, Mul 5]), ltlEventually (ltlAny' [Add 5, Mul 5])]) traceDuo)
                    [ [24 + 5, 13 + 5],
                      [24 + 5, 13 * 5],
                      [24 * 5, 13 * 5],
                      [24 * 5, 13 + 5],
                      [24 + 5, 13],
                      [24 * 5, 13],
                      [24, 13 + 5],
                      [24, 13 * 5]
                    ],
                testCase "ltlAll" $
                  assertSameSets
                    (go $ modifyLtl (ltlAll' [Add 5, Mul 5]) traceSolo)
                    [[24 * 5 + 5]],
                testCase "ltlAall [ltlAny, ltlAny]" $
                  assertSameSets
                    (go $ modifyLtl (ltlAll [ltlAny' [Add 5, Mul 5], ltlAny' [Add 5, Mul 5]]) traceSolo)
                    [ [24 + 5 + 5],
                      [24 * 5 + 5],
                      [24 * 5 * 5],
                      [(24 + 5) * 5]
                    ],
                testCase "ltlDelay (neg)" $
                  assertSameSets
                    (go $ modifyLtl (ltlDelay' 0 (Add 5)) traceDuo)
                    (go $ modifyLtl (ltlDelay' (-10) (Add 5)) traceDuo),
                testCase "ltlDelay' (pos)" $
                  assertSameSets
                    (go $ modifyLtl (ltlDelay' 1 (Add 5)) traceDuo)
                    [[24, 13 + 5]],
                testCase "ltlDelay (ltlAny [ltlEventually, ltlAlways])" $
                  assertSameSets
                    (go $ modifyLtl (ltlDelay 3 (ltlAny [ltlEventually' (Add 5), ltlAlways' (Mul 5)])) (traceDuo >> traceDuo >> traceDuo))
                    [ [24, 13, 24, 13 + 5, 24, 13],
                      [24, 13, 24, 13, 24 + 5, 13],
                      [24, 13, 24, 13, 24, 13 + 5],
                      [24, 13, 24, 13 * 5, 24 * 5, 13 * 5]
                    ],
                testCase "ltlAlways fails if a step cannot be modified" $
                  assertSameSets
                    (go $ modifyLtl (ltlAlways' (Add 5)) (traceDuo >> emitInteger 5))
                    [],
                testCase "ltlEventually succeeds if a step cannot be modified" $
                  assertSameSets
                    (go $ modifyLtl (ltlEventually' (Add 5)) (traceDuo >> emitInteger 5))
                    [ [24 + 5, 13, 5],
                      [24, 13 + 5, 5]
                    ],
                testCase "ltlWheneverPossible succeeds if a few steps cannot be modified" $
                  assertSameSets
                    ( go $
                        modifyLtl
                          (ltlWhenPossible' (Add 5))
                          (traceDuo >> emitInteger 5 >> emitInteger 5 >> traceDuo >> emitInteger 5 >> traceDuo)
                    )
                    [[24 + 5, 13 + 5, 5, 5, 24 + 5, 13 + 5, 5, 24 + 5, 13 + 5]],
                testCase "ltlNever succeeds when no step can be modified..." $
                  assertSameSets
                    (go $ modifyLtl (ltlNever' (Add 5)) (replicateM 10 (emitInteger 5)))
                    [replicate 10 5],
                testCase "... and fails otherwise" $
                  assertSameSets
                    (go $ modifyLtl (ltlNever' (Add 5)) $ modifyLtl (ltlEventually' (Add 1)) $ replicateM 10 (emitInteger 5))
                    []
              ]
    ]
