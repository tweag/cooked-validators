{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Ltl where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Cooked.Ltl
import Cooked.Ltl.Combinators
import Cooked.MockChain.Staged
import Cooked.MockChain.Testing
import Data.Maybe
import Test.Tasty
import Test.Tasty.HUnit

data TestBuiltin a where
  EmitInteger :: Integer -> TestBuiltin ()
  GetInteger :: TestBuiltin Integer
  WrapLtl :: Ltl TestModification -> Staged TestBuiltin a -> TestBuiltin a

instance MonadLtl TestModification (Staged TestBuiltin) where
  modifyLtl formula = singletonBuiltin . WrapLtl formula

data TestModification
  = Add Integer
  | Mul Integer
  | Fail
  deriving (Show, Eq)

applyMod :: Integer -> TestModification -> Maybe Integer
applyMod _ Fail = Nothing
applyMod i (Add i') = if i == i' then Nothing else Just $ i + i'
applyMod i (Mul i') = if i == i' then Nothing else Just $ i * i'

interpBuiltin :: (MonadPlus m) => TestBuiltin a -> StateT [Ltl TestModification] (WriterT [Integer] m) a
interpBuiltin GetInteger = return 42
interpBuiltin (EmitInteger i) = do
  gets nowLaterList
    >>= msum
      . map
        ( \(now, later) -> do
            maybe mzero (tell . (: [])) $
              foldl
                ( \acc (modif, el) -> do
                    current <- acc
                    if el
                      then
                        applyMod current modif
                      else do
                        guard $ isNothing $ applyMod current modif
                        return current
                )
                (Just i)
                now
            put later
        )
interpBuiltin (WrapLtl formula comp) = do
  modify' (formula :)
  res <- interpStaged interpBuiltin comp
  formulas <- get
  unless (null formulas) $ do
    guard $ finished $ head formulas
    put $ tail formulas
  return res

emitInteger :: Integer -> Staged TestBuiltin ()
emitInteger = singletonBuiltin . EmitInteger

getInteger :: Staged TestBuiltin Integer
getInteger = singletonBuiltin GetInteger

go :: Staged TestBuiltin a -> [[Integer]]
go = execWriterT . flip execStateT [] . interpStaged interpBuiltin

nonemptyTraces :: [Staged TestBuiltin ()]
nonemptyTraces =
  [ getInteger >>= emitInteger,
    emitInteger 1 >> emitInteger 2,
    emitInteger 1 >> getInteger >>= emitInteger >> emitInteger 2
  ]

emptyTraces :: [Staged TestBuiltin ()]
emptyTraces = [return (), void getInteger]

testTraces :: [Staged TestBuiltin ()]
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
                go (modifyLtl (add1 `ltlImplies'` add2) (emitInteger 1)) @?= [[1]],
              testCase "Implication when both modifications apply" $
                go (modifyLtl (add1 `ltlImplies'` add2) (emitInteger 3)) @?= [[3 + 1 + 2]],
              testCase "Implication when the first modification applies, but not the second" $
                go (modifyLtl (add1 `ltlImplies'` add3) (emitInteger 2)) @?= [],
              testCase "Implication backwards in time" $
                go . modifyLtl (LtlNext add1 `ltlImplies'` add3) . mapM_ emitInteger
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
                  (\tr -> assertSameSets (go $ modifyLtl (always (Add n)) tr) (incAll $ go tr))
                  testTraces,
          testCase "somewhere case-splits" $
            let n = 3
                caseSplit :: [[Integer]] -> [[Integer]]
                caseSplit = concatMap alternatives
                  where
                    alternatives [] = []
                    alternatives (x : xs) = (x + n : xs) : map (x :) (alternatives xs)
             in testAll
                  (\tr -> assertSameSets (go $ modifyLtl (eventually (Add n)) tr) (caseSplit $ go tr))
                  testTraces,
          testCase "somewhere is exponential in branch number" $
            let tr = emitInteger 42 >> emitInteger 3
             in assertSameSets
                  (go $ modifyLtl (eventually (Add 1)) $ modifyLtl (eventually (Add 2)) tr)
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
                  modifyLtl (always (Add 1)) $ do
                    emitInteger 42
                    modifyLtl (always (Add 2)) $ do
                      emitInteger 43
                      modifyLtl (always (Add 3)) $ do
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
           in [ testCase "anyOf" $
                  assertSameSets
                    (go $ modifyLtl (anyOf [Add 5, Mul 5]) traceSolo)
                    [ [24 + 5],
                      [24 * 5]
                    ],
                testCase "anyOf [always, eventually]" $
                  assertSameSets
                    (go $ modifyLtl (anyOf' [always (Add 5), eventually (Mul 5)]) traceDuo)
                    [ [24 + 5, 13 + 5],
                      [24 * 5, 13],
                      [24, 13 * 5]
                    ],
                testCase "anyOf [always anyOf, eventually anyOf]" $
                  assertSameSets
                    (go $ modifyLtl (anyOf' [always' (anyOf [Add 5, Mul 5]), eventually' (anyOf [Add 5, Mul 5])]) traceDuo)
                    [ [24 + 5, 13 + 5],
                      [24 + 5, 13 * 5],
                      [24 * 5, 13 * 5],
                      [24 * 5, 13 + 5],
                      [24 + 5, 13],
                      [24 * 5, 13],
                      [24, 13 + 5],
                      [24, 13 * 5]
                    ],
                testCase "allOf" $
                  assertSameSets
                    (go $ modifyLtl (allOf [Add 5, Mul 5]) traceSolo)
                    [[24 * 5 + 5]],
                testCase "allOf [anyOf, anyOf]" $
                  assertSameSets
                    (go $ modifyLtl (allOf' [anyOf [Add 5, Mul 5], anyOf [Add 5, Mul 5]]) traceSolo)
                    [ [24 + 5 + 5],
                      [24 * 5 + 5],
                      [24 * 5 * 5],
                      [(24 + 5) * 5]
                    ],
                testCase "delay (neg)" $
                  assertSameSets
                    (go $ modifyLtl (delay 0 (Add 5)) traceDuo)
                    (go $ modifyLtl (delay (-10) (Add 5)) traceDuo),
                testCase "delay (pos)" $
                  assertSameSets
                    (go $ modifyLtl (delay 1 (Add 5)) traceDuo)
                    [[24, 13 + 5]],
                testCase "delay (anyOf [eventually, always])" $
                  assertSameSets
                    (go $ modifyLtl (delay' 3 (anyOf' [eventually (Add 5), always (Mul 5)])) (traceDuo >> traceDuo >> traceDuo))
                    [ [24, 13, 24, 13 + 5, 24, 13],
                      [24, 13, 24, 13, 24 + 5, 13],
                      [24, 13, 24, 13, 24, 13 + 5],
                      [24, 13, 24, 13 * 5, 24 * 5, 13 * 5]
                    ],
                testCase "always fails if a step cannot be modified" $
                  assertSameSets
                    (go $ modifyLtl (always (Add 5)) (traceDuo >> emitInteger 5))
                    [],
                testCase "eventually succeeds if a step cannot be modified" $
                  assertSameSets
                    (go $ modifyLtl (eventually (Add 5)) (traceDuo >> emitInteger 5))
                    [ [24 + 5, 13, 5],
                      [24, 13 + 5, 5]
                    ],
                testCase "wherever possible succeeds if a few steps cannot be modified" $
                  assertSameSets
                    ( go $
                        modifyLtl
                          (whenPossible (Add 5))
                          (traceDuo >> emitInteger 5 >> emitInteger 5 >> traceDuo >> emitInteger 5 >> traceDuo)
                    )
                    [[24 + 5, 13 + 5, 5, 5, 24 + 5, 13 + 5, 5, 24 + 5, 13 + 5]],
                testCase "never succeeds when no step can be modified..." $
                  assertSameSets
                    (go $ modifyLtl (never (Add 5)) (replicateM 10 (emitInteger 5)))
                    [replicate 10 5],
                testCase "... and fails otherwise" $
                  assertSameSets
                    (go $ modifyLtl (never (Add 5)) $ modifyLtl (eventually (Add 1)) $ replicateM 10 (emitInteger 5))
                    []
              ]
    ]
