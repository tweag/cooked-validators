{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cooked.MockChain.LtlSpec (tests) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Cooked.MockChain.Ltl
import Data.Set (fromList)
import Test.Tasty
import Test.Tasty.HUnit

data TestBuiltin a where
  EmitInteger :: Integer -> TestBuiltin ()
  GetInteger :: TestBuiltin Integer

type TestModification = Integer

instance Semigroup TestModification where
  a <> b = a + b

instance Monoid TestModification where
  mempty = 0

instance (MonadPlus m, MonadFail m) => InterpLtl TestModification TestBuiltin (WriterT [Integer] m) where
  interpBuiltin GetInteger = return 42
  interpBuiltin (EmitInteger i) =
    get
      >>= msum
        . map (\(now, later) -> tell [i + now] <* put later)
        . nowLater

emitInteger :: Integer -> Staged (LtlOp TestModification TestBuiltin) ()
emitInteger i = Instr (Builtin (EmitInteger i)) Return

getInteger :: Staged (LtlOp TestModification TestBuiltin) Integer
getInteger = Instr (Builtin GetInteger) Return

go :: Staged (LtlOp TestModification TestBuiltin) a -> [[Integer]]
go = execWriterT . flip execStateT LtlTruth . interpLtl

nonemptyTraces :: [Staged (LtlOp TestModification TestBuiltin) ()]
nonemptyTraces =
  [ getInteger >>= emitInteger,
    emitInteger 1 >> emitInteger 2,
    emitInteger 1 >> getInteger >>= emitInteger >> emitInteger 2,
    emitInteger 1 >> emitInteger 2 <|> emitInteger 3 >> emitInteger 4
  ]

emptyTraces :: [Staged (LtlOp TestModification TestBuiltin) ()]
emptyTraces = [return (), void getInteger]

testTraces :: [Staged (LtlOp TestModification TestBuiltin) ()]
testTraces = emptyTraces ++ nonemptyTraces

assertAll :: [a] -> (a -> Assertion) -> Assertion
assertAll space f = mapM_ f space

assertEqualSets :: (Show a, Ord a) => [a] -> [a] -> Assertion
assertEqualSets l r =
  assertBool
    ( "unequal sets:\n"
        ++ "expected: "
        ++ show r
        ++ "\n"
        ++ " but got: "
        ++ show l
    )
    (fromList l == fromList r)

tests :: [TestTree]
tests =
  [ testGroup
      "simple laws"
      [ testCase "LtlFalsity fails on every computation" $
          assertAll testTraces (\tr -> go (startLtl LtlFalsity >> tr) @?= []),
        testCase "LtlTruth leaves every computation unchanged" $
          assertAll testTraces (\tr -> go (startLtl LtlTruth >> tr) @?= go tr),
        testCase "x `LtlUntil` y == y `LtlOr` (x `LtlAnd` LtlNext (x `LtlUntil` y))" $
          let x = LtlAtom 1
              y = LtlAtom 2
              a = x `LtlUntil` y
              b = y `LtlOr` (x `LtlAnd` LtlNext (x `LtlUntil` y))
           in assertAll
                testTraces
                (\tr -> assertEqualSets (go $ startLtl a >> tr) (go $ startLtl b >> tr)),
        testCase "x `LtlRelease` y == y `LtlAnd` (x `LtlOr` LtlNext (x `LtlRelease` y)) for nonempty traces" $
          let x = LtlAtom 1
              y = LtlAtom 2
              a = x `LtlRelease` y
              b = y `LtlAnd` (x `LtlOr` LtlNext (x `LtlRelease` y))
           in assertAll
                nonemptyTraces
                (\tr -> assertEqualSets (go $ startLtl a >> tr) (go $ startLtl b >> tr))
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
           in assertAll
                testTraces
                ( \tr ->
                    assertEqualSets
                      (go $ startLtl (LtlNext $ LtlAtom n) >> tr)
                      (incSeconds $ go tr)
                ),
        testCase "everywhere changes everything" $
          let n = 3

              incAll :: [[Integer]] -> [[Integer]]
              incAll = map (map (+ n))
           in assertAll
                testTraces
                (\tr -> assertEqualSets (go $ everywhere n >> tr) (incAll $ go tr)),
        testCase "somewhere case-splits" $
          let n = 3

              caseSplit :: [[Integer]] -> [[Integer]]
              caseSplit = concatMap alternatives
                where
                  alternatives [] = []
                  alternatives (x : xs) = (x + n : xs) : map (x :) (alternatives xs)
           in assertAll
                testTraces
                (\tr -> assertEqualSets (go $ somewhere n >> tr) (caseSplit $ go tr)),
        testCase "somewhere is exponential in branch number" $
          -- If we make a trace @tr = a >> b@, we expect
          --
          -- > somewhere f $ somewhere g tr
          --
          -- to describe the following four traces:
          --
          -- > 1. f (g a) >> b
          -- > 2. f a >> g b
          -- > 3. g a >> f b
          -- > 4. a >> f (g b)
          --
          let tr = emitInteger 42 >> emitInteger 3
           in assertEqualSets (go $ somewhere 1 >> somewhere 2 >> tr) [[45, 3], [42, 6], [43, 5], [44, 4]],
        testCase "nested everywhere combines modifications" $
          assertEqualSets
            (go $ everywhere 1 >> emitInteger 5 >> everywhere 2 >> emitInteger 7 >> emitInteger 8)
            [[6, 10, 11]]
      ]
  ]
