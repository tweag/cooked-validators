{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cooked.LtlSpec (tests) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Cooked.Ltl
import Data.Set (fromList)
import Test.Tasty
import Test.Tasty.HUnit

data TestBuiltin a where
  EmitInteger :: Integer -> TestBuiltin ()
  GetInteger :: TestBuiltin Integer

type TestModification = Integer -> Integer

instance {-# OVERLAPS #-} Semigroup TestModification where
  a <> b = a . b

instance {-# OVERLAPS #-} Monoid TestModification where
  mempty = id

instance MonadPlus m => InterpLtl TestModification TestBuiltin (WriterT [Integer] m) where
  interpBuiltin GetInteger = return 42
  interpBuiltin (EmitInteger i) =
    get
      >>= msum
        . map (\(now, later) -> tell [now i] <* put later)
        . nowLaterList

{- Remark: Why are we re-defining 'somewhere' and 'everywhere' here?

In some sense, the following two definitions of 'somewhere' and 'everywhere' are
the correct ones, because they work in an arbitrary 'MonadModal'. The
definitions in "Cooked.MockChain.Monad.Staged" are necessary because we want
functions with those names that we can directly apply to 'Attack's: Since the
'Modification's of any 'MonadModal' (including 'MonadModalMockChain') have to be
a constant type, but 'Attack' isn't, we use the definitions there to hide the
'UntypedAttack' wrapper from the user.

With the definitions below, one would have to write

> somewhere (UntypedAttack a) trace

instead of

> somewhere a trace

in the only use-case outside of tests. This justifies the re-definition here, in
my opinion.

-}

somewhere :: MonadModal m => Modification m -> m a -> m a
somewhere x = modifyLtl $ LtlTruth `LtlUntil` LtlAtom x

everywhere :: MonadModal m => Modification m -> m a -> m a
everywhere x = modifyLtl $ LtlFalsity `LtlRelease` LtlAtom x

emitInteger :: Integer -> Staged (LtlOp TestModification TestBuiltin) ()
emitInteger i = Instr (Builtin (EmitInteger i)) Return

getInteger :: Staged (LtlOp TestModification TestBuiltin) Integer
getInteger = Instr (Builtin GetInteger) Return

go :: Staged (LtlOp TestModification TestBuiltin) a -> [[Integer]]
go = execWriterT . flip execStateT [] . interpLtl

nonemptyTraces :: [Staged (LtlOp TestModification TestBuiltin) ()]
nonemptyTraces =
  [ getInteger >>= emitInteger,
    emitInteger 1 >> emitInteger 2,
    emitInteger 1 >> getInteger >>= emitInteger >> emitInteger 2
  ]

emptyTraces :: [Staged (LtlOp TestModification TestBuiltin) ()]
emptyTraces = [return (), void getInteger]

testTraces :: [Staged (LtlOp TestModification TestBuiltin) ()]
testTraces = nonemptyTraces ++ emptyTraces

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

tests :: TestTree
tests =
  testGroup
    "LTL"
    [ testGroup
        "simple laws"
        [ testCase "LtlFalsity fails on every computation" $
            assertAll testTraces (\tr -> go (modifyLtl LtlFalsity tr) @?= []),
          testCase "LtlTruth leaves every computation unchanged" $
            assertAll testTraces (\tr -> go (modifyLtl LtlTruth tr) @?= go tr),
          testCase "x `LtlUntil` y == y `LtlOr` (x `LtlAnd` LtlNext (x `LtlUntil` y))" $
            let x = LtlAtom (1 +)
                y = LtlAtom (2 +)
                a = x `LtlUntil` y
                b = y `LtlOr` (x `LtlAnd` LtlNext (x `LtlUntil` y))
             in assertAll
                  testTraces
                  (\tr -> assertEqualSets (go $ modifyLtl a tr) (go $ modifyLtl b tr)),
          testCase "x `LtlRelease` y == y `LtlAnd` (x `LtlOr` LtlNext (x `LtlRelease` y)) for nonempty traces" $
            let x = LtlAtom (1 +)
                y = LtlAtom (2 +)
                a = x `LtlRelease` y
                b = y `LtlAnd` (x `LtlOr` LtlNext (x `LtlRelease` y))
             in assertAll
                  nonemptyTraces
                  (\tr -> assertEqualSets (go $ modifyLtl a tr) (go $ modifyLtl b tr))
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
                        (go $ modifyLtl (LtlNext $ LtlAtom (n +)) tr)
                        (incSeconds $ go tr)
                  ),
          testCase "everywhere changes everything" $
            let n = 3

                incAll :: [[Integer]] -> [[Integer]]
                incAll = map (map (+ n))
             in assertAll
                  testTraces
                  (\tr -> assertEqualSets (go $ everywhere (n +) tr) (incAll $ go tr)),
          testCase "somewhere case-splits" $
            let n = 3

                caseSplit :: [[Integer]] -> [[Integer]]
                caseSplit = concatMap alternatives
                  where
                    alternatives [] = []
                    alternatives (x : xs) = (x + n : xs) : map (x :) (alternatives xs)
             in assertAll
                  testTraces
                  (\tr -> assertEqualSets (go $ somewhere (n +) tr) (caseSplit $ go tr)),
          testCase "somewhere is exponential in branch number" $
            -- If we have @tr = a >> b@, we expect
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
             in assertEqualSets
                  (go $ somewhere (1 +) $ somewhere (2 +) tr)
                  [[42 + 1 + 2, 3], [42, 3 + 1 + 2], [42 + 1, 3 + 2], [42 + 2, 3 + 1]],
          testCase "modality order is respected" $
            assertEqualSets (go $ everywhere (1 +) $ everywhere (const 2) $ emitInteger 1) [[3]],
          testCase "nested everywhere combines modifications" $
            assertEqualSets
              ( go $
                  everywhere (1 +) $
                    emitInteger 42
                      >> everywhere
                        (2 +)
                        ( emitInteger 43
                            >> everywhere (3 *) (emitInteger 44)
                        )
                      >> emitInteger 45
              )
              [[42 + 1, 43 + 1 + 2, 44 * 3 + 1 + 2, 45 + 1]]
        ]
    ]
