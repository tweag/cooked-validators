{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cooked.Ltl.IntegerIdentity (integerIdentityTests) where

import Control.Monad (MonadPlus, forM_, msum, void)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (execStateT, get, put)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Cooked.Ltl
import Cooked.Ltl.Structure (Labelled, Mod (Mod), ModExt, labelledBy, lift, toLabelled)
import Data.Set (fromList)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Debug.Trace (trace)

-- The type of modifications on integers that cannot fail
type IntegerIdentityMods = ModExt Integer Identity

-- The builtins with a visible and an invisible event
data EmitAndGetIntegers a where
  EmitInteger :: Integer -> EmitAndGetIntegers ()
  GetInteger :: EmitAndGetIntegers Integer

-- Maybe we could go a bit further in providing interpltl
-- The idea would be to force users to only provide visible events.
-- Should their traces contains invisible events (events that should
-- be ignored in the modification process), they could provide an
-- overlay only providing visible events.
instance
  MonadPlus m =>
  InterpLtl
    IntegerIdentityMods
    EmitAndGetIntegers
    (WriterT [(Integer, [String])] m)
  where
  -- GetInteger is made "invisible" here since modifications ignore it
  interpBuiltin GetInteger = return 42
  interpBuiltin (EmitInteger i) =
    get
      >>= msum
        . map (\(now, later) -> tell [runIdentity $ now $ toLabelled i] <* put later)
        . nowLaterList

type IntegerIdentityOp = LtlOp IntegerIdentityMods EmitAndGetIntegers

emitInteger :: Integer -> Staged IntegerIdentityOp ()
emitInteger i = Instr (Builtin (EmitInteger i)) Return

getInteger :: Staged IntegerIdentityOp Integer
getInteger = Instr (Builtin GetInteger) Return

-- Can't we provide this function?
go :: Staged IntegerIdentityOp a -> [[Labelled Integer]]
go = execWriterT . flip execStateT [] . interpLtl

-- The family of modifications that add a certain integer
addValue :: Integer -> ModExt Integer Identity
addValue n = lift $ Mod ("+" ++ show n) (return . (+ n))

-- The family of modifications that set to a certain integer
setValue :: Integer -> ModExt Integer Identity
setValue n = lift $ Mod ("=" ++ show n) (\_ -> return n)

-- Some traces
nonemptyTraces :: [Staged IntegerIdentityOp ()]
nonemptyTraces =
  [ getInteger >>= emitInteger,
    emitInteger 1 >> emitInteger 2,
    emitInteger 1 >> getInteger >>= emitInteger >> emitInteger 2
  ]

emptyTraces :: [Staged IntegerIdentityOp ()]
emptyTraces = [return (), void getInteger]

testTraces :: [Staged IntegerIdentityOp ()]
testTraces = emptyTraces ++ nonemptyTraces

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

integerIdentityTests :: [TestTree]
integerIdentityTests =
  [ testGroup
      "Truth and Falsity laws"
      [ testCase "LtlFalsity fails on every computation" $
          forM_
            testTraces
            ( \tr ->
                go (startLtl LtlFalsity >> tr) @?= []
            ),
        testCase "LtlTruth leaves every computation unchanged" $
          forM_
            testTraces
            ( \tr ->
                go (startLtl LtlTruth >> tr) @?= go tr
            )
      ],
    testGroup
      "Until and Release laws"
      ( let x = LtlAtom (addValue 1)
            y = LtlAtom (addValue 2)
            a = x `LtlUntil` y
            b = y `LtlOr` (x `LtlAnd` LtlNext (x `LtlUntil` y))
            c = x `LtlRelease` y
            d = y `LtlAnd` (x `LtlOr` LtlNext (x `LtlRelease` y))
         in [ testCase "x `LtlUntil` y == y `LtlOr` (x `LtlAnd` LtlNext (x `LtlUntil` y))" $
                forM_
                  testTraces
                  ( \tr ->
                      assertEqualSets (go $ startLtl a >> tr) (go $ startLtl b >> tr)
                  ),
              testCase "x `LtlRelease` y == y `LtlAnd` (x `LtlOr` LtlNext (x `LtlRelease` y)) for nonempty traces" $
                forM_
                  nonemptyTraces
                  ( \tr ->
                      assertEqualSets (go $ startLtl c >> tr) (go $ startLtl d >> tr)
                  )
            ]
      ),
    testGroup
      "Unit tests"
      [ testCase "LtlNext changes the second step if any" $
          forM_
            testTraces
            ( \tr ->
                let ans = go $ startLtl (LtlNext $ LtlAtom $ addValue 3) >> tr
                 in assertBool "Second step should be modified if any" (length ans < 2 || labelledBy (head $ head ans) "+3")
            ),
        testCase "everywhere changes everything" $
          forM_
            testTraces
            ( \tr ->
                assertBool "Everything should be changed" (all (all (`labelledBy` "+3")) (go $ everywhere (addValue 3) tr))
            ),
        testCase "somewhere case-splits" $
          forM_
            testTraces
            ( \tr ->
                assertBool "One and only on computation should be changed" (all ((1 ==) . length . filter (`labelledBy` "+3")) (go $ somewhere (addValue 3) tr))
            ),
        testCase "somewhere is exponential in branch numbers" $
          forM_
            testTraces
            ( \tr ->
                assertBool
                  "There should be n^2 cases"
                  ( let out = go $ somewhere (addValue 3) $ somewhere (addValue 1) tr in
                     null out || length out == length (head out) ^ 2
                  )
            ),
        testCase "modality order is respected" $
          assertBool
            "The modality order should be respected"
            ( let e = go $ everywhere (addValue 1) $ everywhere (setValue 2) $ emitInteger 1 in trace (show e) 
            (fst (head (head (go $ everywhere (addValue 1) $ everywhere (setValue 2) $ emitInteger 1))) == 3))
      ]
      --     testCase "modality order is respected" $
      --       assertEqualSets (go $ everywhere (1 +) $ everywhere (const 2) $ emitInteger 1) [[3]],
      --     testCase "nested everywhere combines modifications" $
      --       assertEqualSets
      --         ( go $
      --             everywhere (1 +) $
      --               emitInteger 42
      --                 >> everywhere
      --                   (2 +)
      --                   ( emitInteger 43
      --                       >> everywhere (3 *) (emitInteger 44)
      --                   )
      --                 >> emitInteger 45
      --         )
      --         [[42 + 1, 43 + 1 + 2, 44 * 3 + 1 + 2, 45 + 1]]
      --   ]
  ]
