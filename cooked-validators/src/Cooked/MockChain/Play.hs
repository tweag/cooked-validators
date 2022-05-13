{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Cooked.MockChain.Play where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer hiding (Alt)
import Data.Maybe
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (until)

data Ltl a
  = LtlTruth
  | LtlFalsity
  | LtlAtom a
  | LtlOr (Ltl a) (Ltl a)
  | LtlAnd (Ltl a) (Ltl a)
  | -- | Assert that the given formula holds at the next time step.
    LtlNext (Ltl a)
  | -- | Assert that the first formula holds at least until the second one begins
    -- to hold, which must happen eventually. The formulae
    -- > a `LtlUntil` b
    -- and
    -- > b `LtlOr` (a `LtlAnd` LtlNext (a `LtlUntil` b))
    -- are equivalent.
    LtlUntil (Ltl a) (Ltl a)
  | -- | Assert that the second formula has to be true up to and including the
    -- point when the first one becomes true; if that never happens, the second
    -- formula has to remain true forever. View this as dual to 'LtlUntil'. The
    -- formulae
    -- > a `LtlRelease` b
    -- and
    -- > b `LtlAnd` (a `LtlOr` LtlNext (a `LtlRelease` b))
    -- are equivalent.
    LtlRelease (Ltl a) (Ltl a)
  deriving (Show)

somewhere :: Ltl a -> Ltl a
somewhere x = LtlTruth `LtlUntil` x

everywhere :: Ltl a -> Ltl a
everywhere x = LtlFalsity `LtlRelease` x

-- | This function splits an LTL formula that describes a modification of a
-- computation into a list of @(doNow, doLater, finished)@ triples, where
-- * @doNow@ is the concrete modification to be applied to the current time
--   step,
-- * @doLater@ is an LTL formula describing the modification that should be
--   applied at the next time step, and
-- * @finished@ is a @Bool@ we use to get around the fact that time is
--   conceptually infinite in LTL, but the computations we consider (hopefully)
--   only have a finite number of steps. It indicates whether the given formula
--   allows the current time step to be the last one. More explicitly,
--     * @finished == True@ if the whole formula is satisfied, even if there is
--      no next step, and
--     * @finished == False@ if there must be a next step satisfying the "do
--       later" formula for the whole formula to be satisfied.
-- The return value is a list because a formula might be satisfied in different
-- ways. For example, the modification described by @a `LtlUntil` b@ might be
-- accomplished by applying the modification @b@ right now, or by applying @b@
-- right now and @a `LtlUntil` b@ at the next step; the returned list will
-- therefore contain these two options.
deriv ::
  -- | The "do nothing"-modification.
  a ->
  -- | Composition of modifications. Strictly speaking, this should be
  -- commutative, but if you know the dangers, you can do anything. TODO: more
  -- explanation.
  (a -> a -> a) ->
  -- | The input formula
  Ltl a ->
  -- | List of possible modifications to the current time step, together with
  -- the formula that should be used to modify the next time step, and an
  -- indication whether the formula allows the current time step to be the last
  -- one.
  [(a, Ltl a, Bool)]
deriv t _ LtlTruth = [(t, LtlTruth, True)]
deriv _ _ LtlFalsity = []
deriv _ _ (LtlAtom g) = [(g, LtlTruth, True)]
deriv t comp (a `LtlOr` b) = deriv t comp a ++ deriv t comp b
deriv t comp (a `LtlAnd` b) =
  [ (g `comp` f, ltlSimpl $ c `LtlAnd` d, i && j)
    | (f, c, i) <- deriv t comp a,
      (g, d, j) <- deriv t comp b
  ]
deriv t _ (LtlNext a) = [(t, a, False)]
deriv t comp (a `LtlUntil` b) =
  -- The following definition is equivalent to
  -- > deriv t comp b
  -- >   ++ [ (f, ltlSimpl $ c `LtlAnd` (a `LtlUntil` b), False)
  -- >        | (f, c, _) <- deriv t comp a
  -- >      ]
  -- but more readable:
  deriv t comp $
    b `LtlOr` (a `LtlAnd` LtlNext (a `LtlUntil` b))
deriv t comp (a `LtlRelease` b) =
  -- We have to be subtle here: Normally, we'd like to write something readable
  -- like
  -- > deriv t comp $
  -- >   b `LtlAnd` (a `LtlOr` LtlNext (a `LtlRelease` b))
  -- as we did in the equation for `LtlUntil`. This won't work, however, because
  -- of the finite number of time steps. The above definition would be evaluated
  -- to something like
  -- > [ (g `comp` f, ltlSimpl $ c `LtlAnd` d, i && j)
  -- >   | (f, c, i) <- deriv t comp b,
  -- >     (g, d, j) <- (t, a `LtlRelease` b, False) : deriv t comp a
  -- > ]
  -- (modulo the order of elements in the lists). If @a@ is simply @LtlFalsity@,
  -- that would mean that we can never finish, even if @b@ is always
  -- applicable. This means we have to write this (note the @True@ instead of
  -- @False@):
  [ (g `comp` f, ltlSimpl $ c `LtlAnd` d, i && j)
    | (f, c, i) <- deriv t comp b,
      (g, d, j) <- (t, a `LtlRelease` b, True) : deriv t comp a
  ]

-- | Straightforward simplification procedure for LTL formulae. This function
-- knows how 'LtlTruth' and 'LtlFalsity' play with conjunction an disjunction
-- and recursively applies this knowledge; it does not do anything "fancy" like
-- normal forms etc. It is only used to keep the formulae 'deriv' generates from
-- growing too wildly.
ltlSimpl :: Ltl a -> Ltl a
ltlSimpl expr =
  let (expr', progress) = simpl expr
   in if progress then expr' else expr
  where
    simpl :: Ltl a -> (Ltl a, Bool)
    simpl (LtlAnd a b) = simplAnd a b
    simpl (LtlOr a b) = simplOr a b
    simpl (LtlNext a) =
      let (a', pa) = simpl a
       in if pa
            then (LtlNext a', True)
            else (LtlNext a, False)
    simpl (LtlUntil a b) = recurse2 LtlUntil a b
    simpl (LtlRelease a b) = recurse2 LtlRelease a b
    simpl x = (x, False)

    simplAnd :: Ltl a -> Ltl a -> (Ltl a, Bool)
    simplAnd a b =
      let (a', pa) = simpl a
          (b', pb) = simpl b
       in case (a', b') of
            (LtlTruth, _) -> (b', True)
            (_, LtlTruth) -> (a', True)
            (LtlFalsity, _) -> (LtlFalsity, True)
            (_, LtlFalsity) -> (LtlFalsity, True)
            _ -> if pa || pb then (LtlAnd a' b', True) else (LtlAnd a b, False)

    simplOr :: Ltl a -> Ltl a -> (Ltl a, Bool)
    simplOr a b =
      let (a', pa) = simpl a
          (b', pb) = simpl b
       in case (a', b') of
            (LtlTruth, _) -> (LtlTruth, True)
            (_, LtlTruth) -> (LtlTruth, True)
            (LtlFalsity, _) -> (b', True)
            (_, LtlFalsity) -> (a', True)
            _ -> if pa || pb then (LtlOr a' b', True) else (LtlOr a b, False)

    recurse2 ::
      (Ltl a -> Ltl a -> Ltl a) ->
      Ltl a ->
      Ltl a ->
      (Ltl a, Bool)
    recurse2 f a b =
      let (a', pa) = simpl a
          (b', pb) = simpl b
       in if pa || pb
            then (f a' b', True)
            else (f a b, False)

data Op a where
  -- | the failing operation ("discard this branch"):
  Empty :: Op a
  -- | the "time-branching" operation
  Alt :: Staged Op a -> Staged Op a -> Op a
  -- | the operation that applies a modification on a certain sub-computation:
  Modify :: Ltl (Action -> Maybe Action) -> Staged Op a -> Op a
  -- -- Another nice-to-have: signalling an error. This should also behave
  -- -- sensibly with the time-branching structure.
  -- Fail :: String -> Op a

  -- this is where the MonadBlockChain Operations would normally go, these are
  -- here to have something simpler to play around with:
  GetInteger :: Op Integer
  EmitInteger :: Integer -> Op ()

data Staged (op :: * -> *) :: * -> * where
  Return :: a -> Staged op a
  Instr :: op a -> (a -> Staged op b) -> Staged op b

instance Functor (Staged Op) where
  fmap f (Return x) = Return $ f x
  fmap f (Instr op cont) = Instr op (fmap f . cont)

instance Applicative (Staged Op) where
  pure = Return
  (<*>) = ap

instance Monad (Staged Op) where
  (Return x) >>= f = f x
  (Instr i m) >>= f = Instr i (m >=> f)

instance Applicative (Staged Op) => Alternative (Staged Op) where
  empty = Instr Empty Return
  a <|> b = Instr (Alt a b) Return

type Action = Integer

interpInstr ::
  (MonadPlus m) =>
  Bool ->
  Ltl (Action -> Maybe Action) ->
  Op a ->
  (a -> Staged Op b) ->
  WriterT [Integer] m b
interpInstr p x GetInteger f = (interpLtl p x . f) 42
interpInstr _ x (EmitInteger i) f =
  msum $
    map
      (\(m, y, q) -> maybe mzero (tell . (: [])) (m i) >>= interpLtl q y . f)
      (deriv Just (\g h -> maybe Nothing g . h) x)
interpInstr _ _ Empty _ = mzero
interpInstr p x (Alt a b) f = interpLtl p x (a >>= f) `mplus` interpLtl p x (b >>= f)
interpInstr p x (Modify y c) f = interpLtl p (x `LtlAnd` y) c >>= interpLtl p x . f

interpLtl :: (MonadPlus m) => Bool -> Ltl (Action -> Maybe Action) -> Staged Op a -> WriterT [Integer] m a
interpLtl p _ (Return a) = if p then return a else mzero
interpLtl p x (Instr o f) = interpInstr p x o f

-- tests/examples

trace1 :: Staged Op ()
trace1 =
  Instr (EmitInteger 3) $
    \_ ->
      Instr GetInteger $
        \i -> Instr (EmitInteger i) Return

trace2 :: Staged Op ()
trace2 = Instr (EmitInteger 2) $ \_ -> Instr (EmitInteger 5) Return

testSomewhere :: [[Integer]]
testSomewhere = execWriterT $ interpLtl True (somewhere (LtlAtom $ Just . (* 2))) trace1

testEverywhere :: [[Integer]]
testEverywhere = execWriterT $ interpLtl True (everywhere (LtlAtom $ Just . (* 2))) trace1

testNext :: [[Integer]]
testNext = execWriterT $ interpLtl True (LtlNext (LtlAtom $ Just . (* 2))) trace1

testAndNext :: [[Integer]]
testAndNext =
  execWriterT $
    interpLtl
      True
      (LtlAtom (Just . (+ 1)) `LtlAnd` LtlNext (LtlAtom f))
      (trace1 <|> trace2)
  where
    f 42 = Just 0
    f _ = Nothing

traceWithModify :: Staged Op ()
traceWithModify =
  Instr (EmitInteger 1) $
    \_ ->
      Instr
        ( Modify x $
            Instr (EmitInteger 2) $
              \_ -> Instr (EmitInteger 3) Return
        )
        $ \_ -> Instr (EmitInteger 5) Return
  where
    x = LtlAtom (Just . (+ 1)) `LtlOr` LtlNext (LtlAtom (Just . (* 3)))

testModify :: [[Integer]]
testModify = execWriterT $ interpLtl True LtlTruth traceWithModify

tests :: TestTree
tests =
  testGroup
    "LTL tests"
    [ testCase "somewhere behaves" $
        testSomewhere @?= [[6, 42], [3, 84]],
      testCase "everywhere behaves" $
        testEverywhere @?= [[6, 84]],
      testCase "next behaves" $
        testNext @?= [[3, 84]],
      testCase "and-next behaves" $
        testAndNext @?= [[4, 0]],
      testCase "Modify in a context with no modification behaves" $
        testModify @?= [[1, 3, 3, 5], [1, 2, 9, 5]]
    ]
