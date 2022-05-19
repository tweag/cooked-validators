{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Cooked.MockChain.Ltl where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer hiding (Alt)
import Prelude hiding (until)

data Ltl a
  = LtlTruth
  | LtlFalsity
  | LtlAtom a
  | -- | Disjunction will be interpreted in an "intuitionistic" way, i.e. as
    -- branching into the "timeline" where the left disjunct holds and the one
    -- where the right disjunct holds. In that sense, it is an exclusive or,
    -- as it does not introduce the branche where both disjuncts hold.
    LtlOr (Ltl a) (Ltl a)
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
--
-- * @doNow@ is the modification to be applied to the current time step,
--
-- * @doLater@ is an LTL formula describing the modification that should be
--   applied from the next time step onwards, and
--
-- * @finished@ is a @Bool@ we use to get around the fact that time is
--   conceptually infinite in LTL, but the computations we consider (hopefully)
--   only have a finite number of steps. It indicates whether the given formula
--   allows the current time step to be the last one. More explicitly,
--
--     * @finished == True@ if the whole formula is satisfied, even if there is
--       no next step, and
--
--     * @finished == False@ if there must be a next step satisfying the "do
--       later" formula for the whole formula to be satisfied.
--
-- The return value is a list because a formula might be satisfied in different
-- ways. For example, the modification described by @a `LtlUntil` b@ might be
-- accomplished by applying the modification @b@ right now, or by applying @a@
-- right now and @a `LtlUntil` b@ from the next step onwards; the returned list
-- will contain these two options.
--
-- Modifications should form a monoid, where 'mempty' is the do-nothing
-- modification, and '<>' is the composition of modifications. Attention: If
-- '<>' is not commutative, conjunction will also fail to be commutative!
nowLater :: (Monoid a) => Ltl a -> [(a, Ltl a, Bool)]
nowLater LtlTruth = [(mempty, LtlTruth, True)]
nowLater LtlFalsity = []
nowLater (LtlAtom g) = [(g, LtlTruth, True)]
nowLater (a `LtlOr` b) = nowLater a ++ nowLater b
nowLater (a `LtlAnd` b) =
  [ (g <> f, ltlSimpl $ c `LtlAnd` d, i && j)
    | (f, c, i) <- nowLater a,
      (g, d, j) <- nowLater b
  ]
nowLater (LtlNext a) = [(mempty, a, False)]
nowLater (a `LtlUntil` b) =
  -- The following definition is equivalent to
  -- > nowLater b
  -- >   ++ [ (f, ltlSimpl $ c `LtlAnd` (a `LtlUntil` b), False)
  -- >        | (f, c, _) <- nowLater a
  -- >      ]
  -- but more readable:
  nowLater $ b `LtlOr` (a `LtlAnd` LtlNext (a `LtlUntil` b))
nowLater (a `LtlRelease` b) =
  -- We have to be subtle here: Normally, we'd like to write something readable
  -- like
  -- > nowLater $ b `LtlAnd` (a `LtlOr` LtlNext (a `LtlRelease` b))
  -- as we did in the equation for `LtlUntil`. This won't work, however, because
  -- of the finite number of time steps. The above definition would be evaluated
  -- to something like
  -- > [ (g <> f, ltlSimpl $ c `LtlAnd` d, i && j)
  -- >   | (f, c, i) <- nowLater b,
  -- >     (g, d, j) <- (mempty, a `LtlRelease` b, False) : nowLater a
  -- > ]
  -- (modulo the order of elements in the lists). If @a@ is simply @LtlFalsity@,
  -- that would mean that we can never finish, even if @b@ is always
  -- applicable. This means we have to write this (note the @True@ instead of
  -- @False@):
  [ (g <> f, ltlSimpl $ c `LtlAnd` d, i && j)
    | (f, c, i) <- nowLater b,
      (g, d, j) <- (mempty, a `LtlRelease` b, True) : nowLater a
  ]

-- | Straightforward simplification procedure for LTL formulae. This function
-- knows how 'LtlTruth' and 'LtlFalsity' play with the other connectives and
-- recursively applies this knowledge; it does not do anything fancy like normal
-- forms etc. It is only used to keep the formulae 'nowLater' generates from
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

data LtlOp (modification :: *) (builtin :: * -> *) :: * -> * where
  Empty :: LtlOp modification builtin a
  Alt :: Staged (LtlOp modification builtin) a -> Staged (LtlOp modification builtin) a -> LtlOp modification builtin a
  Fail :: String -> LtlOp modification builtin a
  Modify :: Ltl modification -> LtlOp modification builtin ()
  Builtin :: builtin a -> LtlOp modification builtin a

data Staged (op :: * -> *) :: * -> * where
  Return :: a -> Staged op a
  Instr :: op a -> (a -> Staged op b) -> Staged op b

instance Functor (Staged op) where
  fmap f (Return x) = Return $ f x
  fmap f (Instr op cont) = Instr op (fmap f . cont)

instance Applicative (Staged op) where
  pure = Return
  (<*>) = ap

instance Monad (Staged op) where
  (Return x) >>= f = f x
  (Instr i m) >>= f = Instr i (m >=> f)

instance Applicative (Staged (LtlOp modification builtin)) => Alternative (Staged (LtlOp modification builtin)) where
  empty = Instr Empty Return
  a <|> b = Instr (Alt a b) Return

instance MonadFail (Staged (LtlOp modification builtin)) where
  fail msg = Instr (Fail msg) Return

class Monad m => HasBuiltins (modification :: *) (builtin :: * -> *) m where
  modifyBuiltin ::
    Ltl modification ->
    builtin a ->
    [(m a, Ltl modification, Bool)]

-- class Monad m => InternalMonadLtl modification m where
--   unsafeAddModification :: Ltl modification -> (() -> m a) -> m a

-- instance HasBuiltins modification builtin m => InternalMonadLtl modification m where
--   unsafeAddModification x cont = Instr (Modify x) cont

-- canModify :: builtin a -> Bool

-- type Action = Integer

-- interpLtl ::
--   (Monoid modification, HasBuiltins modification builtin m, MonadPlus m, MonadFail m) =>
--   Bool ->
--   Ltl modification ->
--   Staged (LtlOp modification builtin) a ->
--   m a
-- interpLtl p _ (Return a) = if p then return a else mzero
-- interpLtl _ _ (Instr Empty _) = mzero
-- interpLtl p x (Instr (Alt a b) f) = interpLtl p x (a >>= f) `mplus` interpLtl p x (b >>= f)
-- interpLtl _ _ (Instr (Fail msg) _) = fail msg
-- -- see here for the reasoning why 'LtlAnd' is the right thing to do:
-- -- https://github.com/tweag/scverif-exploration/blob/main/LTL-Attacks/after.tex
-- interpLtl p x (Instr (Modify y) f) = interpLtl p (ltlSimpl $ x `LtlAnd` y) (f ())
-- -- Should we really ignore @p@ here, or should nowLater have another parameter?
-- interpLtl p x (Instr (Builtin g) f) = modifyBuiltin x g f

-- msum $
--   map
--     (\(m, y, q) -> modifyBuiltin m g >>= interpLtl q y . f)
--     (nowLater x)

-- -- tests/examples

-- trace1 :: Staged LtlOp ()
-- trace1 =
--   Instr (EmitInteger 3) $ \_ ->
--     Instr GetInteger $ \i ->
--       Instr (EmitInteger i) Return

-- trace2 :: Staged LtlOp ()
-- trace2 = Instr (EmitInteger 2) $ \_ -> Instr (EmitInteger 5) Return

-- testSomewhere :: [[Integer]]
-- testSomewhere = execWriterT $ interpLtl False (somewhere (LtlAtom $ Just . (* 2))) trace1

-- testEverywhere :: [[Integer]]
-- testEverywhere = execWriterT $ interpLtl True (everywhere (LtlAtom $ Just . (* 2))) trace1

-- testNext :: [[Integer]]
-- testNext = execWriterT $ interpLtl True (LtlNext (LtlAtom $ Just . (* 2))) trace1

-- testAndNext :: [[Integer]]
-- testAndNext =
--   execWriterT $
--     interpLtl
--       True
--       (LtlAtom (Just . (+ 1)) `LtlAnd` LtlNext (LtlAtom f))
--       (trace1 <|> trace2)
--   where
--     f 42 = Just 0
--     f _ = Nothing

-- traceWithModify :: Staged LtlOp ()
-- traceWithModify =
--   Instr (EmitInteger 1) $ \_ ->
--     Instr (ChangeFormula x) $ \_ ->
--       Instr (EmitInteger 2) $ \_ ->
--         Instr (EmitInteger 3) $ \_ ->
--           Instr
--             (EmitInteger 5)
--             Return
--   where
--     x = LtlAtom (Just . (+ 1)) `LtlOr` LtlNext (LtlAtom (Just . (* 3)))

-- testModifyNoContext :: [[Integer]]
-- testModifyNoContext = execWriterT $ interpLtl True LtlTruth traceWithModify

-- testModifyContext :: [[Integer]]
-- testModifyContext = execWriterT $ interpLtl True x traceWithModify
--   where
--     x = LtlNext $ LtlAtom $ Just . (+ 5)

-- tests :: TestTree
-- tests =
--   testGroup
--     "LTL tests"
--     [ testCase "somewhere" $
--         testSomewhere @?= [[6, 42], [3, 84]],
--       testCase "everywhere" $
--         testEverywhere @?= [[6, 84]],
--       testCase "next (together with Alt)" $
--         testNext @?= [[3, 84]],
--       testCase "and-next" $
--         testAndNext @?= [[4, 0]],
--       testCase "Modify in empty context" $
--         testModifyNoContext @?= [[1, 3, 3, 5], [1, 2, 9, 5]],
--       testCase "Modify with context" $
--         testModifyContext @?= [[1, 8, 3, 5], [1, 7, 9, 5]]
--     ]
