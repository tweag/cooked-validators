{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cooked.MockChain.Ltl where

import Control.Applicative
import Control.Monad
import Control.Monad.State

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

-- | This function splits an LTL formula that describes a modification of a
-- computation into a list of @(doNow, doLater)@ pairs, where
--
-- * @doNow@ is the modification to be applied to the current time step,
--
-- * @doLater@ is an LTL formula describing the modification that should be
--   applied from the next time step onwards, and
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
nowLater :: (Monoid a) => Ltl a -> [(a, Ltl a)]
nowLater LtlTruth = [(mempty, LtlTruth)]
nowLater LtlFalsity = []
nowLater (LtlAtom g) = [(g, LtlTruth)]
nowLater (a `LtlOr` b) = nowLater a ++ nowLater b
nowLater (a `LtlAnd` b) =
  [ (g <> f, ltlSimpl $ c `LtlAnd` d)
    | (f, c) <- nowLater a,
      (g, d) <- nowLater b
  ]
nowLater (LtlNext a) = [(mempty, a)]
nowLater (a `LtlUntil` b) =
  nowLater $ b `LtlOr` (a `LtlAnd` LtlNext (a `LtlUntil` b))
nowLater (a `LtlRelease` b) =
  nowLater $ b `LtlAnd` (a `LtlOr` LtlNext (a `LtlRelease` b))

-- | If there are no more steps and the next step should satisfy the given
-- formula: Are we finished, i.e. was the initial formula satisfied by now?
finished :: Ltl a -> Bool
finished LtlTruth = True
finished LtlFalsity = False --  we want falsity to fail always, even on the empty computation
finished (LtlAtom _) = False
finished (a `LtlAnd` b) = finished a && finished b
finished (a `LtlOr` b) = finished a || finished b
finished (LtlNext _) = False
finished (LtlUntil _ _) = False
finished (LtlRelease _ _) = True

-- | Straightforward simplification procedure for LTL formulae. This function
-- knows how 'LtlTruth' and 'LtlFalsity' play with conjunction and disjunction
-- and recursively applies this knowledge; it does not do anything fancy like
-- normal forms etc. It is only used to keep the formulae 'nowLater' generates
-- from growing too wildly.
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
  StartLtl :: Ltl modification -> LtlOp modification builtin ()
  StopLtl :: LtlOp modification builtin ()
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

startLtl :: Ltl modification -> Staged (LtlOp modification builtin) ()
startLtl x = Instr (StartLtl x) Return

stopLtl :: Staged (LtlOp modification builtin) ()
stopLtl = Instr StopLtl Return

somewhere :: modification -> Staged (LtlOp modification builtin) a -> Staged (LtlOp modification builtin) a
somewhere x tr =
  startLtl (LtlTruth `LtlUntil` LtlAtom x)
    >> tr
    >>= \res ->
      stopLtl
        >> return res

everywhere :: modification -> Staged (LtlOp modification builtin) a -> Staged (LtlOp modification builtin) a
everywhere x tr =
  startLtl (LtlFalsity `LtlRelease` LtlAtom x)
    >> tr
    >>= \res ->
      stopLtl
        >> return res

class (MonadPlus m, MonadFail m) => InterpLtl modification builtin m where
  interpBuiltin :: builtin a -> StateT (Ltl modification) m a

interpLtl ::
  (MonadPlus m, MonadFail m, InterpLtl modification builtin m) =>
  Staged (LtlOp modification builtin) a ->
  StateT (Ltl modification) m a
interpLtl (Return a) = get >>= \x -> if finished x then return a else mzero
interpLtl (Instr Empty _) = mzero
interpLtl (Instr (Alt l r) f) = interpLtl (l >>= f) `mplus` interpLtl (r >>= f)
interpLtl (Instr (StartLtl x) f) = get >>= put . LtlAnd x >>= interpLtl . f
interpLtl (Instr StopLtl f) = get >>= \x -> if finished x then interpLtl $ f () else mzero
interpLtl (Instr (Fail msg) _) = fail msg
interpLtl (Instr (Builtin b) f) = interpBuiltin b >>= interpLtl . f

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
