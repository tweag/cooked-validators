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

-- * LTL formulas and operations on them

-- | Type of LTL formulas with atomic formulas of type @a@. Think of @a@ as a
-- type of "modifications", then a value of type @Ltl a@ describes where to
-- apply modifications. Since it does not make (obvious) sense to talk of a
-- negated modification or of one modification (possibly in the future) to
-- imply another modification, implication and negation are absent.
data Ltl a
  = LtlTruth
  | LtlFalsity
  | LtlAtom a
  | -- | Disjunction will be interpreted in an "intuitionistic" way, i.e. as
    -- branching into the "timeline" where the left disjunct holds and the one
    -- where the right disjunct holds. In that sense, it is an exclusive or,
    -- as it does not introduce the branch where both disjuncts hold.
    LtlOr (Ltl a) (Ltl a)
  | LtlAnd (Ltl a) (Ltl a)
  | -- | Assert that the given formula holds at the next time step.
    LtlNext (Ltl a)
  | -- | Assert that the first formula holds at least until the second one begins
    -- to hold, which must happen eventually. The formulas
    -- > a `LtlUntil` b
    -- and
    -- > b `LtlOr` (a `LtlAnd` LtlNext (a `LtlUntil` b))
    -- are equivalent.
    LtlUntil (Ltl a) (Ltl a)
  | -- | Assert that the second formula has to be true up to and including the
    -- point when the first one becomes true; if that never happens, the second
    -- formula has to remain true forever. View this as dual to 'LtlUntil'. The
    -- formulas
    -- > a `LtlRelease` b
    -- and
    -- > b `LtlAnd` (a `LtlOr` LtlNext (a `LtlRelease` b))
    -- are equivalent.
    LtlRelease (Ltl a) (Ltl a)
  deriving (Show)

-- | Split an LTL formula that describes a modification of a computation into a
-- list of @(doNow, doLater)@ pairs, where
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
-- and recursively applies this knowledge; it does not do anything "fancy" like
-- computing a normal form It is only used to keep the formulae 'nowLater'
-- generates from growing too wildly.
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

-- * An AST for "reified computations"

-- | The idea is that a value of type @Staged (LtlOp modification builtin) a@
-- describes a set of (monadic) computations that return an @a@ such that
--
-- * every step of the computations that returns a @b@ is reified as a @builtin
--   b@, and
--
-- * every step can be modified by a @modification@.

-- | Operations for computations that can be modified using LTL formulas.
data LtlOp (modification :: *) (builtin :: * -> *) :: * -> * where
  -- | The operation that discards the current time line
  Empty :: LtlOp modification builtin a
  -- | The operation the opens two alternative time lines
  Alt ::
    Staged (LtlOp modification builtin) a ->
    Staged (LtlOp modification builtin) a ->
    LtlOp modification builtin a
  -- | The failing operation
  Fail :: String -> LtlOp modification builtin a
  -- | The operation that introduces an LTL formula that should be used to
  -- modify the following computations. Think of this operation as coming
  -- between time steps and adding (with `LtlAnd`, paper upcoming to explain why
  -- this is the right choice) a new formula to the formula that should be
  -- applied to the next time step.
  StartLtl :: Ltl modification -> LtlOp modification builtin ()
  -- | The operation that stops all modifying LTL formulas currently in
  -- effect. If the formula that is to be applied to the next time step is not
  -- 'finished', this will discard the current time line.
  StopLtl :: LtlOp modification builtin ()
  -- | A builtin operation.
  Builtin :: builtin a -> LtlOp modification builtin a

-- | The freer monad on @op@. We think of this as the AST of a computation with
-- operations of types @op a@.
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

-- * Interpreting the AST

-- | To be a suitable semantic domain for computations modified by LTL formulas,
-- a monad @m@ has to
--
-- * have the right 'builtin' functions, which can be modified by the right
--   'modification's,
--
-- * be a 'MonadPlus', because one LTL formula might yield different modified
--   versions of the computation, and
--
-- * be a 'MonadFail' to interpret the 'Fail' operation.
--
-- This type class only wants us to specify how to interpret the (modified)
-- builtins. In order to do so, it passes around the formula that is to be
-- applied to the next time step in a @StateT@. A common idiom to modify an
-- operation should be this:
--
-- > interpBuiltin op =
-- >  get
-- >    >>= msum
-- >      . map (\(now, later) -> applyModification now op <* put later)
-- >      . nowLater
--
-- (But to write this, @modification@ has to be a 'Monoid' to make 'nowLater'
-- work!) Look at the tests for this module and at
-- "Cooked.MockChain.Monad.Staged" for examples of how to use this type class.
class (MonadPlus m, MonadFail m) => InterpLtl modification builtin m where
  interpBuiltin :: builtin a -> StateT (Ltl modification) m a

-- | Interpret a 'Staged' computation into a suitable domain, using the function
-- 'interpBuiltin' to interpret the builtins.
interpLtl ::
  -- forall modification builtin m a.
  (InterpLtl modification builtin m) =>
  Staged (LtlOp modification builtin) a ->
  StateT (Ltl modification) m a
interpLtl (Return a) = get >>= \x -> if finished x then return a else mzero
interpLtl (Instr Empty _) = mzero
interpLtl (Instr (Alt l r) f) = interpLtl (l >>= f) `mplus` interpLtl (r >>= f)
interpLtl (Instr (StartLtl x) f) = get >>= put . LtlAnd x >>= interpLtl . f
interpLtl (Instr StopLtl f) = do
  x <- get
  if finished x
    then do
      put LtlTruth
      interpLtl $ f ()
    else mzero
-- get >>= \x ->
--   if finished x
--     then put (LtlTruth @modification) >> interpLtl $ f ()
--     else mzero
interpLtl (Instr (Fail msg) _) = fail msg
interpLtl (Instr (Builtin b) f) = interpBuiltin b >>= interpLtl . f

-- * Convenience functions

-- | Introduce an LTL formula to be in effect from now on, modifying the
-- remainder of the computation.
startLtl :: Ltl modification -> Staged (LtlOp modification builtin) ()
startLtl x = Instr (StartLtl x) Return

-- | Stop all LTL formulas currently modifying the computation. If any of them
-- is not yet 'finished', the current time line will fail.
stopLtl :: Staged (LtlOp modification builtin) ()
stopLtl = Instr StopLtl Return

-- | Apply a modification somewhere in the given computation. The modification
-- must apply at least once.
somewhere :: modification -> Staged (LtlOp modification builtin) a -> Staged (LtlOp modification builtin) a
somewhere x tr =
  startLtl (LtlTruth `LtlUntil` LtlAtom x)
    >> tr
    >>= \res ->
      stopLtl
        >> return res

-- | Apply a modification everywhere in the given computation. This is also
-- successful if there are no modifiable time steps.
everywhere :: modification -> Staged (LtlOp modification builtin) a -> Staged (LtlOp modification builtin) a
everywhere x tr =
  startLtl (LtlFalsity `LtlRelease` LtlAtom x)
    >> tr
    >>= \res ->
      stopLtl
        >> return res
