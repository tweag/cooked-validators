{-# LANGUAGE DeriveFunctor #-}

-- | This modules provides the infrastructure to modify sequences of
-- transactions using pseudo-LTL formulaes with atomic modifications. This idea
-- is to describe when to apply certain modifications within a trace. This is to
-- be replaced later on with a dependency to https://github.com/tweag/graft.
module Cooked.Ltl
  ( Ltl (..),
    nowLaterList,
    LtlOp (..),
    Staged (..),
    interpLtl,
    interpLtlAndPruneUnfinished,
    InterpLtl (..),
    MonadModal (..),
  )
where

import Control.Monad
import Control.Monad.State
import Data.Kind

-- * LTL formulas and operations on them

-- | Type of LTL formulas with atomic formulas of type @a@. Think of @a@ as a
-- type of "modifications", then a value of type @Ltl a@ describes where to
-- apply modifications. Since there is no (obvious) semantics for a negated
-- modification or of one modification (possibly in the future) implying another
-- modification, implication and negation are currently absent.
data Ltl a
  = -- | The modification that always applies but does noting
    LtlTruth
  | -- | The modification that never applies (i.e. always fails)
    LtlFalsity
  | -- | The atomic modification, applying at the current time step
    LtlAtom a
  | -- | Disjunction will be interpreted in an "intuitionistic" way, i.e. as
    -- branching into the "timeline" where the left disjunct holds and the one
    -- where the right disjunct holds. In that sense, it is an exclusive or, as
    -- it does not introduce the branch where both disjuncts hold.
    LtlOr (Ltl a) (Ltl a)
  | -- | Conjunction will be interpreted as "apply both modifications".
    -- Attention: The "apply both" operation will be user-defined for atomic
    -- modifications, so that conjunction may for example fail to be commutative
    -- if the operation on atomic modification is not commutative.
    LtlAnd (Ltl a) (Ltl a)
  | -- | Assert that the given formula holds at the next time step.
    LtlNext (Ltl a)
  | -- | Assert that the first formula holds at least until the second one
    -- begins to hold, which must happen eventually. The following holds:
    --
    -- > a `LtlUntil` b <=> b `LtlOr` (a `LtlAnd` LtlNext (a `LtlUntil` b))
    --
    -- `LtlUntil` could technically be defined as the above formula using
    -- Haskell's laziness, but is left as a constructor to have a counterpart
    -- for `LtlRelease`, which cannot.
    LtlUntil (Ltl a) (Ltl a)
  | -- | Assert that the second formula has to hold up to and including the
    -- point when the first begins to hold; if that never happens, the second
    -- formula has to remain true forever. View this as dual to 'LtlUntil'. The
    -- following holds:
    --
    -- > a `LtlRelease` b <=> b `LtlAnd` (a `LtlOr` LtlNext (a `LtlRelease` b))
    --
    -- `LtlRelease` needs it own constructor, as it is considered valid on an
    -- empty computation, which the above formula is not in most cases.
    LtlRelease (Ltl a) (Ltl a)
  | -- | Assert that the given formula must not hold at the current time
    -- step. This will be interpreted as ensuring the appropriate modifications
    -- fail.
    LtlNot (Ltl a)
  deriving (Show, Eq, Functor)

-- | For each LTL formula that describes a modification of a computation in a
-- list, split it into a list of @(doNow, mustFailNow, doLater)@ triplets, and
-- then appropriately combine the results. The result of the splitting is bound
-- to the following semantics:
--
-- * @doNow@ is the list of modifications to be consecutively applied to the
-- * current time step,
--
-- * @mustFailNow@ is the list of modifications that each must fail when applied
-- * to the current time step, and
--
-- * @doLater@ is an LTL formula describing the modification that should be
--   applied from the next time step onwards.
--
-- The return value is a list because a formula might be satisfied in different
-- ways. For example, the modification described by @a `LtlUntil` b@ might be
-- accomplished by applying the modification @b@ right now, or by applying @a@
-- right now and @a `LtlUntil` b@ from the next step onwards; the returned list
-- will contain these two options.
nowLaterList :: [Ltl a] -> [([a], [a], [Ltl a])]
nowLaterList =
  foldr
    ( \el acc -> do
        (toApply, toFail, next) <- nowLater $ ltlSimpl el
        (toApply', toFail', nexts) <- acc
        return (toApply <> toApply', toFail <> toFail', next : nexts)
    )
    [([], [], [])]
  where
    nowLater :: Ltl a -> [([a], [a], Ltl a)]
    nowLater LtlTruth = [([], [], LtlTruth)]
    nowLater LtlFalsity = [([], [], LtlFalsity)]
    nowLater (LtlAtom now) = [([now], [], LtlTruth)]
    nowLater (LtlNext f) = [([], [], f)]
    nowLater (LtlNot (LtlAtom now)) = [([], [now], LtlTruth)]
    nowLater (f1 `LtlOr` f2) = nowLater f1 ++ nowLater f2
    nowLater (f1 `LtlAnd` f2) = do
      (toApply1, toFail1, next1) <- nowLater f1
      (toApply2, toFail2, next2) <- nowLater f2
      return (toApply1 <> toApply2, toFail1 <> toFail2, next1 `LtlAnd` next2)
    nowLater _ = error "nowLater is always called after ltlSimpl which does not yield more cases."

    -- Straightforward simplification procedure for LTL formulas. This function
    -- knows how 'LtlTruth' and 'LtlFalsity' play with negation, conjunction and
    -- disjunction and recursively applies this knowledge; it is used to keep
    -- the formulas 'nowLater' generates from growing too wildly.
    ltlSimpl :: Ltl a -> Ltl a
    ltlSimpl (LtlAtom a) = LtlAtom a
    ltlSimpl LtlTruth = LtlTruth
    ltlSimpl LtlFalsity = LtlFalsity
    ltlSimpl (LtlNext f) = LtlNext f
    ltlSimpl (LtlRelease f1 f2) = ltlSimpl $ f2 `LtlAnd` (f1 `LtlOr` LtlNext (f1 `LtlRelease` f2))
    ltlSimpl (LtlUntil f1 f2) = ltlSimpl $ f2 `LtlOr` (f1 `LtlAnd` LtlNext (f1 `LtlUntil` f2))
    ltlSimpl (LtlNot (ltlSimpl -> LtlTruth)) = LtlFalsity
    ltlSimpl (LtlNot (ltlSimpl -> LtlFalsity)) = LtlTruth
    ltlSimpl (LtlNot (ltlSimpl -> LtlNot f)) = f
    ltlSimpl (LtlNot (ltlSimpl -> LtlAnd f1 f2)) = ltlSimpl $ LtlNot f1 `LtlOr` LtlNot f2
    ltlSimpl (LtlNot (ltlSimpl -> LtlOr f1 f2)) = ltlSimpl $ LtlNot f1 `LtlAnd` LtlNot f2
    ltlSimpl (LtlNot (ltlSimpl -> LtlNext f)) = LtlNext (LtlNot f)
    -- The following will never occur, as `ltlSimpl` never returns something of
    -- the shape `LtlUntil` or `LtlRelease`
    ltlSimpl (LtlNot (ltlSimpl -> f)) = LtlNot f
    ltlSimpl (LtlAnd (ltlSimpl -> LtlFalsity) _) = LtlFalsity
    ltlSimpl (LtlAnd _ (ltlSimpl -> LtlFalsity)) = LtlFalsity
    ltlSimpl (LtlAnd (ltlSimpl -> LtlTruth) (ltlSimpl -> f2)) = f2
    ltlSimpl (LtlAnd (ltlSimpl -> f1) (ltlSimpl -> LtlTruth)) = f1
    ltlSimpl (LtlAnd (ltlSimpl -> f1) (ltlSimpl -> f2)) = LtlAnd f1 f2
    ltlSimpl (LtlOr (ltlSimpl -> LtlFalsity) (ltlSimpl -> f2)) = f2
    ltlSimpl (LtlOr (ltlSimpl -> f1) (ltlSimpl -> LtlFalsity)) = f1
    -- We don't perform any reduction when `LtlOr` is applied to `LtlTruth` as
    -- we still need to keep both branches, and certainly don't want to discard
    -- the branch were potential meaningful modifications need to be applied.
    ltlSimpl (LtlOr (ltlSimpl -> f1) (ltlSimpl -> f2)) = LtlOr f1 f2

-- | If there are no more steps and the next step should satisfy the given
-- formula: Are we finished, i.e. was the initial formula satisfied by now?
finished :: Ltl a -> Bool
finished LtlTruth = True
finished LtlFalsity = False --  we want falsity to fail always, even on the empty computation
finished (LtlAtom _) = False
finished (LtlAnd f1 f2) = finished f1 && finished f2
finished (LtlOr f1 f2) = finished f1 || finished f2
finished (LtlNext _) = False
finished (LtlUntil _ _) = False
finished (LtlRelease _ _) = True
finished (LtlNot f) = not $ finished f

-- * An AST for "reified computations"

-- | The idea is that a value of type @Staged (LtlOp modification builtin) a@
-- describes a set of (monadic) computations that return an @a@ such that
--
-- * every step of the computations that returns a @b@ is reified as a @builtin
--   b@, and
--
-- * every step can be modified by a @modification@.

-- | Operations for computations that can be modified using LTL formulas.
data LtlOp (modification :: Type) (builtin :: Type -> Type) :: Type -> Type where
  -- | The operation that introduces a new LTL formula that should be used to
  -- modify the following computations. Think of this operation as coming
  -- between time steps and adding a new formula to be applied before all of the
  -- formulas that should already be applied to the next time step.
  StartLtl :: Ltl modification -> LtlOp modification builtin ()
  -- | The operation that removes the last LTL formula that was introduced. If
  -- the formula is not yet finished, the current time line will fail.
  StopLtl :: LtlOp modification builtin ()
  Builtin :: builtin a -> LtlOp modification builtin a

-- | The freer monad on @op@. We think of this as the AST of a computation with
-- operations of types @op a@.
data Staged (op :: Type -> Type) :: Type -> Type where
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

-- * Interpreting the AST

-- | To be a suitable semantic domain for computations modified by LTL formulas,
-- a monad @m@ has to
--
-- * have the right @builtin@ functions, which can be modified by the right
--   @modification@s,
--
-- * be a 'MonadPlus', because one LTL formula might yield different modified
--   versions of the computation, and
--
-- This type class only requires from the user to specify how to interpret the
-- (modified) builtins. In order to do so, it passes around the formulas that
-- are to be applied to the next time step in a @StateT@. A common idiom to
-- modify an operation should be this:
--
-- > interpBuiltin op =
-- >  get
-- >    >>= msum
-- >      . map (\(now, later) -> applyModification now op <* put later)
-- >      . nowLaterList
--
-- (But to write this, @modification@ has to be a 'Monoid' to make
-- 'nowLaterList' work!) Look at the tests for this module and at
-- "Cooked.MockChain.Monad.Staged" for examples of how to use this type class.
class (MonadPlus m) => InterpLtl modification builtin m where
  interpBuiltin :: builtin a -> StateT [Ltl modification] m a

-- | Interpret a 'Staged' computation into a suitable domain, using the function
-- 'interpBuiltin' to interpret the builtins.
interpLtl ::
  (InterpLtl modification builtin m) =>
  Staged (LtlOp modification builtin) a ->
  StateT [Ltl modification] m a
interpLtl (Return res) = return res
interpLtl (Instr (StartLtl formula) computation) = do
  modify' (formula :)
  interpLtl $ computation ()
interpLtl (Instr StopLtl f) =
  get >>= \case
    formula : formulas -> do
      guard $ finished formula
      put formulas
      interpLtl $ f ()
    [] -> error "You called 'StopLtl' before 'StartLtl'. This is only possible if you're using internals."
interpLtl (Instr (Builtin b) f) = interpBuiltin b >>= interpLtl . f

-- | Interpret a 'Staged' computation into a suitable domain, using the function
-- 'interpBuiltin' to interpret the builtins. At the end of the computation,
-- prune branches that still have unfinished modifications applied to them.  See
-- the discussion on the regression test case for PRs 110 and 131 in
-- 'StagedSpec.hs' for a discussion on why this function has to exist.
interpLtlAndPruneUnfinished ::
  (InterpLtl modification builtin m) =>
  Staged (LtlOp modification builtin) a ->
  StateT [Ltl modification] m a
interpLtlAndPruneUnfinished computation = do
  res <- interpLtl computation
  mods <- get
  guard $ all finished mods
  return res

-- * Convenience functions

-- Users of this module should never use 'StartLtl' and 'StopLtl' explicitly.
-- Here are some safe-to-use functions that should be used instead. Most
-- functions like the ones below should be defined for the class 'MonadModal'
-- because there might be other possibilities to equip a monad with LTL
-- modifications beside the method above.

-- | Monads that allow modifications with LTL formulas.
class (Monad m) => MonadModal m where
  type Modification m :: Type
  modifyLtl :: Ltl (Modification m) -> m a -> m a

instance MonadModal (Staged (LtlOp modification builtin)) where
  type Modification (Staged (LtlOp modification builtin)) = modification
  modifyLtl formula trace = do
    Instr (StartLtl formula) Return
    res <- trace
    Instr StopLtl Return
    return res
