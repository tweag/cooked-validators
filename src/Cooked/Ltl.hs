{-# LANGUAGE DeriveFunctor #-}

-- | This modules provides the infrastructure to modify sequences of
-- transactions using pseudo-LTL formulaes with atomic modifications. This idea
-- is to describe when to apply certain modifications within a trace. This is to
-- be replaced later on with a dependency to https://github.com/tweag/graft.
module Cooked.Ltl
  ( Ltl (..),
    nowLaterList,
    ltlSimpl,
    finished,
    MonadLtl (..),
    Requirement (..),
    interpStagedLtl,
    singletonBuiltin,
    LtlOp (..),
    StagedLtl,
    ModInterpBuiltin (..),
  )
where

import Control.Monad
import Control.Monad.State
import Cooked.Staged
import Data.Functor
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
    -- if the operation on atomic modification is not commutative. In
    -- particular, this is the case for tweaks, where the second modification
    -- will be applied first, to be consistent with nested modifications.
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

-- | Simplification procedure for LTL formulas. This function knows how
-- 'LtlTruth' and 'LtlFalsity' play with negation, conjunction and disjunction
-- and recursively applies this knowledge; it is used to keep the formulas
-- 'nowLater' generates from growing too wildly. While this function does not
-- compute a normal form per se (as it does not tamper with nested conjunction
-- and disjunction), it does ensure a few properties:
--
-- * `LtlNext` is left unchanged
--
-- * `LtlNot` only appears in the resulting formula wrapping up a `LtlAtom`
--
-- * `LtlUntil` and `LtlRelease` are interpreted in terms of other constructs,
--   and thus are never returned.
--
-- * Two `LtlNext` appearing in both sides of an `LtlAnd` and `LtlOr` are
--   merged. Thus a formula of shape @LtlAnd (LtlNext a) (LtlNext b)@ will never
--   be returned, and similarly with @LtlOr@.
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
ltlSimpl (LtlAnd (ltlSimpl -> LtlNext f1) (ltlSimpl -> LtlNext f2)) = LtlNext $ f1 `LtlAnd` f2
ltlSimpl (LtlAnd (ltlSimpl -> f1) (ltlSimpl -> f2)) = LtlAnd f1 f2
ltlSimpl (LtlOr (ltlSimpl -> LtlFalsity) (ltlSimpl -> f2)) = f2
ltlSimpl (LtlOr (ltlSimpl -> f1) (ltlSimpl -> LtlFalsity)) = f1
ltlSimpl (LtlOr (ltlSimpl -> LtlNext f1) (ltlSimpl -> LtlNext f2)) = LtlNext $ f1 `LtlOr` f2
-- We don't perform any reduction when `LtlOr` is applied to `LtlTruth` as
-- we still need to keep both branches, and certainly don't want to discard
-- the branch were potential meaningful modifications need to be applied.
ltlSimpl (LtlOr (ltlSimpl -> f1) (ltlSimpl -> f2)) = LtlOr f1 f2

-- | Requirements implied by a given formula at a given time step
data Requirement a
  = -- | Apply this modification now
    Apply a
  | -- | Ensure this modification fails now
    EnsureFailure a

-- | For each LTL formula that describes a modification of a computation in a
-- list, split it into a list of @(doNow, doLater)@ pairs, and then
-- appropriately combine the results. The result of the splitting is bound to
-- the following semantics:
--
-- * @doNow@ is the list of modifications to be consecutively either applied to
-- the current time step (`Apply`), or that should fail at the current time step
-- (`EnsureFailure`)
--
-- * @doLater@ is an LTL formula describing the modification that should be
-- applied from the next time step onwards.
--
-- The return value is a list because a formula might be satisfied in different
-- ways. For example, the modification described by @a `LtlUntil` b@ might be
-- accomplished by applying the modification @b@ right now, or by applying @a@
-- right now and @a `LtlUntil` b@ from the next step onwards; the returned list
-- will contain these two options.
nowLaterList :: [Ltl a] -> [([Requirement a], [Ltl a])]
nowLaterList =
  foldr
    ( \el acc -> do
        (now, next) <- nowLater $ ltlSimpl el
        (now', nexts) <- acc
        return (now <> now', next : nexts)
    )
    [([], [])]
  where
    nowLater :: Ltl a -> [([Requirement a], Ltl a)]
    nowLater LtlTruth = [([], LtlTruth)]
    nowLater LtlFalsity = [([], LtlFalsity)]
    nowLater (LtlAtom now) = [([Apply now], LtlTruth)]
    nowLater (LtlNext f) = [([], f)]
    nowLater (LtlNot (LtlAtom now)) = [([EnsureFailure now], LtlTruth)]
    nowLater (f1 `LtlOr` f2) = nowLater f1 ++ nowLater f2
    nowLater (f1 `LtlAnd` f2) = do
      (now1, next1) <- nowLater f1
      (now2, next2) <- nowLater f2
      return (now2 <> now1, next2 `LtlAnd` next1)
    -- Only the above cases can occur, as they are outcomes of @ltlSimpl@. This
    -- is handy (and intended), as the remaining cases would lead to complicated
    -- interactions and hard to handle growth in the number of formulas.
    nowLater _ = error "nowLater is always called after ltlSimpl which does not yield more cases."

-- | If there are no more steps and the next step should satisfy the given
-- formula: Are we finished, i.e. was the initial formula satisfied by now?
finished :: Ltl a -> Bool
finished LtlTruth = True
finished LtlFalsity = False
finished (LtlAtom _) = False
finished (LtlAnd f1 f2) = finished f1 && finished f2
finished (LtlOr f1 f2) = finished f1 || finished f2
finished (LtlNext _) = False
finished (LtlUntil _ _) = False
finished (LtlRelease _ _) = True
finished (LtlNot f) = not $ finished f

-- * The `MonadLtl` effect and associated functions

-- | Operations that either allow to use a builtin, or to modify a computation
-- using an @Ltl@ formula.
data LtlOp modification builtin :: Type -> Type where
  WrapLtl :: Ltl modification -> StagedLtl modification builtin a -> LtlOp modification builtin a
  Builtin :: builtin a -> LtlOp modification builtin a

-- | An AST of builtins wrapped into an @Ltl@ setting
type StagedLtl modification builtin = Staged (LtlOp modification builtin)

-- | Building a singleton instruction in a `StagedLtl` monad
singletonBuiltin :: builtin a -> StagedLtl modification builtin a
singletonBuiltin = (`Instr` Return) . Builtin

-- | The effect of being able to modify a computation with an Ltl formula
class (Monad m) => MonadLtl modification m where
  modifyLtl :: Ltl modification -> m a -> m a

instance MonadLtl modification (StagedLtl modification builtin) where
  modifyLtl formula comp = Instr (WrapLtl formula comp) Return

-- | The class that depicts the ability to modify certain builtins and interpret
-- then in a certain domain. Each builtins should either be interpreted directly
-- through @Apply@ or give or way to modify them with @Right@.
class ModInterpBuiltin modification builtin m where
  modifyAndInterpBuiltin ::
    builtin a ->
    Either
      (m a) -- only interpret
      ([Requirement modification] -> m a) -- modify and then interpret

-- | Interpret a staged computation of @Ltl op@ based on an interpretation of
-- @builtin@ with respect to possible modifications. This requires an
-- intermediate interpretation with a state monad, and unfolds as follows:
--
-- * When a builtin is met, which is directly interpreted, we return the
--   associated computation, with no changes to the @Ltl@ state.
--
-- * When a builtin is met, which requires a modification, we return the
--   modified interpretation, and consume the current modification requirements.
--
-- * When a wrapped computation is met, we store the new associated formula, and
--   ensure that when the computation ends, the formula is finished.
interpStagedLtl ::
  forall modification builtin m.
  (MonadPlus m, ModInterpBuiltin modification builtin m) =>
  forall a. StagedLtl modification builtin a -> m a
interpStagedLtl = flip evalStateT [] . go
  where
    go :: forall a. Staged (LtlOp modification builtin) a -> StateT [Ltl modification] m a
    go = interpStaged $ \case
      WrapLtl formula comp -> do
        modify' (formula :)
        res <- go comp
        formulas <- get
        unless (null formulas) $ do
          guard $ finished $ head formulas
          put $ tail formulas
        return res
      Builtin builtin ->
        case modifyAndInterpBuiltin builtin of
          Left comp -> lift comp
          Right applyMod -> do
            modifications <- gets nowLaterList
            msum . (modifications <&>) $
              \(now, later) -> do
                put later
                lift $ applyMod now
