{-# LANGUAGE TemplateHaskell #-}

-- | This modules provides the infrastructure to modify sequences of
-- transactions using LTL formulaes with atomic modifications. This idea is to
-- describe when to apply certain modifications within a trace.
module Cooked.Ltl
  ( -- * LTL formulas
    Ltl (..),

    -- * LTL combinators
    ltlNot',
    ltlOr',
    ltlAnd',
    ltlNext',
    ltlAny,
    ltlAny',
    ltlAll,
    ltlAll',
    ltlDelay,
    ltlDelay',
    ltlEventually,
    ltlEventually',
    ltlAlways,
    ltlAlways',
    ltlWhenPossible,
    ltlWhenPossible',
    ltlIfPossible,
    ltlIfPossible',
    ltlImplies,
    ltlImplies',
    ltlNever,
    ltlNever',

    -- * LTL Effects
    Requirement (..),
    ModifyGlobally,
    modifyLtl,
    runModifyGlobally,
    ModifyLocally,
    getRequirements,
    runModifyLocally,
  )
where

import Control.Monad
import Data.Functor
import Polysemy
import Polysemy.NonDet
import Polysemy.State

-- | Type of LTL formulas with atomic formulas of type @a@. Think of @a@ as a
-- type of "modifications", then a value of type @Ltl a@ describes where to
-- apply `Requirement`s in a trace.
data Ltl a
  = -- | The modification that always applies but does nothing.
    LtlTruth
  | -- | The modification that never applies (i.e. always fails).
    LtlFalsity
  | -- | The atomic modification, applying at the current time step.
    LtlAtom a
  | -- | Assert that the given formula must not hold at the current time step
    -- i.e. that the appropriate modifications fail.
    LtlNot (Ltl a)
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
    -- begins to hold, which must happen ltlEventually. The following holds:
    --
    -- > a `LtlUntil` b <=> b `LtlOr` (a `LtlAnd` LtlNext (a `LtlUntil` b))
    --
    -- `LtlUntil` could technically be defined as the above formula using
    -- Haskell's laziness, but is left as a constructor to have a counterpart
    -- for `LtlRelease`, which cannot.
    LtlUntil (Ltl a) (Ltl a)
  | -- | Assert that the second formula has to hold up to and including the
    -- point when the first begins to hold; if that ltlNever happens, the second
    -- formula has to remain true forever. View this as dual to 'LtlUntil'. The
    -- following holds:
    --
    -- > a `LtlRelease` b <=> b `LtlAnd` (a `LtlOr` LtlNext (a `LtlRelease` b))
    --
    -- `LtlRelease` needs it own constructor, as it is considered valid on an
    -- empty computation, which the above formula is not in most cases.
    LtlRelease (Ltl a) (Ltl a)
  deriving (Show, Eq, Functor)

-- | Same as `LtlNot`, but first wraps the input in an atomic formula.
ltlNot' :: a -> Ltl a
ltlNot' = LtlNot . LtlAtom

-- | Same as `LtlOr`, but first wraps the inputs in atomic formulas.
ltlOr' :: a -> a -> Ltl a
ltlOr' f1 f2 = LtlOr (LtlAtom f1) (LtlAtom f2)

-- | Same as `LtlAnd`, but first wraps the inputs in atomic formulas.
ltlAnd' :: a -> a -> Ltl a
ltlAnd' f1 f2 = LtlAnd (LtlAtom f1) (LtlAtom f2)

-- | Same as `LtlNext`, but first wraps the input in an atomic formula.
ltlNext' :: a -> Ltl a
ltlNext' = LtlNext . LtlAtom

-- | Produces an Ltl formula which consists of the disjunction of all the
-- formulas in the input list.
ltlAny :: [Ltl a] -> Ltl a
ltlAny = foldr LtlOr LtlFalsity

-- | Same as `ltlAny`, but first wraps the elements in the input list in atomic
-- formulas.
ltlAny' :: [a] -> Ltl a
ltlAny' = ltlAny . map LtlAtom

-- | Produces an Ltl formula which consists of the conjunction of all the
-- formulas in the input list.
ltlAll :: [Ltl a] -> Ltl a
ltlAll = foldr LtlAnd LtlTruth

-- | Same as `ltlAll`, but first wraps the elements in the input list in atomic
-- formulas.
ltlAll' :: [a] -> Ltl a
ltlAll' = ltlAll . map LtlAtom

-- | Produces an Ltl formula which consists of the delay of the input formula by
-- @n@ time steps, if @n > 0@.
ltlDelay :: Integer -> Ltl a -> Ltl a
ltlDelay n | n <= 0 = id
ltlDelay n = LtlNext . ltlDelay (n - 1)

-- | Same as `ltlDelay`, but first wraps the input in an atomic formula.
ltlDelay' :: Integer -> a -> Ltl a
ltlDelay' n = ltlDelay n . LtlAtom

-- | Produces an Ltl formula which ensures the input formula eventually holds.
ltlEventually :: Ltl a -> Ltl a
ltlEventually = LtlUntil LtlTruth

-- | Same as `ltlEventually`, but first wraps the input in an atomic formula.
ltlEventually' :: a -> Ltl a
ltlEventually' = ltlEventually . LtlAtom

-- | Produces an Ltl formula which ensures the input formula always holds.
ltlAlways :: Ltl a -> Ltl a
ltlAlways = LtlRelease LtlFalsity

-- | Same as `ltlAlways`, but first wraps the input in an atomic formula.
ltlAlways' :: a -> Ltl a
ltlAlways' = ltlAlways . LtlAtom

-- | Produces an Ltl formula which either ensure the given formula does not
-- hold, or apply its modifications.
ltlIfPossible :: Ltl a -> Ltl a
ltlIfPossible f = f `LtlOr` LtlNot f

-- | Same as `ltlIfPossible`, but first wraps the input in an atomic formula.
ltlIfPossible' :: a -> Ltl a
ltlIfPossible' = ltlIfPossible . LtlAtom

-- | Produces an Ltl formula which applies a formula whenever possible, while
-- ignoring steps when it is not.
ltlWhenPossible :: Ltl a -> Ltl a
ltlWhenPossible = ltlAlways . ltlIfPossible

-- | Same as `ltlWhenPossible`, but first wraps the input in an atomic formula.
ltlWhenPossible' :: a -> Ltl a
ltlWhenPossible' = ltlWhenPossible . LtlAtom

-- | Produces an Ltl formula ensuring the given formula always fails.
ltlNever :: Ltl a -> Ltl a
ltlNever = ltlAlways . LtlNot

-- | Same as `ltlNever`, but first wraps the input in an atomic formula.
ltlNever' :: a -> Ltl a
ltlNever' = ltlNever . LtlAtom

-- | Produces a formula that succeeds if the first formula does not hold, or if
-- both formulas hold.
ltlImplies :: Ltl a -> Ltl a -> Ltl a
ltlImplies f1 f2 = (f2 `LtlAnd` f1) `LtlOr` LtlNot f1

-- | Same as `ltlImplies` but first wraps the inputs in atomic formulas.
ltlImplies' :: a -> a -> Ltl a
ltlImplies' a1 a2 = LtlAtom a1 `ltlImplies` LtlAtom a2

-- | Simplification procedure for LTL formulas. This function knows how
-- `LtlTruth` and `LtlFalsity` play with negation, conjunction and disjunction
-- and recursively applies this knowledge; it is used to keep the formulas
-- `nowLaterList` generates from growing too wildly. While this function does
-- not compute a normal form per se (as it does not tamper with nested
-- conjunction and disjunction), it does ensure a few properties:
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

-- | Requirements implied by a given `Ltl` formula at a given time step
data Requirement a
  = -- | Apply this modification now
    Apply a
  | -- | Ensure this modification fails now
    EnsureFailure a
  deriving (Show, Eq)

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

-- | An effect to modify a computation with an `Ltl` Formula. The idea is that
-- the formula pinpoints locations where `Requirement`s should be enforced.
data ModifyGlobally a :: Effect where
  ModifyLtl :: Ltl a -> m b -> ModifyGlobally a m b

makeSem ''ModifyGlobally

-- | Running the `ModifyGlobally` effect requires to have access of the current
-- list of `Ltl` formulas, and to have access to an empty computation.
--
-- A new formula is appended at the head of the current list of formula. Then,
-- the actual computation is run, after which the newly added formula must be
-- finished, otherwise the empty computation is returned.
runModifyGlobally ::
  forall modification effs a.
  ( Members
      '[ State [Ltl modification],
         NonDet
       ]
      effs
  ) =>
  Sem (ModifyGlobally modification ': effs) a ->
  Sem effs a
runModifyGlobally =
  interpretH $ \case
    ModifyLtl formula comp -> do
      modify (formula :)
      comp' <- runT comp
      res <- raise $ runModifyGlobally comp'
      formulas <- get
      unless (null formulas) $ do
        guard (finished (head formulas))
        put (tail formulas)
      return res

-- | An effect to request and consume the list of requirements that should be
-- enforced at the current time step.
data ModifyLocally a :: Effect where
  GetRequirements :: ModifyLocally a m [Requirement a]

makeSem ''ModifyLocally

-- | Running the `ModifyLocally` effect requires to have access to the current
-- list of `Ltl` formulas, and to be able to branch.
--
-- The function `nowLaterList` is invoked to fetch the various paths implied by
-- the current formulas, and a branching is performed to explore all of
-- them. The new formulas for next steps are stored, and each path is given the
-- requirements to enforce at the current time step.
runModifyLocally ::
  forall modification effs a.
  ( Members
      '[ State [Ltl modification],
         NonDet
       ]
      effs
  ) =>
  Sem (ModifyLocally modification : effs) a ->
  Sem effs a
runModifyLocally =
  interpret $ \GetRequirements -> do
    modifications <- gets nowLaterList
    msum . (modifications <&>) $ \(now, later) -> put later >> return now
