{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Cooked.MockChain.Play where

import Control.Monad
import Control.Monad.Writer
import Data.Maybe
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
-- (monadic) computation into a list of ("do now", "do later") pairs, where
-- * "do now" is the concrete modification to be applied to the current time
--   step, and
-- * "do later" is an LTL formula describing the modification that should be
--   applied at the next time step.
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
  -- the formula that should be used to modify the next time step.
  [(a, Ltl a)]
deriv t _ LtlTruth = [(t, LtlTruth)]
deriv _ _ LtlFalsity = []
deriv _ _ (LtlAtom g) = [(g, LtlTruth)]
deriv t p (a `LtlOr` b) = deriv t p a ++ deriv t p b
deriv t p (a `LtlAnd` b) =
  [ (g `p` f, ltlSimpl $ c `LtlAnd` d)
    | (f, c) <- deriv t p a,
      (g, d) <- deriv t p b
  ]
deriv t _ (LtlNext a) = [(t, a)]
deriv t p (a `LtlUntil` b) =
  deriv t p b
    ++ [ (f, ltlSimpl $ c `LtlAnd` (a `LtlUntil` b))
         | (f, c) <- deriv t p a
       ]
deriv t p (a `LtlRelease` b) =
  [ (g `p` f, ltlSimpl $ c `LtlAnd` d)
    | (f, c) <- deriv t p b,
      (g, d) <- (t, a `LtlRelease` b) : deriv t p a
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

--------- EXAMPLE
-- interpLtl (LtlNext (Just . (+1))) (EmitString "a" >> EmitInteger 1 >> EmitInteger 42)
-- ~~
-- EmitString "a" >> EmitInteger 1 >> EmitInteger   43

data Staged (op :: * -> *) :: * -> * where
  Return :: a -> Staged op a
  Instr :: op a -> (a -> Staged op b) -> Staged op b

data Op a where
  GetInteger :: Op Integer
  EmitInteger :: Integer -> Op ()

-- instance Show (Op a) where
--   show (EmitString s) = "EmitString \"" ++ s ++ "\""
--   show (EmitInteger i) = "EmitInteger " ++ show i

type Action = Integer

interpOp :: Monad m => (Action -> Maybe Action) -> Op a -> WriterT [Integer] m a
interpOp _ GetInteger = return 42
interpOp g (EmitInteger i) = tell [i']
  where
    i' = fromMaybe i (g i)

interpLtl :: (MonadPlus m) => Ltl (Action -> Maybe Action) -> Staged Op a -> WriterT [Integer] m a
interpLtl x (Return a) = case ltlSimpl x of
  LtlTruth -> return a
  _ -> mzero
interpLtl x (Instr o f) =
  if isAction o
    then
      msum $
        map
          (\(m, y) -> interpOp m o >>= interpLtl y . f)
          (deriv Just (\g h -> maybe Nothing g . h) x)
    else interpOp Just o >>= interpLtl x . f
  where
    isAction :: Op a -> Bool
    isAction (EmitInteger _) = True
    isAction _ = False

test :: [[Integer]]
test = execWriterT $
  interpLtl (everywhere (LtlAtom $ Just . (* 2))) $
    Instr (EmitInteger 3) $
      \_ ->
        Instr GetInteger $
          \i -> Instr (EmitInteger i) Return
