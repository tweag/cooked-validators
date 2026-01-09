-- | This module exposes a simple notion of a Staged computation (or a freer
-- monad) to be used when modifying mockchain runs with Ltl formulas.
module Cooked.Staged
  ( Staged (..),
    interpStaged,
  )
where

import Control.Monad
import Data.Kind

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

-- | Interprets a staged computation given a interpreter of the builtins
interpStaged :: (Monad m) => (forall a. op a -> m a) -> forall a. Staged op a -> m a
interpStaged _ (Return a) = return a
interpStaged interpBuiltin (Instr op cont) = interpBuiltin op >>= interpStaged interpBuiltin . cont
