{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Cooked.Ltl.Structure where
import Control.Monad ((>=>))
import Cooked.Ltl (Staged, interpLtl, LtlOp, InterpLtl)
import Control.Monad.State (execStateT)
import Control.Monad.Trans.Writer (execWriterT, WriterT)

-- The type of modifications of type a in a monad m.
data Monad m => Mod a m =
  Mod String (a -> m a)

-- A type alias to specify a list of modifications over a given state
type Labelled a =
  (a , [String])

-- Any element can be seen as being labelled by an empty list of modifications
toLabelled :: a -> Labelled a
toLabelled = (,[])

-- Labelled modifications
type ModExt a m =
  Labelled a -> m (Labelled a)

instance {-# OVERLAPS #-} Monad m => Semigroup (ModExt a m) where
  a <> b = a >=> b

-- mempty does something in that it encapsulates the value
instance {-# OVERLAPS #-} Monad m => Monoid (ModExt a m) where
  mempty = return

-- Lifting a modification into a labelled modification
lift :: Monad m =>
  Mod a m ->
  ModExt a m
lift (Mod mName mExec) (a , l) = (, mName : l) <$> mExec a
