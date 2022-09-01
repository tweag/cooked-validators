{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.Attack.TamperDatum where

import Cooked.Attack.Common
import Cooked.Tx.Constraints.Optics
import Cooked.Tx.Constraints.Type
import qualified Ledger.Typed.Scripts as Pl
import Optics.Core

-- | An attack that tries to change the datum on 'PaysScript' constraints with a
-- prescribed tampering function, that only applies to datums of a certain type.
tamperDatumAttack ::
  forall a.
  (PaysScriptConstrs a) =>
  -- | Use this function to return 'Just' the changed datum, if you want to
  -- perform a change, and 'Nothing', if you want to leave it as-is. All datums
  -- on 'PaysScript' constraints not paying to a validator of type @a@ are never
  -- touched.
  (Pl.DatumType a -> Maybe (Pl.DatumType a)) ->
  Attack [Pl.DatumType a]
tamperDatumAttack change = do
  unmodified <-
    mkAttack
      (paysScriptConstraintsT % paysScriptConstraintTypeP @a % _2)
      (const change)
  addLabelAttack TamperDatumLbl
  return unmodified

data TamperDatumLbl = TamperDatumLbl deriving (Show, Eq)
