{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.Attack.Tweak.TamperDatum where

import Cooked.Attack.Tweak.Common
import Cooked.Tx.Constraints.Optics
import Cooked.Tx.Constraints.Type
import qualified Ledger.Typed.Scripts as Pl
import Optics.Core

-- | A tweak that tries to change the datum on 'PaysScript' constraints with a
-- prescribed tampering function, that only applies to datums of a certain type.
--
-- The tweak returns a list of the modified datums, as they were *before* the
-- modification was applied to them.
tamperDatumTweak ::
  forall a.
  (PaysScriptConstrs a) =>
  -- | Use this function to return 'Just' the changed datum, if you want to
  -- perform a change, and 'Nothing', if you want to leave it as-is. All datums
  -- on 'PaysScript' constraints not paying to a validator of type @a@ are never
  -- touched.
  (Pl.DatumType a -> Maybe (Pl.DatumType a)) ->
  Tweak [Pl.DatumType a]
tamperDatumTweak change = do
  unmodified <-
    mkTweak
      (paysScriptConstraintsT % paysScriptConstraintTypeP @a % _3)
      (const change)
  addLabelTweak TamperDatumLbl
  return unmodified

data TamperDatumLbl = TamperDatumLbl deriving (Show, Eq)
