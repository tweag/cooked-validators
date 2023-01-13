{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.Attack.Tweak.TamperDatum where

import Cooked.Attack.Tweak.Common
import Cooked.Tx.Constraints.Optics
import Cooked.Tx.Constraints.Type
import qualified Ledger.Typed.Scripts as Pl
import Optics.Core
import qualified PlutusTx as Pl
import Type.Reflection

-- | A tweak that tries to change the datum on outputs that go to scripts with a
-- prescribed tampering function, that only applies to datums of a certain type.
--
-- The tweak returns a list of the modified datums, as they were *before* the
-- modification was applied to them.
tamperDatumTweak ::
  forall a m.
  ( MonadTweak m,
    Show a,
    Pl.ToData a,
    Pl.FromData a,
    Typeable a
  ) =>
  -- | Use this function to return 'Just' the changed datum, if you want to
  -- perform a change, and 'Nothing', if you want to leave it as-is. All datums
  -- on outputs not paying to a validator of type @a@ are never touched.
  (a -> Maybe a) ->
  m [a]
tamperDatumTweak change = do
  beforeModification <-
    overMaybeTweak
      ( txSkelOutsL
          % traversed
          % txSkelOutputDatumTypeAT @a
      )
      change
  addLabelTweak TamperDatumLbl
  return beforeModification

data TamperDatumLbl = TamperDatumLbl deriving (Show, Eq, Ord)
