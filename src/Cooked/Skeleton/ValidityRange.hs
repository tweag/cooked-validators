{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module provides a way of accessing lower and upper bounds of
-- transaction validity through optics using @at@ and @ix@.
module Cooked.Skeleton.ValidityRange
  ( ValidityBound (..),
  )
where

import Ledger.Slot qualified as Ledger
import Optics.Core
import PlutusLedgerApi.V1.Interval qualified as Api

-- | A type used to index optics within a 'Ledger.SlotRange'. This allow the
-- usage of the following optics: @at Lower@, @at Upper@, @ix Lower@ and @ix
-- Upper@ to modify parts of a slot range. When set to @Nothing@, the associated
-- bound of the interval is considered infinite, and otherwise it is considered
-- finite, with closed closure.
data ValidityBound
  = Lower
  | Upper

type instance Index Ledger.SlotRange = ValidityBound

type instance IxValue Ledger.SlotRange = Ledger.Slot

instance Ixed Ledger.SlotRange

instance At Ledger.SlotRange where
  at Lower =
    lens
      ( \case
          (Api.Interval (Api.LowerBound (Api.Finite val) closure) _) -> Just $ if closure then val else val + 1
          _ -> Nothing
      )
      ( \(Api.Interval _ upperBound) newLowerBound ->
          Api.Interval (Api.LowerBound (maybe Api.NegInf Api.Finite newLowerBound) True) upperBound
      )
  at Upper =
    lens
      ( \case
          (Api.Interval _ (Api.UpperBound (Api.Finite val) closure)) -> Just $ if closure then val else val - 1
          _ -> Nothing
      )
      ( \(Api.Interval lowerBound _) newUpperBound ->
          Api.Interval lowerBound (Api.UpperBound (maybe Api.PosInf Api.Finite newUpperBound) True)
      )
