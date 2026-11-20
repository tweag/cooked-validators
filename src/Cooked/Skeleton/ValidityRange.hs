{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module provides a way of accessing lower and upper bounds of
-- transaction validity through optics using @at@ and @ix@.
module Cooked.Skeleton.ValidityRange
  ( ValidityBound (..),
  )
where

import Ledger.Slot qualified as P.Ledger
import Optics.Core
import PlutusLedgerApi.V1.Interval qualified as Api

-- | A type used to index optics within a 'P.Ledger.SlotRange'. This allows the
-- usage of the following optics: @at Lower@, @at Upper@, @ix Lower@ and @ix
-- Upper@ to modify parts of a slot range. When set to @Nothing@, the associated
-- bound of the interval is considered infinite, and otherwise it is considered
-- finite, with closed closure.
data ValidityBound
  = Lower
  | Upper

type instance Index P.Ledger.SlotRange = ValidityBound

type instance IxValue P.Ledger.SlotRange = P.Ledger.Slot

instance Ixed P.Ledger.SlotRange

instance At P.Ledger.SlotRange where
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
