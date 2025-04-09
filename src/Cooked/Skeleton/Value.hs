-- | This module exposes constructs around the value as it is stored in a
-- 'Cooked.Skeleton.TxSkel'.
module Cooked.Skeleton.Value
  ( TxSkelOutValue (..),
    txSkelOutValueContentL,
    txSkelOutValueAutoAdjustL,
  )
where

import Optics.TH (makeLensesFor)
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | A bundle arond an 'Api.Value' to be stored in a
-- 'Cooked.Skeleton.TxSkel'. This bundles offers the possibility to mark a value
-- as adjustable, in case the ADA amount it contains is insufficient to sustain
-- the storage cost of the UTxO containing it.
data TxSkelOutValue where
  TxSkelOutValue ::
    { -- | Value to be paid
      txSkelOutValueContent :: Api.Value,
      -- | Whether this value can be subject to automated adjustment
      txSkelOutValueAutoAdjust :: Bool
    } ->
    TxSkelOutValue
  deriving (Show, Eq)

instance Script.ToValue TxSkelOutValue where
  toValue = txSkelOutValueContent

-- | A lens to get or set the inner value of a 'TxSkelOutValue'
makeLensesFor [("txSkelOutValueContent", "txSkelOutValueContentL")] ''TxSkelOutValue

-- | A lens to get or set if this value should be auto-adjusted if needed
makeLensesFor [("txSkelOutValueAutoAdjust", "txSkelOutValueAutoAdjustL")] ''TxSkelOutValue
