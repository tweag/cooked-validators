module Cooked.Skeleton.Value
  ( TxSkelOutValue (..),
    txSkelOutValueContentL,
    txSkelOutValueAutoAdjustL,
  )
where

import Cooked.Conversion.ToValue
import Optics.TH (makeLensesFor)
import PlutusLedgerApi.V3 qualified as Api

data TxSkelOutValue where
  TxSkelOutValue ::
    { -- Value to be paid
      txSkelOutValueContent :: Api.Value,
      -- Whether this value should be subject to automated adjustment in case it
      -- is not sufficient to sustain the cost of the UTxO containing it.
      txSkelOutValueAutoAdjust :: Bool
    } ->
    TxSkelOutValue
  deriving (Show, Eq)

instance ToValue TxSkelOutValue where
  toValue = txSkelOutValueContent

makeLensesFor
  [ ("txSkelOutValueContent", "txSkelOutValueContentL"),
    ("txSkelOutValueAutoAdjust", "txSkelOutValueAutoAdjustL")
  ]
  ''TxSkelOutValue
