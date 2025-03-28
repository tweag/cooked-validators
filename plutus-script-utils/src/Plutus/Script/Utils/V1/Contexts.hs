{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Plutus.Script.Utils.V1.Contexts
  ( module Contexts,
    outputsAt,
    valuePaidTo,
  )
where

import Plutus.Script.Utils.Address (ToAddress (toAddress))
import PlutusLedgerApi.V1 (Value)
import PlutusLedgerApi.V1.Contexts as Contexts hiding (valuePaidTo)
import PlutusTx.Prelude (mconcat, (.), (==))

-- | Get the list of values paid to an address within a 'TxInfo'
{-# INLINEABLE outputsAt #-}
outputsAt :: (ToAddress a) => TxInfo -> a -> [Value]
outputsAt txInfo (toAddress -> addr) = [val | TxOut addr' val _ <- txInfoOutputs txInfo, addr == addr']

-- | Get the total value paid to an address within a 'TxInfo'
{-# INLINEABLE valuePaidTo #-}
valuePaidTo :: (ToAddress a) => TxInfo -> a -> Value
valuePaidTo txInfo = mconcat . outputsAt txInfo
