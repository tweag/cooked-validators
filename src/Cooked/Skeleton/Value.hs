{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exposes constructs around the value as it is stored in a
-- 'Cooked.Skeleton.TxSkel'.
module Cooked.Skeleton.Value
  ( TxSkelOutValue (..),
    txSkelOutValueContentL,
    txSkelOutValueAutoAdjustL,
    valueAssetClassAmountL,
    txSkelOutValueAssetClassAmountL,
  )
where

import Optics.Core
import Optics.TH (makeLensesFor)
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusTx.AssocMap qualified as PMap
import PlutusTx.List qualified as PlutusTx

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

-- | A lens to get or set the amount of tokens of a certain 'Api.AssetClass'
-- from a given 'Api.Value'. This removes the entry if the new amount is 0.
valueAssetClassAmountL :: (Script.ToMintingPolicyHash mp) => mp -> Api.TokenName -> Lens' Api.Value Integer
valueAssetClassAmountL (Script.toCurrencySymbol -> cs) tk =
  lens
    (`Api.assetClassValueOf` Api.assetClass cs tk)
    ( \v@(Api.Value val) i -> case PMap.lookup cs val of
        -- No previous cs entry, and nothing to add
        Nothing | i == 0 -> v
        -- No previous cs entry, and something to add
        Nothing -> Api.Value $ PMap.insert cs (PMap.singleton tk i) val
        -- A previous cs entry, no previous tk entry, and nothing to add
        Just tokenMap | i == 0, Nothing <- PMap.lookup tk tokenMap -> v
        -- A previous cs and tk entry, which needs to be removed and the whole
        -- cs entry as well because it only contained this tk
        Just tokenMap | i == 0, Just _ <- PMap.lookup tk tokenMap, PlutusTx.length (PMap.toList tokenMap) == 1 -> Api.Value $ PMap.delete cs val
        -- A previous cs and tk entry, which needs to be removed, but the whole
        -- cs entry has other tokens and thus is kept
        Just tokenMap | i == 0 -> Api.Value $ PMap.insert cs (PMap.delete tk tokenMap) val
        -- A previous cs entry, in which we insert the new tk (regarless of
        -- whether the tk was already present).
        Just tokenMap -> Api.Value $ PMap.insert cs (PMap.insert tk i tokenMap) val
    )

-- | A lens to get or set the amount of tokens of a certain 'Api.AssetClass'
-- from a given 'TxSkelOutValue'. The removes the entry if the new amount is 0.
txSkelOutValueAssetClassAmountL :: (Script.ToMintingPolicyHash mp) => mp -> Api.TokenName -> Lens' TxSkelOutValue Integer
txSkelOutValueAssetClassAmountL mp tk = txSkelOutValueContentL % valueAssetClassAmountL mp tk
