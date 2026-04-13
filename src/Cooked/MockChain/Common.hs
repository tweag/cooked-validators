-- | This module exposes some type aliases common to our MockChain library
module Cooked.MockChain.Common
  ( -- * Type aliases
    Fee,
    CollateralIns,
    Collaterals,
    Utxo,
    Utxos,
  )
where

import Cooked.Skeleton.Output
import Data.Set (Set)
import PlutusLedgerApi.V3 qualified as Api

-- * Type aliases

-- | An alias for Integers used as fees
type Fee = Integer

-- | An alias for sets of utxos used as collateral inputs
type CollateralIns = Set Api.TxOutRef

-- | An alias for optional pairs of collateral inputs and optional return
-- collateral output
type Collaterals = (CollateralIns, Maybe TxSkelOut)

-- | An alias for an output and its reference
type Utxo = (Api.TxOutRef, TxSkelOut)

-- | An alias for lists of `Utxo`
type Utxos = [Utxo]
