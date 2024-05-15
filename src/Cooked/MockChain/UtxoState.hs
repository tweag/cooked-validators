-- | This module provides a depiction of the internal state we carry around to
-- emulate the blockchain index. This is mostly useful in the Direct
-- implementation of the MonadBlockChain.
module Cooked.MockChain.UtxoState
  ( UtxoState (..),
    UtxoPayloadSet (..),
    UtxoPayload (..),
    holdsInState,
  )
where

import Cooked.Skeleton (TxSkelOutDatum)
import Data.Function (on)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | A description of who owns what in a blockchain. Owners are addresses and
-- they each own a 'UtxoPayloadSet'.
newtype UtxoState = UtxoState {utxoState :: Map Api.Address UtxoPayloadSet}
  deriving (Eq)

-- | Total value accessible to what's pointed by the address.
holdsInState :: Api.Address -> UtxoState -> Api.Value
holdsInState address (UtxoState m) =
  maybe mempty utxoPayloadSetTotal (Map.lookup address m)

instance Semigroup UtxoState where
  (UtxoState a) <> (UtxoState b) = UtxoState $ Map.unionWith (<>) a b

-- | Represents a /set/ of payloads.
newtype UtxoPayloadSet = UtxoPayloadSet
  { utxoPayloadSet :: [UtxoPayload]
  -- We use a list instead of a set because 'Api.Value' doesn't implement 'Ord'
  -- and because it is possible that we want to distinguish between utxo states
  -- that have additional utxos, even if these could have been merged together.
  }
  deriving (Show)

-- | A convenient wrapping of the interesting information of a UTxO.
data UtxoPayload = UtxoPayload
  { utxoPayloadTxOutRef :: Api.TxOutRef,
    utxoPayloadValue :: Api.Value,
    utxoPayloadSkelOutDatum :: TxSkelOutDatum,
    utxoPayloadReferenceScript :: Maybe Api.ScriptHash
  }
  deriving (Eq, Show)

instance Eq UtxoPayloadSet where
  (UtxoPayloadSet xs) == (UtxoPayloadSet ys) = xs' == ys'
    where
      k (UtxoPayload ref val dat rs) = (ref, Script.flattenValue val, dat, rs)
      xs' = List.sortBy (compare `on` k) xs
      ys' = List.sortBy (compare `on` k) ys

instance Semigroup UtxoPayloadSet where
  UtxoPayloadSet a <> UtxoPayloadSet b = UtxoPayloadSet $ a ++ b

instance Monoid UtxoPayloadSet where
  mempty = UtxoPayloadSet []

-- | Computes the total value in a set
utxoPayloadSetTotal :: UtxoPayloadSet -> Api.Value
utxoPayloadSetTotal = mconcat . fmap utxoPayloadValue . utxoPayloadSet
