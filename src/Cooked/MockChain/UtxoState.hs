-- | This module provides a depiction of the state we return when running a
-- 'Cooked.BlockChain.Direct.MockChain'.
module Cooked.MockChain.UtxoState
  ( UtxoState (..),
    UtxoPayloadSet (..),
    UtxoPayload (..),
    UtxoPayloadDatum (..),
    holdsInState,
  )
where

import Cooked.Skeleton.Datum
import Data.Function (on)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Plutus.Script.Utils.Address qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api

-- | A description of who owns what in a blockchain. Owners are addresses and
-- they each own a 'UtxoPayloadSet'.
data UtxoState where
  UtxoState ::
    { -- | Utxos available to be consumed
      availableUtxos :: Map Api.Address UtxoPayloadSet,
      -- | Utxos already consumed
      consumedUtxos :: Map Api.Address UtxoPayloadSet
    } ->
    UtxoState
  deriving (Eq)

-- | Total value accessible to what's pointed by the address.
holdsInState :: (Script.ToAddress a) => a -> UtxoState -> Api.Value
holdsInState (Script.toAddress -> address) = maybe mempty utxoPayloadSetTotal . Map.lookup address . availableUtxos

instance Semigroup UtxoState where
  (UtxoState a c) <> (UtxoState a' c') = UtxoState (Map.unionWith (<>) a a') (Map.unionWith (<>) c c')

instance Monoid UtxoState where
  mempty = UtxoState Map.empty Map.empty

-- | Represents a /set/ of payloads.
newtype UtxoPayloadSet = UtxoPayloadSet
  { -- | List of UTxOs contained in this 'UtxoPayloadSet'
    utxoPayloadSet :: [UtxoPayload]
    -- We use a list instead of a set because 'Api.Value' doesn't implement 'Ord'
    -- and because it is possible that we want to distinguish between utxo states
    -- that have additional utxos, even if these could have been merged together.
  }
  deriving (Show)

-- | A simplified version of a 'Cooked.Skeleton.Datum.TxSkelOutDatum' which only
-- stores the actual datum and whether it is hashed or inline.
data UtxoPayloadDatum where
  NoUtxoPayloadDatum :: UtxoPayloadDatum
  SomeUtxoPayloadDatum :: (DatumConstrs dat) => dat -> Bool -> UtxoPayloadDatum

deriving instance Show UtxoPayloadDatum

instance Ord UtxoPayloadDatum where
  compare NoUtxoPayloadDatum NoUtxoPayloadDatum = EQ
  compare NoUtxoPayloadDatum _ = LT
  compare _ NoUtxoPayloadDatum = GT
  compare
    (SomeUtxoPayloadDatum (Api.toBuiltinData -> dat) b)
    (SomeUtxoPayloadDatum (Api.toBuiltinData -> dat') b') =
      compare (dat, b) (dat', b')

instance Eq UtxoPayloadDatum where
  dat == dat' = compare dat dat' == EQ

-- | A convenient wrapping of the interesting information of a UTxO.
data UtxoPayload where
  UtxoPayload ::
    { -- \| The reference of this UTxO
      utxoPayloadTxOutRef :: Api.TxOutRef,
      -- \| The value stored in this UTxO
      utxoPayloadValue :: Api.Value,
      -- \| The optional datum stored in this UTxO
      utxoPayloadDatum :: UtxoPayloadDatum,
      -- \| The optional reference script stored in this UTxO
      utxoPayloadReferenceScript :: Maybe Api.ScriptHash
    } ->
    UtxoPayload
  deriving (Eq, Show)

instance Eq UtxoPayloadSet where
  (UtxoPayloadSet xs) == (UtxoPayloadSet ys) = xs' == ys'
    where
      k (UtxoPayload ref val dat rs) = (ref, Api.flattenValue val, dat, rs)
      xs' = List.sortBy (compare `on` k) xs
      ys' = List.sortBy (compare `on` k) ys

instance Semigroup UtxoPayloadSet where
  UtxoPayloadSet a <> UtxoPayloadSet b = UtxoPayloadSet $ a ++ b

instance Monoid UtxoPayloadSet where
  mempty = UtxoPayloadSet []

-- | Computes the total value in a set
utxoPayloadSetTotal :: UtxoPayloadSet -> Api.Value
utxoPayloadSetTotal = mconcat . fmap utxoPayloadValue . utxoPayloadSet
