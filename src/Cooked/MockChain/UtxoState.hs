-- | This module provides a depiction of the internal state we carry around to
-- emulate the blockchain index. This is mostly useful in the Direct
-- implementation of the MonadBlockChain.
module Cooked.MockChain.UtxoState
  ( UtxoState (..),
    UtxoPayloadSet (..),
    UtxoPayload (..),
    holdsInState,
    SkelContext (..),
    lookupOutput,
  )
where

import Cooked.Output
import Cooked.Skeleton
import Data.Function (on)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Plutus.Script.Utils.Data qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
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

-- | The missing information on a 'TxSkel' that can only be resolved by querying
-- the state of the blockchain.
data SkelContext = SkelContext
  { skelContextTxOuts :: Map Api.TxOutRef Api.TxOut,
    skelContextTxSkelOutDatums :: Map Api.DatumHash TxSkelOutDatum
  }

lookupOutput :: SkelContext -> Api.TxOutRef -> Maybe (Api.TxOut, TxSkelOutDatum)
lookupOutput (SkelContext managedTxOuts managedTxSkelOutDatums) txOutRef = do
  output <- Map.lookup txOutRef managedTxOuts
  datum <- case outputOutputDatum output of
    Api.OutputDatum datum -> Map.lookup (Script.datumHash datum) managedTxSkelOutDatums
    Api.OutputDatumHash datumHash -> Map.lookup datumHash managedTxSkelOutDatums
    Api.NoOutputDatum -> Just TxSkelOutNoDatum
  return (output, datum)
