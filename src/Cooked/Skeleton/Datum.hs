-- | This module exposes the notion of datums as they are handled within a
-- 'Cooked.Skeleton.TxSkel'
module Cooked.Skeleton.Datum
  ( TxSkelOutDatumConstrs,
    DatumContent (..),
    datumContentToDatum,
    datumContentToDatumHash,
    datumContentToTypedDatum,
    DatumResolved (..),
    DatumKind (..),
    TxSkelOutDatum (..),
    txSkelOutDatumContent,
    txSkelOutDatumHash,
    txSkelOutTypedDatum,
    txSkelOutUntypedDatum,
  )
where

import Control.Monad
import Cooked.Pretty.Class
import Data.Typeable (cast)
import Plutus.Script.Utils.Data qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Prelude qualified as PlutusTx
import Type.Reflection

-- * Type constraints on datums used in cooked-validators

-- | Type constraints that must be satisfied by the datum content
type TxSkelOutDatumConstrs a =
  ( Show a,
    PrettyCooked a,
    Api.ToData a,
    PlutusTx.Eq a,
    Typeable a
  )

-- * Wrapping datums of arbitrary types satisfying 'TxSkelOutDatumConstrs'

-- | Data type of wrapped datums satisfying 'TxSkelOutDatumConstrs'
data DatumContent where
  -- | Wraps an element satisfying 'TxSkelOutDatumConstrs'
  DatumContent :: (TxSkelOutDatumConstrs a) => a -> DatumContent

deriving instance Show DatumContent

instance Api.ToData DatumContent where
  toBuiltinData (DatumContent dat) = Api.toBuiltinData dat

-- | Extracts the datum from a 'DatumContent'
datumContentToDatum :: DatumContent -> Api.Datum
datumContentToDatum = Api.Datum . Api.toBuiltinData

-- | Extracts the datum hash from a 'DatumContent'
datumContentToDatumHash :: DatumContent -> Api.DatumHash
datumContentToDatumHash = Script.datumHash . datumContentToDatum

-- | Extracts a typed datum from a 'DatumContent'
datumContentToTypedDatum :: (Typeable a) => DatumContent -> Maybe a
datumContentToTypedDatum (DatumContent dat) = cast dat

instance Ord DatumContent where
  compare (DatumContent d1) (DatumContent d2) =
    case compare (SomeTypeRep (typeOf d1)) (SomeTypeRep (typeOf d2)) of
      EQ -> compare (Api.toBuiltinData d1) (Api.toBuiltinData d2)
      a -> a

instance Eq DatumContent where
  d1 == d2 = compare d1 d2 == EQ

-- * Datum placement within a transaction

-- | Whether the datum should be resolved in the transaction
data DatumResolved
  = -- | Do not resolve the datum (absent from 'Api.txInfoData')
    NotResolved
  | -- | Resolve the datum (present from 'Api.txInfoData')
    Resolved
  deriving (Show, Eq, Ord)

-- | Options on how to include the datum in the transaction
data DatumKind
  = -- | Include the full datum in the UTxO
    Inline
  | -- | Only include the datum hash in the UTxO. Resolve, or do not resolve,
    -- the full datum in the transaction body.
    Hashed DatumResolved
  deriving (Show, Eq, Ord)

-- * 'Cooked.Skeleton.TxSkel' datums

-- | Datums to be placed in 'Cooked.Skeleton.TxSkel' outputs, which are either
-- empty, or composed of a datum content and its placement
data TxSkelOutDatum where
  -- | use no datum
  TxSkelOutNoDatum :: TxSkelOutDatum
  -- | use some datum content and associated placement
  TxSkelOutSomeDatum :: DatumContent -> DatumKind -> TxSkelOutDatum
  deriving (Eq, Show, Ord)

instance Script.ToOutputDatum TxSkelOutDatum where
  toOutputDatum TxSkelOutNoDatum = Api.NoOutputDatum
  toOutputDatum (TxSkelOutSomeDatum datum Inline) = Api.OutputDatum $ Api.Datum $ Api.toBuiltinData datum
  toOutputDatum (TxSkelOutSomeDatum datum _) = Api.OutputDatumHash $ Script.datumHash $ Api.Datum $ Api.toBuiltinData datum

-- | Retrieves the 'DatumContent' part of a 'TxSkelOutDatum' when it exists
txSkelOutDatumContent :: TxSkelOutDatum -> Maybe DatumContent
txSkelOutDatumContent TxSkelOutNoDatum = Nothing
txSkelOutDatumContent (TxSkelOutSomeDatum dat _) = Just dat

-- | Converts a 'TxSkelOutDatum' into a possible Plutus datum
txSkelOutUntypedDatum :: TxSkelOutDatum -> Maybe Api.Datum
txSkelOutUntypedDatum = fmap datumContentToDatum . txSkelOutDatumContent

-- | Converts a 'TxSkelOutDatum' into a possible Plutus datum hash
txSkelOutDatumHash :: TxSkelOutDatum -> Maybe Api.DatumHash
txSkelOutDatumHash = fmap datumContentToDatumHash . txSkelOutDatumContent

-- | Attempts to cast the content of this 'TxSkelOutDatum' to a given type
txSkelOutTypedDatum :: (Typeable a) => TxSkelOutDatum -> Maybe a
txSkelOutTypedDatum = txSkelOutDatumContent >=> datumContentToTypedDatum
