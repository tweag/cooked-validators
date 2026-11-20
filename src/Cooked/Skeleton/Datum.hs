{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module exposes the datum constructs used in payments of a
-- 'Cooked.Skeleton.TxSkel'. Smart constructors for datums can be found in the
-- 'Cooked.Skeleton.Output.Payable' structure meant to build payments.
module Cooked.Skeleton.Datum
  ( -- * Data types
    DatumConstrs,
    DatumResolved (..),
    DatumKind (..),
    TxSkelOutDatum (..),

    -- * Optics
    datumKindResolvedP,
    txSkelOutDatumKindAT,
    txSkelOutDatumResolvedAT,
    txSkelOutDatumTypedAT,
    txSkelOutDatumDatumAF,
    txSkelOutDatumDatumHashAF,
    txSkelOutDatumOutputDatumG,
    txSkelOutDatumOutputDatumI,

    -- * Datum serialization
    DatumRegistry,
    txSkelOutDatumToJSON,
    txSkelOutDatumFromJSON,
  )
where

import Control.Monad
import Cooked.Pretty.Class
import Cooked.Pretty.Plutus ()
import Cooked.Utilities.Codec
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson.Types (Parser, Value)
import Data.Map qualified as Map
import Data.Typeable (Typeable, cast)
import GHC.Generics (Generic)
import Ledger.Data.CardanoAPI qualified as P.Ledger
import Optics.Core
import Plutus.Script.Utils.Data qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- * Type constraints on datums used in cooked-validators

-- | Type constraints that must be satisfied by the datum content
class (Show datum, PrettyCooked datum, Api.ToData datum, Api.FromData datum, Eq datum, Typeable datum, HasTypeKey datum) => DatumConstrs datum

instance (Show datum, PrettyCooked datum, Api.ToData datum, Api.FromData datum, Eq datum, Typeable datum, HasTypeKey datum) => DatumConstrs datum

-- * Datum kind within a transaction and output

-- | Whether the datum should be resolved in the transaction
data DatumResolved
  = -- | Do not resolve the datum (absent from 'Api.txInfoData')
    NotResolved
  | -- | Resolve the datum (present from 'Api.txInfoData')
    Resolved
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Options on how to include the datum in the transaction
data DatumKind
  = -- | Include the full datum in the UTxO
    Inline
  | -- | Only include the datum hash in the UTxO. Resolve, or do not resolve,
    -- the full datum in the transaction body.
    Hashed DatumResolved
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Builds or retrieves the 'DatumResolved' of a 'DatumKind'
datumKindResolvedP :: Prism' DatumKind DatumResolved
datumKindResolvedP =
  prism
    Hashed
    ( \case
        Inline -> Left Inline
        Hashed resolved -> Right resolved
    )

-- * 'Cooked.Skeleton.TxSkel' datums

-- | Datums to be placed in 'Cooked.Skeleton.TxSkel' outputs, which are either
-- empty, or composed of a datum content and its placement
data TxSkelOutDatum where
  -- | Don't use any datum
  NoTxSkelOutDatum :: TxSkelOutDatum
  -- | Use some datum content with a datum kind
  SomeTxSkelOutDatum :: (DatumConstrs dat) => dat -> DatumKind -> TxSkelOutDatum
  -- | Use some datum hash only
  SomeTxSkelOutDatumHash :: Api.DatumHash -> TxSkelOutDatum

deriving instance Show TxSkelOutDatum

instance Eq TxSkelOutDatum where
  NoTxSkelOutDatum == NoTxSkelOutDatum = True
  (SomeTxSkelOutDatum (Api.toBuiltinData -> dat) b) == (SomeTxSkelOutDatum (Api.toBuiltinData -> dat') b') = (dat, b) == (dat', b')
  (SomeTxSkelOutDatumHash hash) == (SomeTxSkelOutDatumHash hash') = hash == hash'
  _ == _ = False

instance Ord TxSkelOutDatum where
  compare NoTxSkelOutDatum NoTxSkelOutDatum = EQ
  compare NoTxSkelOutDatum _ = LT
  compare _ NoTxSkelOutDatum = GT
  compare
    (SomeTxSkelOutDatum (Api.toBuiltinData -> dat) b)
    (SomeTxSkelOutDatum (Api.toBuiltinData -> dat') b') =
      compare (dat, b) (dat', b')
  compare SomeTxSkelOutDatum {} _ = LT
  compare _ SomeTxSkelOutDatum {} = GT
  compare (SomeTxSkelOutDatumHash hash) (SomeTxSkelOutDatumHash hash') = compare hash hash'

-- * Optics working on 'TxSkelOutDatum'

-- | Focuses on the optional 'DatumKind' of a 'TxSkelOutDatum'
txSkelOutDatumKindAT :: AffineTraversal' TxSkelOutDatum DatumKind
txSkelOutDatumKindAT =
  atraversal
    ( \case
        NoTxSkelOutDatum -> Left NoTxSkelOutDatum
        SomeTxSkelOutDatum _ kind -> Right kind
        SomeTxSkelOutDatumHash _ -> Right (Hashed NotResolved)
    )
    ( flip
        ( \kind -> \case
            NoTxSkelOutDatum -> NoTxSkelOutDatum
            SomeTxSkelOutDatum content _ -> SomeTxSkelOutDatum content kind
            datum@(SomeTxSkelOutDatumHash _) -> datum
        )
    )

-- | Focuses on the optional 'DatumResolved' of a 'TxSkelOutDatum'
txSkelOutDatumResolvedAT :: AffineTraversal' TxSkelOutDatum DatumResolved
txSkelOutDatumResolvedAT = txSkelOutDatumKindAT % datumKindResolvedP

-- | Focuses on the optional typed datum of a 'TxSkelOutDatum'. This is
-- attempted in two ways: first, we try to simply cast the content, and then, if
-- it fails, we serialise the content and then attempt to deserialise it to the
-- right type. This second case is specifically useful when the current content
-- is an 'Api.BuiltinData' itself directly, but it can also be used in the
-- corner case when both types have compatible serialized representation.
txSkelOutDatumTypedAT :: (DatumConstrs a, DatumConstrs b) => AffineTraversal TxSkelOutDatum TxSkelOutDatum a b
txSkelOutDatumTypedAT =
  atraversal
    ( \case
        (SomeTxSkelOutDatum content _) | Just content' <- cast content -> Right content'
        (SomeTxSkelOutDatum content _) | Just content' <- Api.fromBuiltinData $ Api.toBuiltinData content -> Right content'
        dc -> Left dc
    )
    ( flip
        ( \content -> \case
            NoTxSkelOutDatum -> NoTxSkelOutDatum
            SomeTxSkelOutDatum _ kind -> SomeTxSkelOutDatum content kind
            SomeTxSkelOutDatumHash _ -> SomeTxSkelOutDatum content (Hashed NotResolved)
        )
    )

-- | Retrieves the optional 'Api.Datum' of a 'TxSkelOutDatum'
txSkelOutDatumDatumAF :: AffineFold TxSkelOutDatum Api.Datum
txSkelOutDatumDatumAF = txSkelOutDatumTypedAT % to Api.Datum

-- | Retrieves the optional 'Api.DatumHash' of a 'TxSkelOutDatum'
txSkelOutDatumDatumHashAF :: AffineFold TxSkelOutDatum Api.DatumHash
txSkelOutDatumDatumHashAF =
  afolding
    ( \case
        SomeTxSkelOutDatumHash hash -> Just hash
        datum -> P.Ledger.datumHash <$> preview txSkelOutDatumDatumAF datum
    )

-- | Retrieves the 'Api.OutputDatum' of a 'TxSkelOutDatum'
txSkelOutDatumOutputDatumG :: Getter TxSkelOutDatum Api.OutputDatum
txSkelOutDatumOutputDatumG = to Script.toOutputDatum

instance Script.ToOutputDatum TxSkelOutDatum where
  toOutputDatum NoTxSkelOutDatum = Api.NoOutputDatum
  toOutputDatum (SomeTxSkelOutDatum datum Inline) = Api.OutputDatum $ Api.Datum $ Api.toBuiltinData datum
  toOutputDatum (SomeTxSkelOutDatum datum _) = Api.OutputDatumHash $ P.Ledger.datumHash $ Api.Datum $ Api.toBuiltinData datum
  toOutputDatum (SomeTxSkelOutDatumHash hash) = Api.OutputDatumHash hash

-- | An isomorphism betwean our 'TxSkelOutDatum' and Plutus
-- 'Api.OutputDatum'. The existence of this function does not mean that both
-- share the same expressiveness. In only means that there exists a sensible way
-- to convert one into the other, and vice versa.
txSkelOutDatumOutputDatumI :: Iso' TxSkelOutDatum Api.OutputDatum
txSkelOutDatumOutputDatumI =
  iso
    Script.toOutputDatum
    ( \case
        Api.OutputDatum (Api.Datum bData) -> SomeTxSkelOutDatum bData Inline
        Api.NoOutputDatum -> NoTxSkelOutDatum
        Api.OutputDatumHash dHash -> SomeTxSkelOutDatumHash dHash
    )

-- | Internal representation of a 'TxSkelOutDatum' that can be serialized to
-- JSON which turns the existential type into a string representation of the
-- type, and the datum content into a 'Api.BuiltinData'.
data DatumSerialized where
  NoDatumSerialized :: DatumSerialized
  SomeDatumSerialized :: String -> Api.BuiltinData -> DatumKind -> DatumSerialized
  SomeDatumHashSerialized :: Api.DatumHash -> DatumSerialized
  deriving (Generic, ToJSON, FromJSON)

-- | Transforms a 'TxSkelOutDatum' to the internal representation.
serializeDatum :: TxSkelOutDatum -> DatumSerialized
serializeDatum NoTxSkelOutDatum = NoDatumSerialized
serializeDatum (SomeTxSkelOutDatum @datum content kind) =
  SomeDatumSerialized (typeKey @datum) (Api.toBuiltinData content) kind
serializeDatum (SomeTxSkelOutDatumHash hash) =
  SomeDatumHashSerialized hash

-- | A registry for datum types satisfying 'DatumConstrs'.
type DatumRegistry = Registry DatumConstrs

-- | Produces a 'TxSkelOutDatum' from the internal representation, using a
-- 'DatumRegistry' to resolve the datum type.
deserializeDatum :: DatumRegistry -> DatumSerialized -> Parser TxSkelOutDatum
deserializeDatum _ NoDatumSerialized = return NoTxSkelOutDatum
deserializeDatum registry (SomeDatumSerialized typeName bData kind) = do
  Codec @datum <- case Map.lookup typeName registry of
    Nothing -> fail $ "deserializeDatum: unknown datum type: " <> typeName
    Just codec -> return codec
  deserializedDatum <- case Api.fromBuiltinData @datum bData of
    Nothing -> fail $ "deserializeDatum: failed to deserialize datum of type: " <> typeName
    Just d -> return d
  return $ SomeTxSkelOutDatum deserializedDatum kind
deserializeDatum _ (SomeDatumHashSerialized hash) = return $ SomeTxSkelOutDatumHash hash

-- | Serialize a 'TxSkelOutDatum' to JSON.
txSkelOutDatumToJSON :: TxSkelOutDatum -> Value
txSkelOutDatumToJSON = toJSON . serializeDatum

-- | Deserialize a 'TxSkelOutDatum' from JSON, using a 'DatumRegistry' to
-- resolve the datum type.
txSkelOutDatumFromJSON :: DatumRegistry -> Value -> Parser TxSkelOutDatum
txSkelOutDatumFromJSON registry = deserializeDatum registry <=< parseJSON
