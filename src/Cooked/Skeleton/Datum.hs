-- | This module exposes the notion of datums as they are handled within a
-- 'Cooked.Skeleton.TxSkel'
module Cooked.Skeleton.Datum
  ( DatumConstrs,
    DatumContent (..),
    datumContentDatumI,
    datumContentDatumHashG,
    DatumResolved (..),
    DatumKind (..),
    TxSkelOutDatum (..),
    txSkelOutDatumHashAF,
    datumContentTypedDatumP,
    txSkelOutDatumAT,
    txSkelOutDatumContentAT,
    txSkelOutTypedDatumAT,
    txSkelOutDatumKindAT,
    datumContentBuiltinDataI,
    datumKindResolvedP,
    txSkelOutDatumResolvedAT,
    txSkelOutDatumBuiltinDataAT,
  )
where

import Cooked.Pretty.Class
import Cooked.Pretty.Plutus ()
import Data.Typeable (cast)
import Optics.Core
import Plutus.Script.Utils.Data qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.Prelude qualified as PlutusTx
import Type.Reflection

-- * Type constraints on datums used in cooked-validators

-- | Type constraints that must be satisfied by the datum content
type DatumConstrs datum =
  ( Show datum,
    PrettyCooked datum,
    Api.ToData datum,
    Api.FromData datum,
    PlutusTx.Eq datum,
    Typeable datum
  )

-- * Wrapping datums of arbitrary types satisfying 'DatumConstrs'

-- | Data type of wrapped datums satisfying 'DatumConstrs'
data DatumContent where
  -- | Wraps an element satisfying 'DatumConstrs'
  DatumContent :: (DatumConstrs a) => a -> DatumContent

deriving instance Show DatumContent

instance Api.ToData DatumContent where
  toBuiltinData (DatumContent dat) = Api.toBuiltinData dat

-- | Transforms a 'DatumContent' into a 'Api.Datum' and vice versa
datumContentDatumI :: Iso' DatumContent Api.Datum
datumContentDatumI =
  iso
    (Api.Datum . view datumContentBuiltinDataI)
    (\(Api.Datum bData) -> review datumContentBuiltinDataI bData)

-- | Extracts the datum hash from a 'DatumContent'
datumContentDatumHashG :: Getter DatumContent Api.DatumHash
datumContentDatumHashG = datumContentDatumI % to Script.datumHash

-- | Transforms a 'DatumContent' into aV 'Api.BuiltinData' and vice versa
datumContentBuiltinDataI :: Iso' DatumContent Api.BuiltinData
datumContentBuiltinDataI =
  iso
    (\(DatumContent dat) -> Api.toBuiltinData dat)
    DatumContent

-- | Extracts, or sets, the typed datum of a 'DatumContent'. This is attempted
-- in two ways: first, we try to simply cast the content, and then, if it fails,
-- we serialise the content and then attempt to deserialise it to the right
-- type. This second case is specifically useful when the current content is an
-- 'Api.BuiltinData' itself directly, but it can also be used in the cornercase
-- when both types have compatible serialized representation.
datumContentTypedDatumP :: (DatumConstrs a) => Prism' DatumContent a
datumContentTypedDatumP =
  prism
    DatumContent
    ( \case
        (DatumContent content) | Just content' <- cast content -> Right content'
        (DatumContent content) | Just content' <- Api.fromBuiltinData $ Api.toBuiltinData content -> Right content'
        dc -> Left dc
    )

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

-- | Builds a 'DatumKind' from a 'DatumResolved' or optionally retrieves it
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
  -- | use no datum
  TxSkelOutNoDatum :: TxSkelOutDatum
  -- | use some datum content and associated placement
  TxSkelOutSomeDatum :: DatumContent -> DatumKind -> TxSkelOutDatum
  deriving (Eq, Show, Ord)

instance Script.ToOutputDatum TxSkelOutDatum where
  toOutputDatum TxSkelOutNoDatum = Api.NoOutputDatum
  toOutputDatum (TxSkelOutSomeDatum datum Inline) = Api.OutputDatum $ Api.Datum $ Api.toBuiltinData datum
  toOutputDatum (TxSkelOutSomeDatum datum _) = Api.OutputDatumHash $ Script.datumHash $ Api.Datum $ Api.toBuiltinData datum

-- | Extracts or changes the 'DatumContent' of a 'TxSkelOutDatum'
txSkelOutDatumContentAT :: AffineTraversal' TxSkelOutDatum DatumContent
txSkelOutDatumContentAT =
  atraversal
    ( \case
        TxSkelOutNoDatum -> Left TxSkelOutNoDatum
        TxSkelOutSomeDatum content _ -> Right content
    )
    ( flip
        ( \content -> \case
            TxSkelOutNoDatum -> TxSkelOutNoDatum
            TxSkelOutSomeDatum _ kind -> TxSkelOutSomeDatum content kind
        )
    )

-- | Extracts or changes the 'DatumKind' of a 'TxSkelOutDatum'
txSkelOutDatumKindAT :: AffineTraversal' TxSkelOutDatum DatumKind
txSkelOutDatumKindAT =
  atraversal
    ( \case
        TxSkelOutNoDatum -> Left TxSkelOutNoDatum
        TxSkelOutSomeDatum _ kind -> Right kind
    )
    ( flip
        ( \kind -> \case
            TxSkelOutNoDatum -> TxSkelOutNoDatum
            TxSkelOutSomeDatum content _ -> TxSkelOutSomeDatum content kind
        )
    )

-- | Extracts or changes the 'DatumResolved' of a 'TxSkelOutDatum'
txSkelOutDatumResolvedAT :: AffineTraversal' TxSkelOutDatum DatumResolved
txSkelOutDatumResolvedAT = txSkelOutDatumKindAT % datumKindResolvedP

-- | Converts a 'TxSkelOutDatum' into a possible 'Api.Datum'
txSkelOutDatumAT :: AffineTraversal' TxSkelOutDatum Api.Datum
txSkelOutDatumAT = txSkelOutDatumContentAT % datumContentDatumI

-- | Converts a 'TxSkelOutDatum' into a possible 'Api.BuiltinData'
txSkelOutDatumBuiltinDataAT :: AffineTraversal' TxSkelOutDatum Api.BuiltinData
txSkelOutDatumBuiltinDataAT = txSkelOutDatumContentAT % datumContentBuiltinDataI

-- | Converts a 'TxSkelOutDatum' into a possible Plutus datum hash
txSkelOutDatumHashAF :: AffineFold TxSkelOutDatum Api.DatumHash
txSkelOutDatumHashAF = txSkelOutDatumContentAT % datumContentDatumHashG

-- | Extracts or changes the inner typed datum of a 'TxSkelOutDatum'
txSkelOutTypedDatumAT :: (DatumConstrs a) => AffineTraversal' TxSkelOutDatum a
txSkelOutTypedDatumAT = txSkelOutDatumContentAT % datumContentTypedDatumP
