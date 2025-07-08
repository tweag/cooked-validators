-- | This module exposes the notion of datums as they are handled within a
-- 'Cooked.Skeleton.TxSkel'
module Cooked.Skeleton.Datum
  ( DatumConstrs,
    DatumResolved (..),
    DatumKind (..),
    TxSkelOutDatum (..),
    datumKindResolvedP,
    txSkelOutDatumKindAT,
    txSkelOutDatumResolvedAT,
    txSkelOutDatumTypedAT,
    txSkelOutDatumDatumAF,
    txSkelOutDatumDatumHashAF,
    txSkelOutDatumOutputDatum,
  )
where

import Cooked.Pretty.Class
import Cooked.Pretty.Plutus ()
import Data.Typeable (cast)
import Optics.Core
import Plutus.Script.Utils.Data qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Type.Reflection

-- * Type constraints on datums used in cooked-validators

-- | Type constraints that must be satisfied by the datum content
type DatumConstrs datum =
  ( Show datum,
    PrettyCooked datum,
    Api.ToData datum,
    Api.FromData datum,
    Eq datum,
    Typeable datum
  )

-- * Datum kind within a transaction and output

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
  NoTxSkelOutDatum :: TxSkelOutDatum
  -- | use some datum content and associated placement
  SomeTxSkelOutDatum :: (DatumConstrs dat) => dat -> DatumKind -> TxSkelOutDatum

deriving instance Show TxSkelOutDatum

instance Ord TxSkelOutDatum where
  compare NoTxSkelOutDatum NoTxSkelOutDatum = EQ
  compare NoTxSkelOutDatum _ = LT
  compare _ NoTxSkelOutDatum = GT
  compare
    (SomeTxSkelOutDatum (Api.toBuiltinData -> dat) b)
    (SomeTxSkelOutDatum (Api.toBuiltinData -> dat') b') =
      compare (dat, b) (dat', b')

instance Eq TxSkelOutDatum where
  dat == dat' = compare dat dat' == EQ

-- * Optics working on 'TxSkelOutDatum'

-- | Extracts or changes the 'DatumKind' of a 'TxSkelOutDatum'
txSkelOutDatumKindAT :: AffineTraversal' TxSkelOutDatum DatumKind
txSkelOutDatumKindAT =
  atraversal
    ( \case
        NoTxSkelOutDatum -> Left NoTxSkelOutDatum
        SomeTxSkelOutDatum _ kind -> Right kind
    )
    ( flip
        ( \kind -> \case
            NoTxSkelOutDatum -> NoTxSkelOutDatum
            SomeTxSkelOutDatum content _ -> SomeTxSkelOutDatum content kind
        )
    )

-- | Extracts or changes the 'DatumResolved' of a 'TxSkelOutDatum'
txSkelOutDatumResolvedAT :: AffineTraversal' TxSkelOutDatum DatumResolved
txSkelOutDatumResolvedAT = txSkelOutDatumKindAT % datumKindResolvedP

-- | Extracts, or sets, the typed datum of a 'TxSkelOutDatum'. This is attempted
-- in two ways: first, we try to simply cast the content, and then, if it fails,
-- we serialise the content and then attempt to deserialise it to the right
-- type. This second case is specifically useful when the current content is an
-- 'Api.BuiltinData' itself directly, but it can also be used in the cornercase
-- when both types have compatible serialized representation.
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
        )
    )

-- | Converts a 'TxSkelOutDatum' into a possible 'Api.Datum'
txSkelOutDatumDatumAF :: AffineFold TxSkelOutDatum Api.Datum
txSkelOutDatumDatumAF = txSkelOutDatumTypedAT % to Api.Datum

-- | Converts a 'TxSkelOutDatum' into a possible 'Api.DatumHash'
txSkelOutDatumDatumHashAF :: AffineFold TxSkelOutDatum Api.DatumHash
txSkelOutDatumDatumHashAF = txSkelOutDatumDatumAF % to Script.datumHash

-- | Converts a 'TxSkelOutDatum' into an 'Api.OutputDatum'
txSkelOutDatumOutputDatum :: Getter TxSkelOutDatum Api.OutputDatum
txSkelOutDatumOutputDatum = to Script.toOutputDatum

instance Script.ToOutputDatum TxSkelOutDatum where
  toOutputDatum NoTxSkelOutDatum = Api.NoOutputDatum
  toOutputDatum (SomeTxSkelOutDatum datum Inline) = Api.OutputDatum $ Api.Datum $ Api.toBuiltinData datum
  toOutputDatum (SomeTxSkelOutDatum datum _) = Api.OutputDatumHash $ Script.datumHash $ Api.Datum $ Api.toBuiltinData datum
