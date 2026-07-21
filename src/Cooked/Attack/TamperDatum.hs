-- | This module provides an attack that modifies the datums of a 'TxSkel'.
module Cooked.Attack.TamperDatum
  ( -- * Tamper datum params
    TamperDatumParams (..),
    allTamperDatumParams,
    overloadTamperDatumParams,

    -- * Tamper datum label
    TamperDatumLabel (..),

    -- * Tamper datum attack
    tamperDatumAttack,
  )
where

import Control.Applicative
import Cooked.Pretty.Class
import Cooked.Skeleton
import Cooked.Tweak
import Optics.Core
import PlutusCore.Data qualified as PLC
import PlutusTx qualified
import Polysemy
import Polysemy.NonDet

-- | A label added to a 'TxSkel' on which a tweak tampering a datum has been
-- applied. The label contains all the datum contents that have been
-- modified, before the modification was applied.
newtype TamperDatumLabel a = TamperDatumLabel [a]
  deriving (Show, Eq, Ord)

instance (PrettyCooked a) => PrettyCooked (TamperDatumLabel a) where
  prettyCookedOpt opts (TamperDatumLabel dats) =
    prettyItemize opts "Tampered Datums" "-" dats

-- | Parameters of the tamper datum attack
data TamperDatumParams a b f k is
  = TamperDatumParams
  { -- | The branching policy to use when several datums are targeted
    tdpBranching :: Branching,
    -- | The optic to use to select eligible 'TxSkelOutDatum'
    tdpOptic :: Optic' k is TxSkel TxSkelOutDatum,
    -- | The modification to apply on targeted datums of type @a@
    tdpModification :: a -> f b,
    -- | The selection function based on the targeted datums indexes
    tdpIndexPred :: Int -> Bool
  }

-- | A tamper datum params where all the datums are considered for targets
allTamperDatumParams ::
  forall a b f.
  Branching ->
  (a -> f b) ->
  TamperDatumParams a b f A_Traversal '[]
allTamperDatumParams branching modif =
  TamperDatumParams
    branching
    (txSkelOutputsL % traversed % txSkelOutDatumL)
    modif
    (const True)

-- | A tamper datum params where the targeted datums are overloaded with dummy
-- extra data @I 42@ at the end of their @BuiltinData@ representation. This only
-- works if the root data is either a @Constr@ or a @List@.
overloadTamperDatumParams ::
  forall k is.
  Branching ->
  Optic' k is TxSkel TxSkelOutDatum ->
  (Int -> Bool) ->
  TamperDatumParams PlutusTx.BuiltinData PlutusTx.BuiltinData Maybe k is
overloadTamperDatumParams branching optic =
  TamperDatumParams
    branching
    optic
    ( \(PlutusTx.builtinDataToData -> bData) -> case bData of
        PLC.Constr i dat -> Just $ PlutusTx.dataToBuiltinData $ PLC.Constr i $ dat <> [PLC.I 42]
        PLC.List l -> Just $ PlutusTx.dataToBuiltinData $ PLC.List $ l <> [PLC.I 42]
        _ -> Nothing
    )

-- | Applies a modification to all datums of type @a@ focused by a given
-- optic. Returns the list of modified datums, as they were before being
-- modified.
tamperDatumAttack ::
  forall a b f k is effs.
  ( DatumConstrs a,
    Ord a,
    DatumConstrs b,
    Foldable f,
    Alternative f,
    Is k A_Traversal,
    Members '[NonDet, Tweak] effs
  ) =>
  TamperDatumParams a b f k is ->
  Sem effs [a]
tamperDatumAttack TamperDatumParams {..} = do
  modified <-
    modifyTweakFromParams $
      ModifyTweakParams tdpBranching tdpOptic txSkelOutDatumTypedAT tdpModification tdpIndexPred
  addLabelTweak $ TamperDatumLabel modified
  return modified
