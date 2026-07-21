{-# LANGUAGE AllowAmbiguousTypes #-}

-- | This module provides an attack to try and redirect selectable outputs to a
-- given thief target.
module Cooked.Attack.DatumHijacking
  ( -- * Datum hijacking params
    DatumHijackingParams (..),
    typedByDatumHijackingParams,
    ownedByDatumHijackingParams,
    scriptsDatumHijackingParams,
    defaultDatumHijackingParams,
    datumOfDatumHijackingParams,
    outPredDatumHijackingParams,

    -- * Datum hijacking label
    DatumHijackingLabel (..),

    -- * Datum hijacking attack
    datumHijackingAttack,
  )
where

import Cooked.Pretty.Class
import Cooked.Pretty.Skeleton ()
import Cooked.Skeleton
import Cooked.Tweak
import Data.Kind (Type)
import Data.Typeable
import Optics.Core
import Polysemy
import Polysemy.NonDet

-- | A label added to a 'TxSkel' on which a tweak highjacking a datum has been
-- applied. The label contains all the outputs that have been hijacked, before
-- the hijacking happened.
newtype DatumHijackingLabel = DatumHijackingLabel [TxSkelOut]
  deriving (Show, Eq, Ord)

instance PrettyCooked DatumHijackingLabel where
  prettyCookedOpt opts (DatumHijackingLabel txSkelOutputs) =
    prettyItemize opts "Hijacked outputs" "-" txSkelOutputs

-- | Parameters of the datum hijacking attacks
data DatumHijackingParams owner f
  = DatumHijackingParams
  { -- | The branching policy to use when several outputs are targeted
    dhpBranching :: Branching,
    -- | Return the new owner embedded in @f@
    dhpOutputPred :: TxSkelOut -> f owner,
    -- | The redirection described by the previous argument might apply to more
    -- than one of the outputs of the transaction. Use this predicate to select
    -- which of the redirectable outputs to actually redirect. We count the
    -- redirectable outputs from the left to the right, starting with zero.
    dhpIndexPred :: Int -> Bool
  }

-- | Hijacks all the outputs for which the focus of a given optic exist. Returns
-- the list of hijacked outputs, as they were before being hijacked.
defaultDatumHijackingParams ::
  (Is k An_AffineFold) =>
  Branching ->
  Optic' k is TxSkelOut a ->
  owner ->
  DatumHijackingParams owner Maybe
defaultDatumHijackingParams branching optic thief =
  DatumHijackingParams
    branching
    ((thief <$) . preview optic)
    (const True)

-- | Targets all the outputs satisfying a given predicate
outPredDatumHijackingParams ::
  Branching ->
  (TxSkelOut -> Bool) ->
  owner ->
  DatumHijackingParams owner Maybe
outPredDatumHijackingParams branching =
  defaultDatumHijackingParams branching . filtered

-- | Datum hijacking parameters targetting all the outputs owned by a certain
-- type of owner.
typedByDatumHijackingParams ::
  forall (oldOwner :: Type) owner.
  (Typeable oldOwner) =>
  Branching ->
  owner ->
  DatumHijackingParams owner Maybe
typedByDatumHijackingParams branching =
  defaultDatumHijackingParams branching (txSkelOutOwnerL % userTypedAF @oldOwner)

-- | Datum hijacking parameters targetting all the outputs owner by a given
-- user, and redirecting each of them in a separate transaction.
ownedByDatumHijackingParams ::
  forall oldOwner owner.
  ( Typeable oldOwner,
    Eq oldOwner
  ) =>
  Branching ->
  oldOwner ->
  owner ->
  DatumHijackingParams owner Maybe
ownedByDatumHijackingParams branching user =
  defaultDatumHijackingParams branching (txSkelOutOwnerL % userTypedAF @oldOwner % filtered (== user))

-- | Datum hijacking parameters targetting all the outputs owned by a script,
-- and redirecting each of them in a separate transaction.
scriptsDatumHijackingParams ::
  Branching ->
  owner ->
  DatumHijackingParams owner Maybe
scriptsDatumHijackingParams branching =
  defaultDatumHijackingParams branching (txSkelOutOwnerL % userScriptHashAF)

-- | Datum hijacking parameters targetting all the outputs with a certain type
-- of datum, and redirecting each of them in a separate transaction.
datumOfDatumHijackingParams ::
  forall dat owner.
  (DatumConstrs dat) =>
  Branching ->
  owner ->
  DatumHijackingParams owner Maybe
datumOfDatumHijackingParams branching =
  defaultDatumHijackingParams branching (txSkelOutDatumL % txSkelOutDatumTypedAT @dat)

-- | Hijacks all the outputs from which a new owner can be computed, and whose
-- indexes match a given predicate. Returns the list of hijacked outputs, as
-- they were before being hijacked.
datumHijackingAttack ::
  ( Members '[NonDet, Tweak] effs,
    IsTxSkelOutAllowedOwner owner
  ) =>
  DatumHijackingParams owner Maybe ->
  Sem effs [TxSkelOut]
datumHijackingAttack (DatumHijackingParams branching mChange select) = do
  modified <-
    modifyTweakFromParams $
      ModifyTweakParams
        branching
        (txSkelOutputsL % traversed)
        simple
        (\out -> (\owner -> set txSkelOutOwnerL (toPKHOrVScript owner) out) <$> mChange out)
        select
  addLabelTweak $ DatumHijackingLabel modified
  return modified
