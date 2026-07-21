{-# LANGUAGE AllowAmbiguousTypes #-}

-- | This module provides an automated attack to try and redirect outputs to a
-- certain target with a similar datum type.
module Cooked.Attack.DatumHijacking
  ( DatumHijackingParams (..),
    DatumHijackingLabel (..),
    datumHijackingAttack,
    typedByDatumHijackingParams,
    ownedByDatumHijackingParams,
    scriptsDatumHijackingParams,
    defaultDatumHijackingParams,
    datumOfDatumHijackingParams,
    outPredDatumHijackingParams,
  )
where

import Control.Applicative
import Cooked.Pretty.Class
import Cooked.Pretty.Skeleton ()
import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Labels
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

-- | Parameters of the datum hijacking attacks. They state precisely which
-- outputs should have their owner changed, wich owner should be assigned, to
-- each of these outputs, and whether several modified outputs should be
-- combined in a single transaction, or instead spread out multiple branches.
data DatumHijackingParams where
  DatumHijackingParams ::
    (IsTxSkelOutAllowedOwner owner, Foldable f, Alternative f) =>
    { -- | Whether all the outputs targetted by the predicates should be
      -- redirected in the same transaction, or one at a time, each in a
      -- distinct transaction.
      dhpBranching :: Branching,
      -- | Return the new owner embedded in @f@
      dhpOutputPred :: TxSkelOut -> f owner,
      -- | The redirection described by the previous argument might apply to more
      -- than one of the outputs of the transaction. Use this predicate to select
      -- which of the redirectable outputs to actually redirect. We count the
      -- redirectable outputs from the left to the right, starting with zero.
      dhpIndexPred :: Int -> Bool
    } ->
    DatumHijackingParams

-- | Hijacks all the outputs for which the focus of a given optic exist. Returns
-- the list of hijacked outputs, as they were before being hijacked.
defaultDatumHijackingParams ::
  ( IsTxSkelOutAllowedOwner owner,
    Is k An_AffineFold
  ) =>
  Branching ->
  Optic' k is TxSkelOut a ->
  owner ->
  DatumHijackingParams
defaultDatumHijackingParams branching optic thief =
  DatumHijackingParams
    branching
    ((thief <$) . preview optic)
    (const True)

-- | Targets all the outputs satisfying a given predicate
outPredDatumHijackingParams ::
  (IsTxSkelOutAllowedOwner owner) =>
  Branching ->
  (TxSkelOut -> Bool) ->
  owner ->
  DatumHijackingParams
outPredDatumHijackingParams branching =
  defaultDatumHijackingParams branching . filtered

-- | Datum hijacking parameters targetting all the outputs owned by a certain
-- type of owner.
typedByDatumHijackingParams ::
  forall (oldOwner :: Type) owner.
  ( IsTxSkelOutAllowedOwner owner,
    Typeable oldOwner
  ) =>
  Branching ->
  owner ->
  DatumHijackingParams
typedByDatumHijackingParams branching =
  defaultDatumHijackingParams branching (txSkelOutOwnerL % userTypedAF @oldOwner)

-- | Datum hijacking parameters targetting all the outputs owner by a given
-- user, and redirecting each of them in a separate transaction.
ownedByDatumHijackingParams ::
  forall oldOwner owner.
  ( IsTxSkelOutAllowedOwner owner,
    Typeable oldOwner,
    Eq oldOwner
  ) =>
  Branching ->
  oldOwner ->
  owner ->
  DatumHijackingParams
ownedByDatumHijackingParams branching user =
  defaultDatumHijackingParams branching (txSkelOutOwnerL % userTypedAF @oldOwner % filtered (== user))

-- | Datum hijacking parameters targetting all the outputs owned by a script,
-- and redirecting each of them in a separate transaction.
scriptsDatumHijackingParams ::
  (IsTxSkelOutAllowedOwner owner) =>
  Branching ->
  owner ->
  DatumHijackingParams
scriptsDatumHijackingParams branching =
  defaultDatumHijackingParams branching (txSkelOutOwnerL % userScriptHashAF)

-- | Datum hijacking parameters targetting all the outputs with a certain type
-- of datum, and redirecting each of them in a separate transaction.
datumOfDatumHijackingParams ::
  forall dat owner.
  ( IsTxSkelOutAllowedOwner owner,
    DatumConstrs dat
  ) =>
  Branching ->
  owner ->
  DatumHijackingParams
datumOfDatumHijackingParams branching =
  defaultDatumHijackingParams branching (txSkelOutDatumL % txSkelOutDatumTypedAT @dat)

-- | Hijacks all the outputs from which a new owner can be computed, and whose
-- indexes match a given predicate. Returns the list of hijacked outputs, as
-- they were before being hijacked.
datumHijackingAttack ::
  (Members '[NonDet, Tweak] effs) =>
  DatumHijackingParams ->
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
