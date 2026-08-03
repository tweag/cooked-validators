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

import Control.Applicative (Alternative)
import Cooked.Pretty.Class
import Cooked.Pretty.Skeleton ()
import Cooked.Skeleton
import Cooked.Tweak
import Data.Kind (Type)
import Data.Typeable
import Optics.Core
import Polysemy
import Polysemy.NonDet

-- | A label added to a 'TxSkel' on which a tweak hijacking a datum has been
-- applied. The label contains all the outputs that have been hijacked, before
-- the hijacking happened.
newtype DatumHijackingLabel = DatumHijackingLabel [TxSkelOut]
  deriving (Show, Eq, Ord)

instance PrettyCooked DatumHijackingLabel where
  prettyCookedOpt opts (DatumHijackingLabel txSkelOutputs) =
    prettyItemize opts "Hijacked outputs" "-" txSkelOutputs

-- | Parameters of the datum hijacking attacks
data DatumHijackingParams owner f k is
  = DatumHijackingParams
  { -- | The branching policy to use when several outputs are targeted
    dhpBranching :: Branching,
    -- | The optic selecting the outputs eligible for redirection, directly from
    -- the skeleton. Only outputs focused by this optic are redirected.
    dhpOptic :: Optic' k is TxSkel TxSkelOut,
    -- | Returns the new owner (embedded in @f@) of a targeted output. The @f@
    -- allows this computation to fail or branch, offering an extra layer of
    -- selection and multiplication on top of 'dhpOptic'.
    dhpNewOwner :: TxSkelOut -> f owner,
    -- | The redirection described by the previous arguments might apply to more
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
  DatumHijackingParams owner Maybe A_Traversal NoIx
defaultDatumHijackingParams branching optic thief =
  DatumHijackingParams
    branching
    (txSkelOutputsL % traversed % selectP (has _Just . preview optic))
    (const (Just thief))
    (const True)

-- | Targets all the outputs satisfying a given predicate
outPredDatumHijackingParams ::
  Branching ->
  (TxSkelOut -> Bool) ->
  owner ->
  DatumHijackingParams owner Maybe A_Traversal NoIx
outPredDatumHijackingParams branching =
  defaultDatumHijackingParams branching . filtered

-- | Datum hijacking parameters targeting all the outputs owned by a certain
-- type of owner.
typedByDatumHijackingParams ::
  forall (oldOwner :: Type) owner.
  (Typeable oldOwner) =>
  Branching ->
  owner ->
  DatumHijackingParams owner Maybe A_Traversal NoIx
typedByDatumHijackingParams branching =
  defaultDatumHijackingParams branching (txSkelOutOwnerL % userTypedAF @oldOwner)

-- | Datum hijacking parameters targeting all the outputs owned by a given
-- user.
ownedByDatumHijackingParams ::
  forall oldOwner owner.
  ( Typeable oldOwner,
    Eq oldOwner
  ) =>
  Branching ->
  oldOwner ->
  owner ->
  DatumHijackingParams owner Maybe A_Traversal NoIx
ownedByDatumHijackingParams branching user =
  defaultDatumHijackingParams branching (txSkelOutOwnerL % userTypedAF @oldOwner % filtered (== user))

-- | Datum hijacking parameters targeting all the outputs owned by a script.
scriptsDatumHijackingParams ::
  Branching ->
  owner ->
  DatumHijackingParams owner Maybe A_Traversal NoIx
scriptsDatumHijackingParams branching =
  defaultDatumHijackingParams branching (txSkelOutOwnerL % userScriptHashAF)

-- | Datum hijacking parameters targeting all the outputs with a certain type
-- of datum.
datumOfDatumHijackingParams ::
  forall dat owner.
  (DatumConstrs dat) =>
  Branching ->
  owner ->
  DatumHijackingParams owner Maybe A_Traversal NoIx
datumOfDatumHijackingParams branching =
  defaultDatumHijackingParams branching (txSkelOutDatumL % txSkelOutDatumTypedAT @dat)

-- | Hijacks all the outputs from which a new owner can be computed, and whose
-- indexes match a given predicate. Returns the list of hijacked outputs, as
-- they were before being hijacked.
datumHijackingAttack ::
  ( Members '[NonDet, Tweak] effs,
    IsTxSkelOutAllowedOwner owner,
    Is k A_Traversal,
    Foldable f,
    Alternative f
  ) =>
  DatumHijackingParams owner f k is ->
  Sem effs [TxSkelOut]
datumHijackingAttack DatumHijackingParams {..} = do
  modified <-
    modifyTweakFromParams $
      ModifyTweakParams
        dhpBranching
        dhpOptic
        simple
        (\out -> (\owner -> set txSkelOutOwnerL (toPKHOrVScript owner) out) <$> dhpNewOwner out)
        dhpIndexPred
  insertInTweak txSkelLabelsL $ TxSkelLabel $ DatumHijackingLabel modified
  return modified
