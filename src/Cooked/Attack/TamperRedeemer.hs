-- | This module provides an attack that modifies the redeemers of a 'TxSkel'.
module Cooked.Attack.TamperRedeemer
  ( -- * Tamper redeemer params
    TamperRedeemerParams (..),
    spendingTamperRedeemerParams,
    mintingTamperRedeemerParams,
    proposingTamperRedeemerParams,
    certifyingTamperRedeemerParams,
    withdrawingTamperRedeemerParams,
    allTamperRedeemerParams,

    -- * Tamper redeemer label
    TamperRedeemerLabel (..),

    -- * Tamper redeemer attack
    tamperRedeemerAttack,
  )
where

import Control.Applicative
import Cooked.Pretty.Class
import Cooked.Skeleton
import Cooked.Tweak
import Optics.Core
import Polysemy
import Polysemy.NonDet

-- | A label added to a 'TxSkel' on which a tweak tampering a redeemer has been
-- applied. The label contains all the redeemer contents that have been
-- modified, before the modification was applied.
newtype TamperRedeemerLabel a = TamperRedeemerLabel [a]
  deriving (Show, Eq, Ord)

instance (PrettyCooked a) => PrettyCooked (TamperRedeemerLabel a) where
  prettyCookedOpt opts (TamperRedeemerLabel reds) =
    prettyItemize opts "Tamper Redeemers" "-" reds

-- | Parameters of the tamper datum attack
data TamperRedeemerParams a b f k is
  = TamperRedeemerParams
  { -- | The branching policy to use when several redeemers are targeted
    trpBranching :: Branching,
    -- | The optic to use to select eligible 'TxSkelRedeemer'
    trpOptic :: Optic' k is TxSkel TxSkelRedeemer,
    -- | The modification to apply on targeted redeemers of type @a@
    trpModification :: a -> f b,
    -- | The selection function based on the targeted redeemers indexes
    trpIndexPred :: Int -> Bool
  }

-- | A tamper redeemer params to apply a modification to all spending redeemers
-- of type @a@.
spendingTamperRedeemerParams ::
  forall a b f.
  Branching ->
  (a -> f b) ->
  TamperRedeemerParams a b f A_Traversal NoIx
spendingTamperRedeemerParams branching mChange =
  TamperRedeemerParams branching txSkelSpendingRedeemersT mChange (const True)

-- | A tamper redeemer params to apply a modification to all minting redeemers
-- of type @a@.
mintingTamperRedeemerParams ::
  forall a b f.
  Branching ->
  (a -> f b) ->
  TamperRedeemerParams a b f A_Traversal NoIx
mintingTamperRedeemerParams branching mChange =
  TamperRedeemerParams branching (txSkelMintingScriptsT % userRedeemerL) mChange (const True)

-- | A tamper redeemer params to apply a modification to all proposing redeemers
-- of type @a@.
proposingTamperRedeemerParams ::
  forall a b f.
  Branching ->
  (a -> f b) ->
  TamperRedeemerParams a b f A_Traversal NoIx
proposingTamperRedeemerParams branching mChange =
  TamperRedeemerParams branching (txSkelProposingScriptsT % userRedeemerL) mChange (const True)

-- | A tamper redeemer params to apply a modification to all withdrawing redeemers
-- of type @a@.
withdrawingTamperRedeemerParams ::
  forall a b f.
  Branching ->
  (a -> f b) ->
  TamperRedeemerParams a b f A_Traversal NoIx
withdrawingTamperRedeemerParams branching mChange =
  TamperRedeemerParams branching (txSkelWithdrawingScriptsT % userRedeemerL) mChange (const True)

-- | A tamper redeemer params to apply a modification to all certifying redeemers
-- of type @a@.
certifyingTamperRedeemerParams ::
  forall a b f.
  Branching ->
  (a -> f b) ->
  TamperRedeemerParams a b f A_Traversal NoIx
certifyingTamperRedeemerParams branching mChange =
  TamperRedeemerParams branching (txSkelCertifyingScriptsT % userRedeemerL) mChange (const True)

-- | A tamper redeemer params to apply a modification to all redeemers of type
-- @a@.
allTamperRedeemerParams ::
  forall a b f.
  Branching ->
  (a -> f b) ->
  TamperRedeemerParams a b f A_Traversal NoIx
allTamperRedeemerParams branching mChange =
  TamperRedeemerParams branching txSkelRedeemersT mChange (const True)

-- | Applies a modification to all redeemers of type @a@ focused by a
-- given optic. Returns the list of modified redeemers, as they were before
-- being modified.
tamperRedeemerAttack ::
  forall a b f k is effs.
  ( RedeemerConstrs a,
    Ord a,
    RedeemerConstrs b,
    Foldable f,
    Alternative f,
    Is k A_Traversal,
    Members '[NonDet, Tweak] effs
  ) =>
  TamperRedeemerParams a b f k is ->
  Sem effs [a]
tamperRedeemerAttack TamperRedeemerParams {..} = do
  modified <-
    modifyTweakFromParams $
      ModifyTweakParams trpBranching trpOptic txSkelRedeemerTypedAT trpModification trpIndexPred
  addLabelTweak $ TamperRedeemerLabel modified
  return modified
