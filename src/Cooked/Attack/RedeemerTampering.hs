-- | This module provides an attack that modifies the redeemers of a 'TxSkel'.
module Cooked.Attack.RedeemerTampering
  ( -- * Tamper redeemer params
    RedeemerTamperingParams (..),
    spendingRedeemerTamperingParams,
    mintingRedeemerTamperingParams,
    proposingRedeemerTamperingParams,
    certifyingRedeemerTamperingParams,
    withdrawingRedeemerTamperingParams,
    allRedeemerTamperingParams,

    -- * Tamper redeemer label
    RedeemerTamperingLabel (..),

    -- * Tamper redeemer attack
    redeemerTamperingAttack,
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
newtype RedeemerTamperingLabel a = RedeemerTamperingLabel [a]
  deriving (Show, Eq, Ord)

instance (PrettyCooked a) => PrettyCooked (RedeemerTamperingLabel a) where
  prettyCookedOpt opts (RedeemerTamperingLabel reds) =
    prettyItemize opts "Tamper Redeemers" "-" reds

-- | Parameters of the tamper datum attack
data RedeemerTamperingParams a b f k is
  = RedeemerTamperingParams
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
spendingRedeemerTamperingParams ::
  forall a b f.
  Branching ->
  (a -> f b) ->
  RedeemerTamperingParams a b f A_Traversal NoIx
spendingRedeemerTamperingParams branching mChange =
  RedeemerTamperingParams branching txSkelSpendingRedeemersT mChange (const True)

-- | A tamper redeemer params to apply a modification to all minting redeemers
-- of type @a@.
mintingRedeemerTamperingParams ::
  forall a b f.
  Branching ->
  (a -> f b) ->
  RedeemerTamperingParams a b f A_Traversal NoIx
mintingRedeemerTamperingParams branching mChange =
  RedeemerTamperingParams branching (txSkelMintingRedeemedScriptsT % userRedeemerL) mChange (const True)

-- | A tamper redeemer params to apply a modification to all proposing redeemers
-- of type @a@.
proposingRedeemerTamperingParams ::
  forall a b f.
  Branching ->
  (a -> f b) ->
  RedeemerTamperingParams a b f A_Traversal NoIx
proposingRedeemerTamperingParams branching mChange =
  RedeemerTamperingParams branching (txSkelProposingRedeemedScriptsT % userRedeemerL) mChange (const True)

-- | A tamper redeemer params to apply a modification to all withdrawing redeemers
-- of type @a@.
withdrawingRedeemerTamperingParams ::
  forall a b f.
  Branching ->
  (a -> f b) ->
  RedeemerTamperingParams a b f A_Traversal NoIx
withdrawingRedeemerTamperingParams branching mChange =
  RedeemerTamperingParams branching (txSkelWithdrawingRedeemedUsersT % userEitherScriptP % userRedeemerL) mChange (const True)

-- | A tamper redeemer params to apply a modification to all certifying redeemers
-- of type @a@.
certifyingRedeemerTamperingParams ::
  forall a b f.
  Branching ->
  (a -> f b) ->
  RedeemerTamperingParams a b f A_Traversal NoIx
certifyingRedeemerTamperingParams branching mChange =
  RedeemerTamperingParams branching (txSkelCertifyingRedeemedUsersT % userEitherScriptP % userRedeemerL) mChange (const True)

-- | A tamper redeemer params to apply a modification to all redeemers of type
-- @a@.
allRedeemerTamperingParams ::
  forall a b f.
  Branching ->
  (a -> f b) ->
  RedeemerTamperingParams a b f A_Traversal NoIx
allRedeemerTamperingParams branching mChange =
  RedeemerTamperingParams branching txSkelRedeemersT mChange (const True)

-- | Applies a modification to all redeemers of type @a@ focused by a
-- given optic. Returns the list of modified redeemers, as they were before
-- being modified.
redeemerTamperingAttack ::
  forall a b f k is effs.
  ( RedeemerConstrs a,
    Ord a,
    RedeemerConstrs b,
    Foldable f,
    Alternative f,
    Is k A_Traversal,
    Members '[NonDet, Tweak] effs
  ) =>
  RedeemerTamperingParams a b f k is ->
  Sem effs [a]
redeemerTamperingAttack RedeemerTamperingParams {..} = do
  modified <-
    modifyTweakFromParams $
      ModifyTweakParams trpBranching trpOptic txSkelRedeemerTypedAT trpModification trpIndexPred
  addLabelTweak $ RedeemerTamperingLabel modified
  return modified
