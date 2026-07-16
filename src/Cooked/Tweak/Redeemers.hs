-- | This module provides 'Tweak's that modify the redeemers of a 'TxSkel'.
--
-- Redeemers occur in five positions of a skeleton: spending (inputs), minting,
-- proposing, withdrawing, and certifying. This module exposes a tweak for each
-- position, a tweak ranging over all positions at once, and a generic
-- optic-based tweak from which they are all derived.
module Cooked.Tweak.Redeemers
  ( -- * Optic-based redeemer modifications
    tamperRedeemersOfTypeTweak,

    -- * Per-position redeemer modifications
    tamperSpendingRedeemersOfTypeTweak,
    tamperMintingRedeemersOfTypeTweak,
    tamperProposingRedeemersOfTypeTweak,
    tamperWithdrawingRedeemersOfTypeTweak,
    tamperCertifyingRedeemersOfTypeTweak,

    -- * All-position redeemer modifications
    tamperAllRedeemersOfTypeTweak,

    -- * Tampered label
    TamperedRedeemerLbl (..),
  )
where

import Control.Applicative
import Cooked.Pretty.Class
import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Labels
import Data.Maybe
import Optics.Core
import Polysemy
import Polysemy.NonDet

-- | A label added to a 'TxSkel' on which a tweak tampering a redeemer has been
-- applied. The label contains all the redeemer contents that have been
-- modified, before the modification was applied.
newtype TamperedRedeemerLbl a = TamperedRedeemerLbl [a]
  deriving (Show, Eq, Ord)

instance (PrettyCooked a) => PrettyCooked (TamperedRedeemerLbl a) where
  prettyCookedOpt opts (TamperedRedeemerLbl reds) =
    prettyItemize opts "Tampered Redeemers" "-" reds

-- | Applies a modification to all redeemers of type @a@ focused by a
-- given optic. Returns the list of modified redeemers, as they were before
-- being modified.
tamperRedeemersOfTypeTweak ::
  forall a b k is t effs.
  ( RedeemerConstrs a,
    Ord a,
    RedeemerConstrs b,
    Members '[NonDet, Tweak] effs,
    Foldable t,
    Alternative t,
    Is k A_Traversal
  ) =>
  -- | An optic focusing the redeemers to consider
  Optic' k is TxSkel TxSkelRedeemer ->
  -- | The modification to attempt on each typed redeemer
  (a -> t b) ->
  Sem effs [TxSkelRedeemer]
tamperRedeemersOfTypeTweak optic mChange = do
  modified <- overModsTweakAll optic $ embedTypeChange txSkelRedeemerTypedAT mChange
  addLabelTweak $ TamperedRedeemerLbl $ fromJust . preview (txSkelRedeemerTypedAT @a) <$> modified
  return modified

-- | Applies a modification to all spending redeemers of type @a@.
-- Returns the list of modified redeemers, as they were before being modified.
tamperSpendingRedeemersOfTypeTweak ::
  ( RedeemerConstrs a,
    RedeemerConstrs b,
    Members '[NonDet, Tweak] effs,
    Foldable t,
    Alternative t,
    Ord a
  ) =>
  (a -> t b) ->
  Sem effs [TxSkelRedeemer]
tamperSpendingRedeemersOfTypeTweak =
  tamperRedeemersOfTypeTweak txSkelSpendingRedeemersT

-- | Applies a modification to all minting redeemers of type @a@.
-- Returns the list of modified redeemers, as they were before being modified.
tamperMintingRedeemersOfTypeTweak ::
  ( RedeemerConstrs a,
    RedeemerConstrs b,
    Members '[NonDet, Tweak] effs,
    Foldable t,
    Alternative t,
    Ord a
  ) =>
  (a -> t b) ->
  Sem effs [TxSkelRedeemer]
tamperMintingRedeemersOfTypeTweak =
  tamperRedeemersOfTypeTweak $ txSkelMintingScriptsT % userRedeemerL

-- | Applies a modification to all proposing redeemers of type @a@.
-- Returns the list of modified redeemers, as they were before being modified.
tamperProposingRedeemersOfTypeTweak ::
  ( RedeemerConstrs a,
    RedeemerConstrs b,
    Members '[NonDet, Tweak] effs,
    Foldable t,
    Alternative t,
    Ord a
  ) =>
  (a -> t b) ->
  Sem effs [TxSkelRedeemer]
tamperProposingRedeemersOfTypeTweak =
  tamperRedeemersOfTypeTweak $ txSkelProposingScriptsT % userRedeemerL

-- | Applies a modification to all withdrawing redeemers of type @a@.
-- Returns the list of modified redeemers, as they were before being modified.
tamperWithdrawingRedeemersOfTypeTweak ::
  ( RedeemerConstrs a,
    RedeemerConstrs b,
    Members '[NonDet, Tweak] effs,
    Foldable t,
    Alternative t,
    Ord a
  ) =>
  (a -> t b) ->
  Sem effs [TxSkelRedeemer]
tamperWithdrawingRedeemersOfTypeTweak =
  tamperRedeemersOfTypeTweak $ txSkelWithdrawingScriptsT % userRedeemerAT

-- | Applies a modification to all certifying redeemers of type @a@.
-- Returns the list of modified redeemers, as they were before being modified.
tamperCertifyingRedeemersOfTypeTweak ::
  ( RedeemerConstrs a,
    RedeemerConstrs b,
    Members '[NonDet, Tweak] effs,
    Foldable t,
    Alternative t,
    Ord a
  ) =>
  (a -> t b) ->
  Sem effs [TxSkelRedeemer]
tamperCertifyingRedeemersOfTypeTweak =
  tamperRedeemersOfTypeTweak $ txSkelCertifyingScriptsT % userRedeemerAT

-- | Applies a modification to all redeemers of type @a@, regardless
-- of their position in the skeleton (spending, minting, proposing, withdrawing
-- or certifying). Returns the list of modified redeemers, as they were before
-- being modified.
tamperAllRedeemersOfTypeTweak ::
  ( RedeemerConstrs a,
    RedeemerConstrs b,
    Members '[NonDet, Tweak] effs,
    Foldable t,
    Alternative t,
    Ord a
  ) =>
  (a -> t b) ->
  Sem effs [TxSkelRedeemer]
tamperAllRedeemersOfTypeTweak =
  tamperRedeemersOfTypeTweak txSkelRedeemersT
