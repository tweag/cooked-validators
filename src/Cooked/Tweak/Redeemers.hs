-- | This module provides 'Tweak's that modify the redeemers of a 'TxSkel'.
--
-- Redeemers occur in five positions of a skeleton: spending (inputs), minting,
-- proposing, withdrawing, and certifying. This module exposes a tweak for each
-- position, a tweak ranging over all positions at once, and a generic
-- optic-based tweak from which they are all derived.
module Cooked.Tweak.Redeemers
  ( TamperedRedeemerLbl (..),
    tamperRedeemersTweak,
    tamperSpendingRedeemersTweak,
    tamperMintingRedeemersTweak,
    tamperProposingRedeemersTweak,
    tamperWithdrawingRedeemersTweak,
    tamperCertifyingRedeemersTweak,
    tamperAllRedeemersTweak,
  )
where

import Control.Applicative
import Cooked.Pretty.Class
import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Labels
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
tamperRedeemersTweak ::
  forall a b k is t effs.
  ( RedeemerConstrs a,
    Ord a,
    RedeemerConstrs b,
    Members '[NonDet, Tweak] effs,
    Foldable t,
    Alternative t,
    Is k A_Traversal
  ) =>
  -- | The branching options
  Branching ->
  -- | An optic focusing the redeemers to consider
  Optic' k is TxSkel TxSkelRedeemer ->
  -- | The modification to attempt on each typed redeemer
  (a -> t b) ->
  Sem effs [a]
tamperRedeemersTweak branching optic mChange = do
  modified <-
    modifyTweakFromParams $
      modifyTweakParamsAllIndexes branching optic txSkelRedeemerTypedAT mChange
  addLabelTweak $ TamperedRedeemerLbl modified
  return modified

-- | Applies a modification to all spending redeemers of type @a@.
-- Returns the list of modified redeemers, as they were before being modified.
tamperSpendingRedeemersTweak ::
  ( RedeemerConstrs a,
    RedeemerConstrs b,
    Members '[NonDet, Tweak] effs,
    Foldable t,
    Alternative t,
    Ord a
  ) =>
  Branching ->
  (a -> t b) ->
  Sem effs [a]
tamperSpendingRedeemersTweak branching =
  tamperRedeemersTweak branching txSkelSpendingRedeemersT

-- | Applies a modification to all minting redeemers of type @a@.
-- Returns the list of modified redeemers, as they were before being modified.
tamperMintingRedeemersTweak ::
  ( RedeemerConstrs a,
    RedeemerConstrs b,
    Members '[NonDet, Tweak] effs,
    Foldable t,
    Alternative t,
    Ord a
  ) =>
  Branching ->
  (a -> t b) ->
  Sem effs [a]
tamperMintingRedeemersTweak branching =
  tamperRedeemersTweak branching $ txSkelMintingScriptsT % userRedeemerL

-- | Applies a modification to all proposing redeemers of type @a@.
-- Returns the list of modified redeemers, as they were before being modified.
tamperProposingRedeemersTweak ::
  ( RedeemerConstrs a,
    RedeemerConstrs b,
    Members '[NonDet, Tweak] effs,
    Foldable t,
    Alternative t,
    Ord a
  ) =>
  Branching ->
  (a -> t b) ->
  Sem effs [a]
tamperProposingRedeemersTweak branching =
  tamperRedeemersTweak branching $ txSkelProposingScriptsT % userRedeemerL

-- | Applies a modification to all withdrawing redeemers of type @a@.
-- Returns the list of modified redeemers, as they were before being modified.
tamperWithdrawingRedeemersTweak ::
  ( RedeemerConstrs a,
    RedeemerConstrs b,
    Members '[NonDet, Tweak] effs,
    Foldable t,
    Alternative t,
    Ord a
  ) =>
  Branching ->
  (a -> t b) ->
  Sem effs [a]
tamperWithdrawingRedeemersTweak branching =
  tamperRedeemersTweak branching $ txSkelWithdrawingScriptsT % userRedeemerAT

-- | Applies a modification to all certifying redeemers of type @a@.
-- Returns the list of modified redeemers, as they were before being modified.
tamperCertifyingRedeemersTweak ::
  ( RedeemerConstrs a,
    RedeemerConstrs b,
    Members '[NonDet, Tweak] effs,
    Foldable t,
    Alternative t,
    Ord a
  ) =>
  Branching ->
  (a -> t b) ->
  Sem effs [a]
tamperCertifyingRedeemersTweak branching =
  tamperRedeemersTweak branching $ txSkelCertifyingScriptsT % userRedeemerAT

-- | Applies a modification to all redeemers of type @a@, regardless
-- of their position in the skeleton (spending, minting, proposing, withdrawing
-- or certifying). Returns the list of modified redeemers, as they were before
-- being modified.
tamperAllRedeemersTweak ::
  ( RedeemerConstrs a,
    RedeemerConstrs b,
    Members '[NonDet, Tweak] effs,
    Foldable t,
    Alternative t,
    Ord a
  ) =>
  Branching ->
  (a -> t b) ->
  Sem effs [a]
tamperAllRedeemersTweak branching =
  tamperRedeemersTweak branching txSkelRedeemersT
