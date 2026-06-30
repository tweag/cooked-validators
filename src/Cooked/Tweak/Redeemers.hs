-- | This module provides 'Tweak's that modify the redeemers of a 'TxSkel'.
--
-- Redeemers occur in five positions of a skeleton: spending (inputs), minting,
-- proposing, withdrawing, and certifying. This module exposes a tweak for each
-- position, a tweak ranging over all positions at once, and the
-- 'tamperRedeemerTweak' / 'malformRedeemerTweak' counterparts of the datum
-- tweaks from "Cooked.Tweak.Outputs".
module Cooked.Tweak.Redeemers
  ( -- * Per-position redeemer modifications
    modifyRedeemersOfTypeAtTweak,
    modifySpendRedeemersOfTypeTweak,
    modifyMintRedeemersOfTypeTweak,
    modifyProposalRedeemersOfTypeTweak,
    modifyWithdrawalRedeemersOfTypeTweak,
    modifyCertificateRedeemersOfTypeTweak,

    -- * All-position redeemer modifications
    modifyRedeemersOfTypeTweak,

    -- * Tampering and malforming redeemers
    tamperRedeemerTweak,
    TamperRedeemerLbl (..),
    malformRedeemerTweak,
    MalformRedeemerLbl (..),
  )
where

import Control.Monad
import Cooked.Pretty.Class
import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Labels
import Data.Map qualified as Map
import Optics.Core
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.NonDet

-- | Applies an optional modification to all redeemers of type @a@ focused by a
-- given optic. Returns the list of modified redeemers, as they were before
-- being modified. This is the position-agnostic building block from which the
-- per-position tweaks below are derived.
modifyRedeemersOfTypeAtTweak ::
  forall a b k is effs.
  ( RedeemerConstrs a,
    RedeemerConstrs b,
    Member Tweak effs,
    Is k A_Traversal
  ) =>
  -- | An optic focusing the redeemers to consider
  Optic' k is TxSkel TxSkelRedeemer ->
  -- | The modification to attempt on each typed redeemer
  (a -> Maybe b) ->
  Sem effs [TxSkelRedeemer]
modifyRedeemersOfTypeAtTweak optic f =
  overMaybeTweak optic $ \red -> do
    typedRedeemer <- red ^? txSkelRedeemerTypedAT
    typedRedeemerModified <- f typedRedeemer
    return $ red & txSkelRedeemerTypedAT @a .~ typedRedeemerModified

-- | Applies an optional modification to all spending redeemers of type @a@.
-- Returns the list of modified redeemers, as they were before being modified.
modifySpendRedeemersOfTypeTweak ::
  forall a b effs.
  (RedeemerConstrs a, RedeemerConstrs b, Member Tweak effs) =>
  (a -> Maybe b) ->
  Sem effs [TxSkelRedeemer]
modifySpendRedeemersOfTypeTweak =
  modifyRedeemersOfTypeAtTweak (txSkelInputsL % iso Map.toList Map.fromList % traversed % _2)

-- | Applies an optional modification to all minting redeemers of type @a@.
-- Returns the list of modified redeemers, as they were before being modified.
modifyMintRedeemersOfTypeTweak ::
  forall a b effs.
  (RedeemerConstrs a, RedeemerConstrs b, Member Tweak effs) =>
  (a -> Maybe b) ->
  Sem effs [TxSkelRedeemer]
modifyMintRedeemersOfTypeTweak =
  modifyRedeemersOfTypeAtTweak (txSkelMintsL % txSkelMintsListI % traversed % mintRedeemedScriptL % userTxSkelRedeemerL)

-- | Applies an optional modification to all proposing redeemers of type @a@.
-- Returns the list of modified redeemers, as they were before being modified.
modifyProposalRedeemersOfTypeTweak ::
  forall a b effs.
  (RedeemerConstrs a, RedeemerConstrs b, Member Tweak effs) =>
  (a -> Maybe b) ->
  Sem effs [TxSkelRedeemer]
modifyProposalRedeemersOfTypeTweak =
  modifyRedeemersOfTypeAtTweak (txSkelProposalsL % traversed % txSkelProposalMConstitutionAT % _Just % userTxSkelRedeemerL)

-- | Applies an optional modification to all withdrawing redeemers of type @a@.
-- Returns the list of modified redeemers, as they were before being modified.
modifyWithdrawalRedeemersOfTypeTweak ::
  forall a b effs.
  (RedeemerConstrs a, RedeemerConstrs b, Member Tweak effs) =>
  (a -> Maybe b) ->
  Sem effs [TxSkelRedeemer]
modifyWithdrawalRedeemersOfTypeTweak =
  modifyRedeemersOfTypeAtTweak (txSkelWithdrawalsL % txSkelWithdrawalsListI % traversed % withdrawalUserL % userTxSkelRedeemerAT)

-- | Applies an optional modification to all certifying redeemers of type @a@.
-- Returns the list of modified redeemers, as they were before being modified.
modifyCertificateRedeemersOfTypeTweak ::
  forall a b effs.
  (RedeemerConstrs a, RedeemerConstrs b, Member Tweak effs) =>
  (a -> Maybe b) ->
  Sem effs [TxSkelRedeemer]
modifyCertificateRedeemersOfTypeTweak =
  modifyRedeemersOfTypeAtTweak (txSkelCertificatesL % traversed % txSkelCertificateOwnerAT % userTxSkelRedeemerL)

-- | Applies an optional modification to all redeemers of type @a@, regardless
-- of their position in the skeleton (spending, minting, proposing, withdrawing
-- or certifying). Returns the list of modified redeemers, as they were before
-- being modified.
modifyRedeemersOfTypeTweak ::
  forall a b effs.
  (RedeemerConstrs a, RedeemerConstrs b, Member Tweak effs) =>
  (a -> Maybe b) ->
  Sem effs [TxSkelRedeemer]
modifyRedeemersOfTypeTweak f =
  concat
    <$> sequence
      [ modifySpendRedeemersOfTypeTweak f,
        modifyMintRedeemersOfTypeTweak f,
        modifyProposalRedeemersOfTypeTweak f,
        modifyWithdrawalRedeemersOfTypeTweak f,
        modifyCertificateRedeemersOfTypeTweak f
      ]

-- | A label added to a 'TxSkel' on which the 'tamperRedeemerTweak' has been
-- successfully applied
data TamperRedeemerLbl = TamperRedeemerLbl deriving (Show, Eq, Ord)

instance PrettyCooked TamperRedeemerLbl where
  prettyCooked _ = "TamperRedeemer"

-- | A tweak that tries to change the redeemers of a certain type, in any
-- position of the skeleton, with a prescribed tampering function. The tampering
-- function ignores redeemers of other types and those for which it returns
-- @Nothing@. This is the redeemer counterpart of
-- 'Cooked.Tweak.Outputs.tamperDatumTweak'.
--
-- The tweak returns a list of the modified redeemers, as they were *before* the
-- modification was applied to them.
tamperRedeemerTweak ::
  forall a effs.
  (Members '[Tweak, NonDet] effs, RedeemerConstrs a) =>
  (a -> Maybe a) ->
  Sem effs [TxSkelRedeemer]
tamperRedeemerTweak change = do
  beforeModification <- modifyRedeemersOfTypeTweak change
  guard . not . null $ beforeModification
  addLabelTweak TamperRedeemerLbl
  return beforeModification

-- | A label added to a 'TxSkel' on which the 'malformRedeemerTweak' has been
-- successfully applied
data MalformRedeemerLbl = MalformRedeemerLbl deriving (Show, Eq, Ord)

instance PrettyCooked MalformRedeemerLbl where
  prettyCooked _ = "MalformRedeemer"

-- | A tweak that tries to change the redeemers of a certain type, in any
-- position of the skeleton, with a prescribed tampering function. There are two
-- main differences with 'tamperRedeemerTweak'. First, the tampering function
-- returns 'Api.BuiltinData', allowing it to do pretty much anything with the
-- redeemers. Second, for every redeemer there are zero or more options for how
-- to modify it, and all combinations of these modifications are tried. This is
-- the redeemer counterpart of 'Cooked.Tweak.Outputs.malformDatumTweak'.
malformRedeemerTweak ::
  forall a effs.
  (Members '[Tweak, NonDet] effs, RedeemerConstrs a) =>
  (a -> [Api.BuiltinData]) ->
  Sem effs ()
malformRedeemerTweak change = do
  redeemers <- viewAllTweak txSkelRedeemersT
  let modifiedRedeemers = map (\red -> red : changeRedeemer red) redeemers
      -- We remove the first combination because it consists of all the heads
      -- and therefore it is the combination consisting of no changes at all.
      modifiedRedeemerGroups = tail $ sequence modifiedRedeemers
  msum $ map (setTweak (partsOf txSkelRedeemersT)) modifiedRedeemerGroups
  addLabelTweak MalformRedeemerLbl
  where
    changeRedeemer :: TxSkelRedeemer -> [TxSkelRedeemer]
    changeRedeemer red = do
      typedRed <- red ^.. txSkelRedeemerTypedAT @a
      modifiedData <- change typedRed
      return $ red & txSkelRedeemerBuiltinDataL .~ modifiedData
