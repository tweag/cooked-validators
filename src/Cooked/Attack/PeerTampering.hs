-- | This module provides an attack to replace a peer with another in every
-- possible locations in a skeleton. If the peers have different levels of
-- privileges, this can uncover vulnerabilities.
module Cooked.Attack.PeerTampering
  ( -- * Peer tampering params
    PeerTamperingParams (..),
    purePeerTamperingParams,
    singlePeerTamperingParams,
    balancingPeerTamperingParams,

    -- * Peer tampering label
    PeerTamperingLabel,

    -- * Peer tampering attack
    peerTamperingAttack,
  )
where

import Control.Monad
import Cooked.Pretty.Class
import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Insert
import Cooked.Tweak.Modify
import Cooked.Tweak.Query
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.NonDet

-- | A label added to a 'TxSkel' on which a tweak tampering a peer has been
-- applied. The label contains both the peer that has been replaced, and the
-- peer that replaced it during the attack.
newtype PeerTamperingLabel = PeerTamperingLabel [Api.PubKeyHash]
  deriving (Show, Eq, Ord)

instance PrettyCooked PeerTamperingLabel where
  prettyCookedOpt opts (PeerTamperingLabel changes) =
    prettyItemize opts "Modified peers" "-" $ prettyHash opts <$> changes

-- | Parameters of the peer tampering attack
data PeerTamperingParams effs
  = PeerTamperingParams
  { -- | The branching policy to use when several peers are targeted
    ptpBranching :: Branching,
    -- | The peer to replace, associated with a list of replacing peers
    ptpChanges :: Sem effs (Api.PubKeyHash, [Api.PubKeyHash])
  }

-- | A pure variant of 'PeerTamperingParams'
purePeerTamperingParams ::
  (Member NonDet effs) =>
  Branching ->
  [(Api.PubKeyHash, [Api.PubKeyHash])] ->
  PeerTamperingParams effs
purePeerTamperingParams branching = PeerTamperingParams branching . msum . fmap return

-- | Peer tampering params transforming a single user into another
singlePeerTamperingParams ::
  (Script.ToPubKeyHash existing, Script.ToPubKeyHash new) =>
  existing ->
  new ->
  PeerTamperingParams effs
singlePeerTamperingParams (Script.toPubKeyHash -> existing) (Script.toPubKeyHash -> new) =
  PeerTamperingParams OneBranchPerFoci (return (existing, [new]))

-- | Peer tampering params transforming the balancing user into another
balancingPeerTamperingParams ::
  ( Script.ToPubKeyHash new,
    Members '[Tweak, NonDet] effs
  ) =>
  new ->
  PeerTamperingParams effs
balancingPeerTamperingParams (Script.toPubKeyHash -> new) =
  PeerTamperingParams OneBranchPerFoci $ do
    balancingPolicy <- viewTweak (txSkelOptsL % txSkelOptBalancingPolicyL)
    existing <- case balancingPolicy of
      BalanceWithFirstSignatory -> do
        signatories <- toListOfTweak (txSkelSignatoriesL % traversed % txSkelSignatoryPubKeyHashL)
        case signatories of
          [] -> mzero
          first : _ -> return first
      BalanceWith (Script.toPubKeyHash -> user) -> return user
      DoNotBalance -> mzero
    return (existing, [new])

-- | Attempts to change the give peer into other peers everywhere in a 'TxSkel'
-- in ah attempt to uncover permission breaches.
peerTamperingAttack ::
  (Members '[Tweak, NonDet] effs) =>
  PeerTamperingParams effs ->
  Sem effs [Api.PubKeyHash]
peerTamperingAttack (PeerTamperingParams branching mChanges) = do
  (existing, targets) <- mChanges
  modified <-
    modifyTweakFromParams
      $ modifyTweakParamsNoTypeChange
        branching
        ((txSkelAllocatedPeersT % userPubKeyHashI) `adjoin` (txSkelRedeemedPeersT % userPubKeyHashI))
      $ \pkh -> if pkh == existing then targets else []
  insertInTweak txSkelLabelsL $ TxSkelLabel $ PeerTamperingLabel modified
  return modified
