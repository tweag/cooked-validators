-- | This module exposes the certificate constructs used in a
-- 'Cooked.Skeleton.TxSkel' and their associated utilities. To emit certificates
-- in a skeleton, the usual way is to invoke @txSkelCertificates =
-- [pubKeyCertificate pk action, scriptCertificate script redeemer action ...]@
module Cooked.Skeleton.Certificate
  ( -- * Data types
    CertificateAction (..),
    TxSkelCertificate (..),

    -- * Optics
    txSkelCertificateOwnerAT,
    txSkelCertificateActionAT,

    -- * Smart constructors
    pubKeyCertificate,
    scriptCertificate,
  )
where

import Cooked.Families
import Cooked.Skeleton.Redeemer
import Cooked.Skeleton.User
import Data.Kind (Type)
import Data.Typeable (Typeable, cast)
import Ledger.Slot qualified as Ledger
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | The depiction of the possible actions in a certificate. Each actions
-- exposes, in its types, the possible owners it can have.
data CertificateAction :: UserKind -> Type where
  StakingRegister :: CertificateAction IsEither
  StakingUnRegister :: CertificateAction IsEither
  StakingDelegate :: Api.Delegatee -> CertificateAction IsEither
  StakingRegisterDelegate :: Api.Delegatee -> CertificateAction IsEither
  DRepRegister :: CertificateAction IsEither
  DRepUpdate :: CertificateAction IsEither
  DRepUnRegister :: CertificateAction IsEither
  PoolRegister :: Api.PubKeyHash -> CertificateAction IsPubKey
  PoolRetire :: Ledger.Slot -> CertificateAction IsPubKey
  CommitteeRegisterHot :: Api.Credential -> CertificateAction IsEither
  CommitteeResign :: CertificateAction IsEither

deriving instance (Show (CertificateAction req))

deriving instance (Eq (CertificateAction req))

-- | Certificates used in 'Cooked.Skeleton.TxSkel'. The types ensure that each
-- certificate action is associated with a proper owner.
data TxSkelCertificate where
  TxSkelCertificate ::
    (Typeable kind) =>
    { -- | All owners of certificates must be in 'Redemption' mode
      txSkelCertificateOwner :: User kind Redemption,
      -- | The certificate itself does impose a 'UserKind'
      txSkelCertificateAction :: CertificateAction kind
    } ->
    TxSkelCertificate

deriving instance (Show TxSkelCertificate)

instance Eq TxSkelCertificate where
  (TxSkelCertificate owner action) == (TxSkelCertificate owner' action') =
    cast owner == Just owner' && cast action == Just action'

-- | Focuses on the owner of a 'TxSkelCertificate'
txSkelCertificateOwnerAT :: (Typeable user) => AffineTraversal' TxSkelCertificate (User user Redemption)
txSkelCertificateOwnerAT =
  atraversal
    (\cert@(TxSkelCertificate {txSkelCertificateOwner}) -> maybe (Left cert) Right $ cast txSkelCertificateOwner)
    (\cert@(TxSkelCertificate @user' _ action) -> maybe cert (`TxSkelCertificate` action) . cast @_ @(User user' Redemption))

-- | Focuses on the action of a 'TxSkelCertificate'
txSkelCertificateActionAT :: (Typeable user) => AffineTraversal' TxSkelCertificate (CertificateAction user)
txSkelCertificateActionAT =
  atraversal
    (\cert@(TxSkelCertificate {txSkelCertificateAction}) -> maybe (Left cert) Right $ cast txSkelCertificateAction)
    (\cert@(TxSkelCertificate @user' owner _) -> maybe cert (TxSkelCertificate owner) . cast @_ @(CertificateAction user'))

-- | Smart constructor for a pubkey certificate
pubKeyCertificate :: (Script.ToPubKeyHash pkh, Typeable pkh, Typeable a, a ∈ '[IsPubKey, IsEither]) => pkh -> CertificateAction a -> TxSkelCertificate
pubKeyCertificate pkh = TxSkelCertificate (UserPubKey pkh)

-- | Smart constructor for a script certificate
scriptCertificate :: (ToVScript script, Typeable script, RedeemerConstrs red) => script -> red -> CertificateAction IsEither -> TxSkelCertificate
scriptCertificate script red = TxSkelCertificate (UserRedeemedScript script (someTxSkelRedeemer red))
