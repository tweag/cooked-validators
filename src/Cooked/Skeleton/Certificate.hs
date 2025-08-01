-- | This module exposes certificates in 'Cooked.Skelton.TxSkel'
module Cooked.Skeleton.Certificate where

import Cooked.Skeleton.Scripts
import Data.Kind (Type)
import Data.Typeable (Typeable, cast)
import Ledger.Slot qualified as Ledger
import Optics.Core
import PlutusLedgerApi.V3 qualified as Api

-- | The depiction of the possible actions in a certificate. Each actions
-- exposes, in its types, the possible owners it can have.
data CertificateAction :: UserReq -> Type where
  StakingRegister :: CertificateAction ReqEither
  StakingUnRegister :: CertificateAction ReqEither
  StakingDelegate :: Api.Delegatee -> CertificateAction ReqEither
  StakingRegisterDelegate :: Api.Delegatee -> CertificateAction ReqEither
  DRepRegister :: CertificateAction ReqEither
  DRepUpdate :: CertificateAction ReqEither
  DRepUnRegister :: CertificateAction ReqEither
  PoolRegister :: Api.PubKeyHash -> CertificateAction ReqPubKey
  PoolRetire :: Ledger.Slot -> CertificateAction ReqPubKey
  CommitteeRegisterHot :: Api.Credential -> CertificateAction ReqEither
  CommitteeResign :: CertificateAction ReqEither

deriving instance (Show (CertificateAction req))

deriving instance (Eq (CertificateAction req))

-- | Certificates used in 'Cooked.Skeleton.TxSkel'. The types ensure that each
-- certificate action is associated with a proper owner and deposit.
data TxSkelCertificate where
  TxSkelCertificate ::
    forall kind req.
    (Typeable kind, Typeable req, kind ⊨ req) =>
    { -- | All owners of certificates must be in 'Redemption' mode
      txSkelCertificateOwner :: User kind Redemption,
      -- | The certificate itself does impose a 'UserKind'
      txSkelCertificateAction :: CertificateAction req
    } ->
    TxSkelCertificate

deriving instance (Show TxSkelCertificate)

instance Eq TxSkelCertificate where
  (TxSkelCertificate owner action) == (TxSkelCertificate owner' action') =
    cast owner == Just owner' && cast action == Just action'

-- | Focuses on the owner of a 'TxSkelCertificate'
txSkelCertificateOwnerAT :: (Typeable user') => AffineTraversal' TxSkelCertificate (User user' Redemption)
txSkelCertificateOwnerAT =
  atraversal
    (\cert@(TxSkelCertificate {txSkelCertificateOwner}) -> maybe (Left cert) Right $ cast txSkelCertificateOwner)
    (\cert@(TxSkelCertificate @user _ action) -> maybe cert (`TxSkelCertificate` action) . cast @_ @(User user Redemption))

-- | Focuses on the action of a 'TxSkelCertificate'
txSkelCertificateActionAT :: (Typeable users') => AffineTraversal' TxSkelCertificate (CertificateAction users')
txSkelCertificateActionAT =
  atraversal
    (\cert@(TxSkelCertificate {txSkelCertificateAction}) -> maybe (Left cert) Right $ cast txSkelCertificateAction)
    (\cert@(TxSkelCertificate @_ @users owner _) -> maybe cert (TxSkelCertificate owner) . cast @_ @(CertificateAction users))
