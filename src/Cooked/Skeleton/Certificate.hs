{-# LANGUAGE UndecidableInstances #-}

-- | This module exposes certificates in 'Cooked.Skelton.TxSkel'
module Cooked.Skeleton.Certificate where

import Cooked.Skeleton.Scripts
import Data.Kind (Type)
import Data.Typeable (Typeable, cast)
import Ledger.Slot qualified as Ledger
import Optics.Core
import PlutusLedgerApi.V3 qualified as Api

-- | The depiction of the possible actions in a certificate. Each actions
-- exposes, in its types, the possible owners it can have and the requirements
-- on the deposits.
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
-- certificate action is associated with a proper owner and deposit.
data TxSkelCertificate where
  TxSkelCertificate ::
    (Typeable req) =>
    { -- | All owners of certificates must be in 'Redemption' mode
      txSkelCertificateOwner :: User req Redemption,
      -- | The certificate itself does impose a 'UserKind'
      txSkelCertificateAction :: CertificateAction req
    } ->
    TxSkelCertificate

deriving instance (Show TxSkelCertificate)

instance Eq TxSkelCertificate where
  (TxSkelCertificate owner action) == (TxSkelCertificate owner' action') =
    cast owner == Just owner' && cast action == Just action'

-- | Focuses on the owner of a 'TxSkelCertificate'
txSkelCertificateOwnerAT :: (Typeable req) => AffineTraversal' TxSkelCertificate (User req Redemption)
txSkelCertificateOwnerAT =
  atraversal
    (\cert@(TxSkelCertificate {txSkelCertificateOwner}) -> maybe (Left cert) Right $ cast txSkelCertificateOwner)
    (\cert@(TxSkelCertificate _ action) -> maybe cert (`TxSkelCertificate` action) . cast)

-- | Focuses on the action of a 'TxSkelCertificate'
txSkelCertificateActionAT :: (Typeable req) => AffineTraversal' TxSkelCertificate (CertificateAction req)
txSkelCertificateActionAT =
  atraversal
    (\cert@(TxSkelCertificate {txSkelCertificateAction}) -> maybe (Left cert) Right $ cast txSkelCertificateAction)
    (\cert@(TxSkelCertificate owner _) -> maybe cert (TxSkelCertificate owner) . cast)
