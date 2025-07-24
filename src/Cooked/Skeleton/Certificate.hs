{-# LANGUAGE UndecidableInstances #-}

-- | This module exposes certificates in 'Cooked.Skelton.TxSkel'
module Cooked.Skeleton.Certificate where

import Cooked.Skeleton.Output (valueLovelaceP)
import Cooked.Skeleton.Redeemer
import Data.Kind (Constraint, Type)
import Data.Typeable (Typeable, cast)
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:<>:)), Symbol, TypeError)
import Ledger.Slot qualified as Ledger
import Optics.Core
import PlutusLedgerApi.V3 qualified as Api

-- | A type family representing membership. This requires @UndecidableInstances@
-- because the type checker is not smart enough to understand that this type
-- family decreases in @els@, due to the presence of @extras@. @extras@ is only
-- used to keep track of the original list and output a relevant message in the
-- empty case, which could otherwise be omitted altogther at no loss of type
-- safety.
type family Member (el :: a) (els :: [a]) (extras :: [a]) :: Constraint where
  Member x (x ': xs) _ = ()
  Member x (y ': xs) l = Member x xs (y ': l)
  Member x '[] l = TypeError ('ShowType x ':<>: 'Text " is not a member of " ':<>: 'ShowType l)

-- | A specific instance of @Member@ where the already browsed elements is @[]@
type (∈) el els = Member el els '[]

-- | The depiction of the possible actions in a certificate. Each actions
-- exposes, in its types, the possible owners it can have and the requirements
-- on the deposits.
data CertificateAction :: [Symbol] -> [Symbol] -> Type where
  StakingRegister :: CertificateAction '["PubKey", "Script"] ["Some", "Empty"]
  StakingUnRegister :: CertificateAction '["PubKey", "Script"] ["Some", "Empty"]
  StakingDelegate :: Api.Delegatee -> CertificateAction '["PubKey", "Script"] '["Empty"]
  StakingRegisterDelegate :: Api.Delegatee -> CertificateAction '["PubKey", "Script"] '["Some"]
  DRepRegister :: CertificateAction '["PubKey", "Script"] '["Some"]
  DRepUpdate :: CertificateAction '["PubKey", "Script"] '["Empty"]
  DRepUnRegister :: CertificateAction '["PubKey", "Script"] '["Some"]
  PoolRegister :: Api.PubKeyHash -> CertificateAction '["PubKey"] '["Empty"]
  PoolRetire :: Ledger.Slot -> CertificateAction '["PubKey"] '["Empty"]
  CommitteeRegisterHot :: Api.Credential -> CertificateAction '["PubKey", "Script"] '["Empty"]
  CommitteeResign :: CertificateAction '["PubKey", "Script"] '["Empty"]

deriving instance (Show (CertificateAction owners deposits))

deriving instance (Eq (CertificateAction owners deposits))

-- | A depiction of the owner of a certificate which exposes in the type the
-- kind of the owner. This is used to account for the fact that some certificate
-- can have any kind of owners while others require a pubkey specifically.
data Owner :: Symbol -> Type where
  PubKeyOwner :: Api.PubKeyHash -> Owner "PubKey"
  ScriptOwner :: RedeemedScript -> Owner "Script"

deriving instance (Show (Owner owner))

deriving instance (Eq (Owner owner))

-- | A depiction of the deposits which exposes in the type if there is indeed a
-- deposit or not. This is used to account for the fact that some certificates
-- require a deposit, some others require no deposit and some can have either.
data Deposit :: Symbol -> Type where
  EmptyDeposit :: Deposit "Empty"
  SomeDeposit :: Api.Lovelace -> Deposit "Some"

deriving instance (Show (Deposit deposit))

deriving instance (Eq (Deposit deposit))

-- | Certificates used in 'Cooked.Skeleton.TxSkel'. The types ensure that each
-- certificate action is associated with a proper owner and deposit.
data TxSkelCertificate where
  TxSkelCertificate ::
    forall owners deposits owner deposit.
    ( Typeable owners,
      Typeable deposits,
      Typeable owner,
      Typeable deposit,
      owner ∈ owners,
      deposit ∈ deposits
    ) =>
    { txSkelCertificateOwner :: Owner owner,
      txSkelCertificateDeposit :: Deposit deposit,
      txSkelCertificateAction :: CertificateAction owners deposits
    } ->
    TxSkelCertificate

deriving instance (Show TxSkelCertificate)

instance Eq TxSkelCertificate where
  (TxSkelCertificate o d a) == (TxSkelCertificate o' d' a') =
    cast o == Just o'
      && cast d == Just d'
      && cast a == Just a'

-- | Focuses on the possible redeemed script of a 'TxSkelCertificate'
txSkelCertificateRedeemedScriptAT :: AffineTraversal' TxSkelCertificate RedeemedScript
txSkelCertificateRedeemedScriptAT =
  atraversal
    ( \cert@(TxSkelCertificate owner _ _) -> case owner of
        ScriptOwner rs -> Right rs
        _ -> Left cert
    )
    ( flip $ \rs -> \case
        TxSkelCertificate ScriptOwner {} dep act -> TxSkelCertificate (ScriptOwner rs) dep act
        cert -> cert
    )

-- | Gets the deposited value in this certificate which might be empty in case
-- of an empty deposit.
txSkelCertificateDepositG :: Getter TxSkelCertificate Api.Value
txSkelCertificateDepositG =
  to
    ( \TxSkelCertificate {txSkelCertificateDeposit} ->
        view
          ( to
              ( \case
                  EmptyDeposit -> Api.Lovelace 0
                  SomeDeposit dep -> dep
              )
              % re valueLovelaceP
          )
          txSkelCertificateDeposit
    )
