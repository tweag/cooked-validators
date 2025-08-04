-- | This module exposes outputs as they can be defined in a
-- 'Cooked.Skeleton.TxSkel' with various utilities around them.
module Cooked.Skeleton.Output
  ( TxSkelOut (..),
    receives,
    txSkelOutValueL,
    txSkelOutValueAutoAdjustL,
    txSkelOutDatumL,
    txSkelOutMReferenceScriptL,
    txSkelOutReferenceScriptAT,
    txSkelOutMStakingCredentialL,
    txSkelOutStakingCredentialAT,
    IsTxSkelOutAllowedOwner (..),
    txSkelOutCredentialG,
    txSkelOutAddressG,
    txSkelOutReferenceScriptHashAF,
    txSkelOutOwnerL,
  )
where

import Cooked.Skeleton.Datum
import Cooked.Skeleton.Payable
import Cooked.Skeleton.User
import Cooked.Skeleton.Value ()
import Cooked.Wallet
import Data.Typeable
import Optics.Core
import Optics.TH (makeLensesFor)
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.V1.Typed qualified as Script (TypedValidator (..))
import Plutus.Script.Utils.V3.Typed qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api

-- * Definition of 'Cooked.Skeleton.TxSkel' outputs

-- | An output to be put into a 'Cooked.Skeleton.TxSkel'
data TxSkelOut where
  TxSkelOut ::
    { -- The owner of this payment
      txSkelOutOwner :: User IsEither Allocation,
      -- What staking credential should be attached to this payment
      txSkelOutStakingCredential :: Maybe Api.StakingCredential,
      -- What datum should be placed in this payment
      txSkelOutDatum :: TxSkelOutDatum,
      -- What value should be paid
      txSkelOutValue :: Api.Value,
      -- Whether the paid value can be auto-adjusted for min ADA
      txSkelOutValueAutoAdjust :: Bool,
      -- What reference script should be attached to this payment
      txSkelOutReferenceScript :: Maybe VScript
    } ->
    TxSkelOut
  deriving (Eq, Ord, Show)

-- * Optics focusing on the reference script of a 'TxSkelOut'

-- | Focuses on the @Maybe VScript@ corresponding to the possible reference
-- script contained in this 'TxSkelOut'
makeLensesFor [("txSkelOutReferenceScript", "txSkelOutMReferenceScriptL")] ''TxSkelOut

-- | Focuses on the reference script of this 'TxSkelOut'
txSkelOutReferenceScriptAT :: AffineTraversal' TxSkelOut VScript
txSkelOutReferenceScriptAT = txSkelOutMReferenceScriptL % _Just

-- | Returns the possible reference script has of this 'TxSkelOut'
txSkelOutReferenceScriptHashAF :: AffineFold TxSkelOut Api.ScriptHash
txSkelOutReferenceScriptHashAF = txSkelOutReferenceScriptAT % to Script.toScriptHash

-- * Optics focusing on the staking credential of a 'TxSkelOut'

-- | Focuses on the @Maybe StakingCredential@ of this 'TxSkelOut'
makeLensesFor [("txSkelOutStakingCredential", "txSkelOutMStakingCredentialL")] ''TxSkelOut

-- | Focuses on the staking credential of this 'TxSkelOut'
txSkelOutStakingCredentialAT :: AffineTraversal' TxSkelOut Api.StakingCredential
txSkelOutStakingCredentialAT = txSkelOutMStakingCredentialL % _Just

-- | Optics focusing on the datum of a 'TxSkelOut'

-- | Focuses on the 'TxSkelOutDatum' of this 'TxSkelOut'
makeLensesFor [("txSkelOutDatum", "txSkelOutDatumL")] ''TxSkelOut

-- * Optics focusing on the value of this 'TxSkelOut'

-- | Focuses on the 'Api.Value' of this 'TxSkelOut'
makeLensesFor [("txSkelOutValue", "txSkelOutValueL")] ''TxSkelOut

-- | Focuses on whether the 'Api.Value' contained in this 'TxSkelOut' can be
-- adjusted to min ADA during transaction generation
makeLensesFor [("txSkelOutValueAutoAdjust", "txSkelOutValueAutoAdjustL")] ''TxSkelOut

-- * Optics focusing on the owner of this 'TxSkelOut'

-- | Focuses on the user of this 'TxSkelOut'
makeLensesFor [("txSkelOutOwner", "txSkelOutOwnerL")] ''TxSkelOut

-- * Additional optics around a 'TxSkelOut'

-- | Returns the credential of this 'TxSkelOut'
txSkelOutCredentialG :: Getter TxSkelOut Api.Credential
txSkelOutCredentialG = to $ \(TxSkelOut {txSkelOutOwner}) -> Script.toCredential txSkelOutOwner

-- | Returns the address of this 'TxSkelOut'
txSkelOutAddressG :: Getter TxSkelOut Api.Address
txSkelOutAddressG = to $ \txSkelOut ->
  Api.Address
    (view txSkelOutCredentialG txSkelOut)
    (view txSkelOutMStakingCredentialL txSkelOut)

-- * Instances for 'TxSkelOut'

instance Script.ToCredential TxSkelOut where
  toCredential = view txSkelOutCredentialG

instance Script.ToAddress TxSkelOut where
  toAddress = view txSkelOutAddressG

-- * Requirements to be able to own a 'TxSkelOut'

-- | A typeclass to ease the building of 'TxSkelOut', to be used in conjunction
-- to 'receives'.
class IsTxSkelOutAllowedOwner a where
  toPKHOrVScript :: a -> User IsEither Allocation

instance IsTxSkelOutAllowedOwner Api.PubKeyHash where
  toPKHOrVScript = UserPubKey

instance IsTxSkelOutAllowedOwner Wallet where
  toPKHOrVScript = UserPubKey

instance IsTxSkelOutAllowedOwner VScript where
  toPKHOrVScript = UserScript

instance (Typeable a) => IsTxSkelOutAllowedOwner (Script.TypedValidator a) where
  toPKHOrVScript = UserScript

instance IsTxSkelOutAllowedOwner (Script.Versioned Script.Validator) where
  toPKHOrVScript = UserScript

instance (Typeable a) => IsTxSkelOutAllowedOwner (Script.MultiPurposeScript a) where
  toPKHOrVScript = UserScript

instance IsTxSkelOutAllowedOwner (User IsEither Allocation) where
  toPKHOrVScript = id

-- * Smart constructor to build 'TxSkelOut's

-- | Smart constructor to build a 'TxSkelOut' from an owner and payment. This
-- should be the main way of building outputs.
receives :: (IsTxSkelOutAllowedOwner owner) => owner -> Payable els -> TxSkelOut
receives (toPKHOrVScript -> owner) =
  ( `go`
      TxSkelOut
        owner
        Nothing -- No staking credential by default
        defaultTxSkelDatum -- Default datum defined below
        mempty -- Empty value by default
        True -- the value is adjustable to min ADA by default
        Nothing -- No reference script by default)
  )
  where
    go :: Payable els -> TxSkelOut -> TxSkelOut
    go (VisibleHashedDatum dat) = set txSkelOutDatumL (SomeTxSkelOutDatum dat (Hashed Resolved))
    go (InlineDatum dat) = set txSkelOutDatumL (SomeTxSkelOutDatum dat Inline)
    go (HiddenHashedDatum dat) = set txSkelOutDatumL (SomeTxSkelOutDatum dat (Hashed NotResolved))
    go (FixedValue (Script.toValue -> v)) = set txSkelOutValueL v . set txSkelOutValueAutoAdjustL False
    go (Value (Script.toValue -> v)) = set txSkelOutValueL v . set txSkelOutValueAutoAdjustL True
    go (ReferenceScript (toVScript -> vScript)) = set txSkelOutMReferenceScriptL (Just vScript)
    go (StakingCredential (Script.toMaybeStakingCredential -> mStCred)) = set txSkelOutMStakingCredentialL mStCred
    go (PayableAnd p1 p2) = go p2 . go p1

    defaultTxSkelDatum = case owner of
      -- V1 and V2 script always need a datum, even if empty
      UserScript (toVScript -> Script.Versioned _ v) | v <= Script.PlutusV2 -> SomeTxSkelOutDatum () (Hashed NotResolved)
      -- V3 script and PKH do not necessarily need a datum
      _ -> NoTxSkelOutDatum
