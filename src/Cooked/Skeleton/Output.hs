-- | This module exposes outputs as they can be defined in a
-- 'Cooked.Skeleton.TxSkel' with various utilities around them.
module Cooked.Skeleton.Output
  ( TxSkelOut (..),
    receives,
    txSkelOutValueL,
    txSkelOutDatumL,
    txSkelOutReferenceScriptL,
    txSkelOutStakingCredentialL,
    txSkelOutValidatorAT,
    IsTxSkelOutAllowedOwner (..),
    OwnerConstrs,
    txSkelOutCredentialG,
    txSkelOutAddressG,
    txSkelOutPKHashAT,
    txSkelOutTypedOwnerAT,
    txSkelOutValidatorHashAF,
  )
where

import Cooked.Skeleton.Datum
import Cooked.Skeleton.Payable
import Cooked.Skeleton.ReferenceScript
import Cooked.Skeleton.Value
import Cooked.Wallet
import Data.Typeable
import Optics.Core
import Optics.TH (makeLensesFor)
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.V1.Typed qualified as Script (TypedValidator (..))
import Plutus.Script.Utils.V3.Typed qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | A 'TxSkelOut' can either be owned by a pubkeyhash or a versioned validator
class IsTxSkelOutAllowedOwner a where
  toPKHOrValidator :: a -> Either Api.PubKeyHash (Script.Versioned Script.Validator)

instance IsTxSkelOutAllowedOwner Api.PubKeyHash where
  toPKHOrValidator = Left

instance IsTxSkelOutAllowedOwner Wallet where
  toPKHOrValidator = Left . Script.toPubKeyHash

instance IsTxSkelOutAllowedOwner (Script.Versioned Script.Validator) where
  toPKHOrValidator = Right

instance IsTxSkelOutAllowedOwner (Script.TypedValidator a) where
  toPKHOrValidator = toPKHOrValidator . Script.toVersioned @Script.Validator

instance IsTxSkelOutAllowedOwner (Script.Versioned Script.Script) where
  toPKHOrValidator = toPKHOrValidator . fmap Script.Validator

instance IsTxSkelOutAllowedOwner (Either Api.PubKeyHash (Script.Versioned Script.Validator)) where
  toPKHOrValidator = id

instance IsTxSkelOutAllowedOwner (Script.MultiPurposeScript a) where
  toPKHOrValidator = toPKHOrValidator . Script.toVersioned @Script.Validator

-- | Type constraints over the owner of a 'TxSkelOut'
type OwnerConstrs owner =
  ( IsTxSkelOutAllowedOwner owner,
    Script.ToCredential owner,
    Typeable owner,
    Show owner
  )

-- | A rich output to be put into a 'Cooked.Skeleton.TxSkel'
data TxSkelOut where
  TxSkelOut ::
    (OwnerConstrs owner) =>
    { txSkelOutOwner :: owner,
      txSkelOutStakingCredential :: Maybe Api.StakingCredential,
      txSkelOutDatum :: TxSkelOutDatum,
      txSkelOutValue :: TxSkelOutValue,
      txSkelOutReferenceScript :: TxSkelOutReferenceScript
    } ->
    TxSkelOut

deriving instance Show TxSkelOut

-- | A lens to get or set the 'TxSkelOutDatum' from a 'TxSkelOut'
makeLensesFor [("txSkelOutDatum", "txSkelOutDatumL")] ''TxSkelOut

-- | A lens to get or set the 'TxSkelOutValue' from a 'TxSkelOut'
makeLensesFor [("txSkelOutValue", "txSkelOutValueL")] ''TxSkelOut

-- | A lens to get or set the 'TxSkelOutReferenceScript' from a 'TxSkelOut'
makeLensesFor [("txSkelOutReferenceScript", "txSkelOutReferenceScriptL")] ''TxSkelOut

-- | A lens to get or set the 'Maybe Api.StakingCredential' from a 'TxSkelOut'
makeLensesFor [("txSkelOutStakingCredential", "txSkelOutStakingCredentialL")] ''TxSkelOut

-- | Returns the credential of this 'TxSkelOut'
txSkelOutCredentialG :: Getter TxSkelOut Api.Credential
txSkelOutCredentialG = to $ \(TxSkelOut {txSkelOutOwner}) -> Script.toCredential txSkelOutOwner

-- | Returns the address of this 'TxSkelOut'
txSkelOutAddressG :: Getter TxSkelOut Api.Address
txSkelOutAddressG = to $ \txSkelOut ->
  Api.Address
    (view txSkelOutCredentialG txSkelOut)
    (view txSkelOutStakingCredentialL txSkelOut)

-- | Attempts to retrieve or set a typed owner from this 'TxSkelOut'
txSkelOutTypedOwnerAT :: (OwnerConstrs a, OwnerConstrs b) => AffineTraversal TxSkelOut TxSkelOut a b
txSkelOutTypedOwnerAT =
  atraversal
    (\txSkelOut@(TxSkelOut {txSkelOutOwner}) -> maybe (Left txSkelOut) Right (cast txSkelOutOwner))
    (\txSkelOut newOwner -> txSkelOut {txSkelOutOwner = newOwner})

instance Eq TxSkelOut where
  txSkelOut == txSkelOut' =
    view txSkelOutAddressG txSkelOut == view txSkelOutAddressG txSkelOut'
      && txSkelOutDatum txSkelOut == txSkelOutDatum txSkelOut'
      && txSkelOutValue txSkelOut == txSkelOutValue txSkelOut'
      && preview (txSkelOutReferenceScriptL % txSkelOutReferenceScriptHashAF) txSkelOut
        == preview (txSkelOutReferenceScriptL % txSkelOutReferenceScriptHashAF) txSkelOut'

-- | Returns the optional private key owning a given 'TxSkelOut'
txSkelOutPKHashAT :: AffineTraversal' TxSkelOut Api.PubKeyHash
txSkelOutPKHashAT =
  atraversal
    (\txSkelOut@(TxSkelOut {txSkelOutOwner}) -> either Right (const (Left txSkelOut)) $ toPKHOrValidator txSkelOutOwner)
    (\txSkelOut pkh -> txSkelOut {txSkelOutOwner = pkh})

-- | Returns the optional validator owning a given 'TxSkelOut'
txSkelOutValidatorAT :: AffineTraversal' TxSkelOut (Script.Versioned Script.Validator)
txSkelOutValidatorAT =
  atraversal
    (\txSkelOut@(TxSkelOut {txSkelOutOwner}) -> either (const $ Left txSkelOut) Right $ toPKHOrValidator txSkelOutOwner)
    (\txSkelOut val -> txSkelOut {txSkelOutOwner = val})

-- | Returns the optional validator hash owning a given 'TxSkelOut'
txSkelOutValidatorHashAF :: AffineFold TxSkelOut Script.ValidatorHash
txSkelOutValidatorHashAF = txSkelOutValidatorAT % to Script.toValidatorHash

-- | Smart constructor to build a 'TxSkelOut' from an owner and payment. This
-- should be the main way of building outputs.
receives :: (OwnerConstrs owner) => owner -> Payable els -> TxSkelOut
receives owner =
  ( `go`
      TxSkelOut
        owner
        Nothing -- No staking credential by default
        defaultTxSkelDatum -- Default datum defined below
        (TxSkelOutValue mempty True) -- Empty value by default, adjustable to min ada
        NoTxSkelOutReferenceScript -- No reference script by default
  )
  where
    go :: Payable els -> TxSkelOut -> TxSkelOut
    go (VisibleHashedDatum dat) = set txSkelOutDatumL (SomeTxSkelOutDatum dat (Hashed Resolved))
    go (InlineDatum dat) = set txSkelOutDatumL (SomeTxSkelOutDatum dat Inline)
    go (HiddenHashedDatum dat) = set txSkelOutDatumL (SomeTxSkelOutDatum dat (Hashed NotResolved))
    go (FixedValue v) = set txSkelOutValueL (TxSkelOutValue (Script.toValue v) False)
    go (Value v) = set txSkelOutValueL (TxSkelOutValue (Script.toValue v) True)
    go (ReferenceScript script) = set txSkelOutReferenceScriptL (SomeTxSkelOutReferenceScript script)
    go (StakingCredential stCred) = set txSkelOutStakingCredentialL (Script.toMaybeStakingCredential stCred)
    go (PayableAnd p1 p2) = go p2 . go p1

    defaultTxSkelDatum = case toPKHOrValidator owner of
      -- V1 and V2 script always need a datum, even if empty
      Right (Script.Versioned _ v) | v <= Script.PlutusV2 -> SomeTxSkelOutDatum () (Hashed NotResolved)
      -- V3 script and PKH do not necessarily need a datum
      _ -> NoTxSkelOutDatum
