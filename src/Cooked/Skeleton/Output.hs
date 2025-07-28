-- | This module exposes outputs as they can be defined in a
-- 'Cooked.Skeleton.TxSkel' with various utilities around them.
module Cooked.Skeleton.Output
  ( TxSkelOut (..),
    receives,
    txSkelOutValueL,
    txSkelOutValueAutoAdjustL,
    txSkelOutDatumL,
    txSkelOutReferenceScriptL,
    txSkelOutStakingCredentialL,
    txSkelOutScriptAT,
    IsTxSkelOutAllowedOwner (..),
    OwnerConstrs,
    txSkelOutCredentialG,
    txSkelOutAddressG,
    txSkelOutPKHashAT,
    txSkelOutTypedOwnerAT,
    txSkelOutScriptHashAF,
    valueAssetClassAmountL,
    lovelaceIntegerI,
    valueLovelaceL,
    valueAssetClassAmountP,
    valueLovelaceP,
    ownerCredentialG,
    ownerUserG,
    txSkelOutUserL,
  )
where

import Cooked.Skeleton.Datum
import Cooked.Skeleton.Payable
import Cooked.Skeleton.ReferenceScript
import Cooked.Skeleton.Scripts
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
import PlutusTx.AssocMap qualified as PMap

-- * Requirements to be able to own a 'TxSkelOut'

-- | A 'TxSkelOut' can either be owned by a pubkeyhash or a versioned script
class IsTxSkelOutAllowedOwner a where
  toPKHOrVScript :: a -> Either Api.PubKeyHash VScript

instance IsTxSkelOutAllowedOwner Api.PubKeyHash where
  toPKHOrVScript = Left

instance IsTxSkelOutAllowedOwner Wallet where
  toPKHOrVScript = Left . Script.toPubKeyHash

instance IsTxSkelOutAllowedOwner VScript where
  toPKHOrVScript = Right

instance IsTxSkelOutAllowedOwner (Script.TypedValidator a) where
  toPKHOrVScript = toPKHOrVScript . Script.toVersioned @Script.Script

instance IsTxSkelOutAllowedOwner (Script.Versioned Script.Validator) where
  toPKHOrVScript = toPKHOrVScript . fmap Script.getValidator

instance IsTxSkelOutAllowedOwner (Either Api.PubKeyHash (Script.Versioned Script.Script)) where
  toPKHOrVScript = id

instance IsTxSkelOutAllowedOwner (Script.MultiPurposeScript a) where
  toPKHOrVScript = toPKHOrVScript . Script.toVersioned @Script.Script

instance IsTxSkelOutAllowedOwner (User IsEither Allocation) where
  toPKHOrVScript (UserPubKeyHash pkh) = Left (Script.toPubKeyHash pkh)
  toPKHOrVScript (UserScript vScript) = Right (toVScript vScript)

-- | Retrieves the credential of a 'TxSkelOut' allowed owner
ownerCredentialG :: (IsTxSkelOutAllowedOwner owner) => Getter owner Api.Credential
ownerCredentialG = ownerUserG % to Script.toCredential

-- | Getting a 'User' from an owner
ownerUserG :: (IsTxSkelOutAllowedOwner owner) => Getter owner (User IsEither Allocation)
ownerUserG = to (either UserPubKeyHash UserScript . toPKHOrVScript)

-- | Type constraints over the owner of a 'TxSkelOut'
type OwnerConstrs owner =
  ( IsTxSkelOutAllowedOwner owner,
    Typeable owner,
    Show owner
  )

-- * Definition of 'Cooked.Skeleton.TxSkel' outputs with associated optics

-- | A rich output to be put into a 'Cooked.Skeleton.TxSkel'
data TxSkelOut where
  TxSkelOut ::
    (OwnerConstrs owner) =>
    { -- The target of this payment
      txSkelOutOwner :: owner,
      -- What staking credential should be attached to this payment
      txSkelOutStakingCredential :: Maybe Api.StakingCredential,
      -- What datum should be placed in this payment
      txSkelOutDatum :: TxSkelOutDatum,
      -- What value should be paid
      txSkelOutValue :: Api.Value,
      -- Whether the paid value can be auto-adjusted for min ADA
      txSkelOutValueAutoAdjust :: Bool,
      -- What reference script should be attached to this payment
      txSkelOutReferenceScript :: TxSkelOutReferenceScript
    } ->
    TxSkelOut

deriving instance Show TxSkelOut

-- | A lens to get or set the 'Maybe Api.StakingCredential' from a 'TxSkelOut'
makeLensesFor [("txSkelOutStakingCredential", "txSkelOutStakingCredentialL")] ''TxSkelOut

-- | A lens to get or set the 'TxSkelOutDatum' from a 'TxSkelOut'
makeLensesFor [("txSkelOutDatum", "txSkelOutDatumL")] ''TxSkelOut

-- | A lens to get or set the 'Api.Value' from a 'TxSkelOut'
makeLensesFor [("txSkelOutValue", "txSkelOutValueL")] ''TxSkelOut

-- | A lens to get or set if the value can be auto-adjusted if needed
makeLensesFor [("txSkelOutValueAutoAdjust", "txSkelOutValueAutoAdjustL")] ''TxSkelOut

-- | A lens to get or set the 'TxSkelOutReferenceScript' from a 'TxSkelOut'
makeLensesFor [("txSkelOutReferenceScript", "txSkelOutReferenceScriptL")] ''TxSkelOut

-- | A lens to get or set the 'User' from a 'TxSkelOut'
txSkelOutUserL :: Lens' TxSkelOut (User IsEither Allocation)
txSkelOutUserL =
  lens
    (\TxSkelOut {txSkelOutOwner} -> view ownerUserG txSkelOutOwner)
    (\txSkelOut user -> txSkelOut {txSkelOutOwner = user})

-- | Returns the credential of this 'TxSkelOut'
txSkelOutCredentialG :: Getter TxSkelOut Api.Credential
txSkelOutCredentialG = to $ \(TxSkelOut {txSkelOutOwner}) -> view ownerCredentialG txSkelOutOwner

instance Script.ToCredential TxSkelOut where
  toCredential = view txSkelOutCredentialG

-- | Returns the address of this 'TxSkelOut'
txSkelOutAddressG :: Getter TxSkelOut Api.Address
txSkelOutAddressG = to $ \txSkelOut ->
  Api.Address
    (view txSkelOutCredentialG txSkelOut)
    (view txSkelOutStakingCredentialL txSkelOut)

instance Script.ToAddress TxSkelOut where
  toAddress = view txSkelOutAddressG

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
    (\txSkelOut@(TxSkelOut {txSkelOutOwner}) -> either Right (const (Left txSkelOut)) $ toPKHOrVScript txSkelOutOwner)
    (\txSkelOut pkh -> txSkelOut {txSkelOutOwner = pkh})

-- | Returns the optional script owning a given 'TxSkelOut'
txSkelOutScriptAT :: AffineTraversal' TxSkelOut VScript
txSkelOutScriptAT =
  atraversal
    (\txSkelOut@(TxSkelOut {txSkelOutOwner}) -> either (const $ Left txSkelOut) Right $ toPKHOrVScript txSkelOutOwner)
    (\txSkelOut val -> txSkelOut {txSkelOutOwner = val})

-- | Returns the optional script hash owning a given 'TxSkelOut'
txSkelOutScriptHashAF :: AffineFold TxSkelOut Script.ScriptHash
txSkelOutScriptHashAF = txSkelOutScriptAT % to Script.toScriptHash

-- * Additional optics revolving around 'Api.Value'

-- | A lens to get or set the amount of tokens of a certain 'Api.AssetClass'
-- from a given 'Api.Value'. This removes the entry if the new amount is 0.
valueAssetClassAmountL :: (Script.ToMintingPolicyHash mp) => mp -> Api.TokenName -> Lens' Api.Value Integer
valueAssetClassAmountL (Script.toCurrencySymbol -> cs) tk =
  lens
    (`Api.assetClassValueOf` Api.assetClass cs tk)
    ( \v@(Api.Value val) i -> case PMap.lookup cs val of
        -- No previous cs entry and nothing to add.
        Nothing | i == 0 -> v
        -- No previous cs entry, and something to add.
        Nothing -> Api.Value $ PMap.insert cs (PMap.singleton tk i) val
        -- A previous cs and tk entry, which needs to be removed and the whole
        -- cs entry as well because it only containes this tk.
        Just (PMap.toList -> [(tk', _)]) | i == 0, tk == tk' -> Api.Value $ PMap.delete cs val
        -- A previous cs and tk entry, which needs to be removed, but the whole
        -- cs entry has other tokens and thus is kept.
        Just tokenMap | i == 0 -> Api.Value $ PMap.insert cs (PMap.delete tk tokenMap) val
        -- A previous cs entry, in which we insert the new tk (regarless of
        -- whether the tk was already present).
        Just tokenMap -> Api.Value $ PMap.insert cs (PMap.insert tk i tokenMap) val
    )

-- | Isomorphism between 'Api.Lovelace' and integers
lovelaceIntegerI :: Iso' Api.Lovelace Integer
lovelaceIntegerI = iso Api.getLovelace Api.Lovelace

-- | Focus the Lovelace part in a value.
valueLovelaceL :: Lens' Api.Value Api.Lovelace
valueLovelaceL = valueAssetClassAmountL Api.adaSymbol Api.adaToken % re lovelaceIntegerI

-- | A prism to build a value from an asset class and amount, or retrieves the
-- amount from this asset class if it is not zero
valueAssetClassAmountP :: (Script.ToMintingPolicyHash mp) => mp -> Api.TokenName -> Prism' Api.Value Integer
valueAssetClassAmountP (Script.toCurrencySymbol -> cs) tk
  | ac <- Api.assetClass cs tk =
      prism
        ( \case
            i | i == 0 -> mempty
            i -> Api.assetClassValue ac i
        )
        ( \val -> case val `Api.assetClassValueOf` ac of
            i | i == 0 -> Left val
            i -> Right i
        )

-- | An instance of 'valueAssetClassAmountP' for 'Api.Lovelace'
valueLovelaceP :: Prism' Api.Value Api.Lovelace
valueLovelaceP = valueAssetClassAmountP Api.adaSymbol Api.adaToken % re lovelaceIntegerI

-- * Smart constructor to build 'TxSkelOut's

-- | Smart constructor to build a 'TxSkelOut' from an owner and payment. This
-- should be the main way of building outputs.
receives :: (OwnerConstrs owner) => owner -> Payable els -> TxSkelOut
receives owner =
  ( `go`
      TxSkelOut
        owner
        Nothing -- No staking credential by default
        defaultTxSkelDatum -- Default datum defined below
        mempty -- Empty value by default
        True -- the value is adjustable to min ADA by default
        NoTxSkelOutReferenceScript -- No reference script by default
  )
  where
    go :: Payable els -> TxSkelOut -> TxSkelOut
    go (VisibleHashedDatum dat) = set txSkelOutDatumL (SomeTxSkelOutDatum dat (Hashed Resolved))
    go (InlineDatum dat) = set txSkelOutDatumL (SomeTxSkelOutDatum dat Inline)
    go (HiddenHashedDatum dat) = set txSkelOutDatumL (SomeTxSkelOutDatum dat (Hashed NotResolved))
    go (FixedValue v) = set txSkelOutValueL (Script.toValue v) . set txSkelOutValueAutoAdjustL False
    go (Value v) = set txSkelOutValueL (Script.toValue v) . set txSkelOutValueAutoAdjustL True
    go (ReferenceScript script) = set txSkelOutReferenceScriptL (SomeTxSkelOutReferenceScript script)
    go (StakingCredential stCred) = set txSkelOutStakingCredentialL (Script.toMaybeStakingCredential stCred)
    go (PayableAnd p1 p2) = go p2 . go p1

    defaultTxSkelDatum = case toPKHOrVScript owner of
      -- V1 and V2 script always need a datum, even if empty
      Right (Script.Versioned _ v) | v <= Script.PlutusV2 -> SomeTxSkelOutDatum () (Hashed NotResolved)
      -- V3 script and PKH do not necessarily need a datum
      _ -> NoTxSkelOutDatum
