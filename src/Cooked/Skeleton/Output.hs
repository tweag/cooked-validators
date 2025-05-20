-- | This module exposes outputs as they can be defined in a
-- 'Cooked.Skeleton.TxSkel' with various utilities around them.
module Cooked.Skeleton.Output
  ( TxSkelOut (..),
    receives,
    txSkelOutValueL,
    txSkelOutDatumL,
    txSkelOutReferenceScriptL,
    txSkelOutStakingCredentialL,
    txSkelOutValue,
    txSkelOutValidator,
    IsTxSkelOutAllowedOwner (..),
    txSkelOutReferenceScript,
    txSkelOutReferenceScriptHash,
    OwnerConstrs,
    txSkelOutAddress,
    txSkelOutPKHash,
    txSkelOutTypedOwnerAT,
  )
where

import Cooked.Skeleton.Datum
import Cooked.Skeleton.Payable
import Cooked.Skeleton.ReferenceScript
import Cooked.Skeleton.Value
import Cooked.Wallet
import Data.Either.Combinators
import Data.Function
import Data.Typeable
import Optics.Core
import Optics.TH (makeLensesFor)
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.V1.Typed qualified as Script (TypedValidator (..))
import Plutus.Script.Utils.V3.Typed qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Depicts the entities that are allowed to own a 'TxSkelOut'
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
    { tsoOwner :: owner,
      tsoSCred :: Maybe Api.StakingCredential,
      tsoDatum :: TxSkelOutDatum,
      tsoValue :: TxSkelOutValue,
      tsoRefSc :: TxSkelOutReferenceScript
    } ->
    TxSkelOut

deriving instance Show TxSkelOut

-- | Returns the address of this 'TxSkelOut'
txSkelOutAddress :: TxSkelOut -> Api.Address
txSkelOutAddress (TxSkelOut owner stCred _ _ _) =
  Api.Address
    (Script.toCredential owner)
    (Script.toMaybeStakingCredential stCred)

-- | A lens to get or set the 'TxSkelOutDatum' from a 'TxSkelOut'
makeLensesFor [("tsoDatum", "txSkelOutDatumL")] ''TxSkelOut

-- | A lens to get or set the 'TxSkelOutValue' from a 'TxSkelOut'
makeLensesFor [("tsoValue", "txSkelOutValueL")] ''TxSkelOut

-- | A lens to get or set the 'TxSkelOutReferenceScript' from a 'TxSkelOut'
makeLensesFor [("tsoRefSc", "txSkelOutReferenceScriptL")] ''TxSkelOut

-- | A lens to get or set the 'Maybe Api.StakingCredential' from a 'TxSkelOut'
makeLensesFor [("tsoSCred", "txSkelOutStakingCredentialL")] ''TxSkelOut

-- | Attempts to retrieve or set a typed owner from this 'TxSkelOut'
txSkelOutTypedOwnerAT :: (OwnerConstrs a) => AffineTraversal' TxSkelOut a
txSkelOutTypedOwnerAT =
  atraversal
    (\txSkelOut@(TxSkelOut {tsoOwner}) -> maybe (Left txSkelOut) Right (cast tsoOwner))
    (\txSkelOut newOwner -> txSkelOut {tsoOwner = newOwner})

-- | Returns the value contained in a 'TxSkelOut'
txSkelOutValue :: TxSkelOut -> Api.Value
txSkelOutValue = (^. (txSkelOutValueL % txSkelOutValueContentL))

instance Eq TxSkelOut where
  txSkelOut == txSkelOut' =
    txSkelOutAddress txSkelOut == txSkelOutAddress txSkelOut'
      && tsoDatum txSkelOut == tsoDatum txSkelOut'
      && txSkelOutValue txSkelOut == txSkelOutValue txSkelOut'
      && txSkelOutReferenceScriptHash txSkelOut == txSkelOutReferenceScriptHash txSkelOut'

-- | Returns the optional private key owning a given 'TxSkelOut'
txSkelOutPKHash :: TxSkelOut -> Maybe Api.PubKeyHash
txSkelOutPKHash (TxSkelOut {tsoOwner}) = leftToMaybe $ toPKHOrValidator tsoOwner

-- | Returns the optional validator owning a given 'TxSkelOut'
txSkelOutValidator :: TxSkelOut -> Maybe (Script.Versioned Script.Validator)
txSkelOutValidator (TxSkelOut {tsoOwner}) = rightToMaybe $ toPKHOrValidator tsoOwner

-- | Returns the optional reference script in a 'TxSkelOut'
txSkelOutReferenceScript :: TxSkelOut -> Maybe (Script.Versioned Script.Script)
txSkelOutReferenceScript = txSkelOutRefScriptVersioned . view txSkelOutReferenceScriptL

-- | Returns the optional reference script hash in a 'TxSkelOut'
txSkelOutReferenceScriptHash :: TxSkelOut -> Maybe Api.ScriptHash
txSkelOutReferenceScriptHash = fmap Script.toScriptHash . txSkelOutReferenceScript

-- | Smart constructor to build a 'TxSkelOut' from an owner and payment. This
-- should be the main way of building outputs.
receives :: (OwnerConstrs owner) => owner -> Payable els -> TxSkelOut
receives owner =
  go $
    TxSkelOut
      owner
      Nothing -- No staking credential by default
      defaultTxSkelDatum -- Default datum defined below
      (TxSkelOutValue mempty True) -- Empty value by default, adjustable to min ada
      TxSkelOutNoReferenceScript -- No reference script by default
  where
    go :: TxSkelOut -> Payable els -> TxSkelOut
    go txSkelOut (VisibleHashedDatum dat) = txSkelOut & txSkelOutDatumL .~ TxSkelOutSomeDatum (DatumContent dat) (Hashed Resolved)
    go txSkelOut (InlineDatum dat) = txSkelOut & txSkelOutDatumL .~ TxSkelOutSomeDatum (DatumContent dat) Inline
    go txSkelOut (HiddenHashedDatum dat) = txSkelOut & txSkelOutDatumL .~ TxSkelOutSomeDatum (DatumContent dat) (Hashed NotResolved)
    go txSkelOut (FixedValue v) = txSkelOut & txSkelOutValueL .~ TxSkelOutValue (Script.toValue v) False
    go txSkelOut (Value v) = txSkelOut & txSkelOutValueL .~ TxSkelOutValue (Script.toValue v) True
    go txSkelOut (ReferenceScript script) = txSkelOut & txSkelOutReferenceScriptL .~ TxSkelOutSomeReferenceScript script
    go txSkelOut (StakingCredential stCred) = txSkelOut & txSkelOutStakingCredentialL .~ Script.toMaybeStakingCredential stCred
    go txSkelOut (PayableAnd p1 p2) = go (go txSkelOut p1) p2

    defaultTxSkelDatum = case toPKHOrValidator owner of
      -- V1 and V2 script always need a datum, even if empty
      Right (Script.Versioned _ v) | v <= Script.PlutusV2 -> TxSkelOutSomeDatum (DatumContent ()) (Hashed NotResolved)
      -- V3 script and PKH do not necessarily need a datum
      _ -> TxSkelOutNoDatum
