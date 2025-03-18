module Plutus.Script.Utils.Typed
  ( UntypedValidator,
    UntypedMintingPolicy,
    UntypedStakeValidator,
    ---
    ValidatorTypes (..),
    TypedValidator (..),
    validatorCardanoAddress,
    validatorCardanoAddressAny,
    validatorScript,
    vValidatorScript,
    forwardingMintingPolicy,
    vForwardingMintingPolicy,
    forwardingMintingPolicyHash,
    generalise,
    ---
    Any,
    Language (PlutusV1, PlutusV2, PlutusV3),
    Versioned (Versioned, unversioned, version),
    IsScriptContext (..),
    ScriptContextV1,
    ScriptContextV2,
    ScriptContextV3,
  )
where

import Cardano.Api qualified as C
import Data.Aeson (ToJSON)
import Data.Kind (Type)
import Data.Void (Void)
import GHC.Generics (Generic)
import Plutus.Script.Utils.Address
  ( ToAddress (toAddress),
    ToCredential (toCredential),
  )
import Plutus.Script.Utils.Scripts
  ( Language (PlutusV1, PlutusV2, PlutusV3),
    MintingPolicy,
    MintingPolicyHash,
    Script,
    ToScript (toScript),
    ToScriptHash (toScriptHash),
    ToValidator (toValidator),
    ToValidatorHash (toValidatorHash),
    ToVersioned (toVersioned),
    Validator,
    ValidatorHash,
    Versioned (Versioned, unversioned, version),
    getValidator,
    toCardanoAddressInConway,
  )
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V2 qualified as PV2
import PlutusLedgerApi.V3 qualified as PV3
import PlutusTx.Prelude (BuiltinData, BuiltinString, BuiltinUnit, check, trace)

type UntypedValidator = BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit

type UntypedScriptChang = BuiltinData -> BuiltinUnit

type UntypedMintingPolicy = BuiltinData -> BuiltinData -> BuiltinUnit

type UntypedStakeValidator = BuiltinData -> BuiltinData -> BuiltinUnit

data Any
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | A class that associates a type standing for a connection type with two types, the type of the
-- redeemer and the data script for that connection type.
class ValidatorTypes (a :: Type) where
  -- | The type of the redeemers of this connection type.
  type RedeemerType a :: Type

  -- | The type of the data of this connection type.
  type DatumType a :: Type

  -- Defaults
  type RedeemerType a = ()
  type DatumType a = BuiltinUnit

instance ValidatorTypes Void where
  type RedeemerType Void = Void
  type DatumType Void = Void

instance ValidatorTypes Any where
  type RedeemerType Any = BuiltinData
  type DatumType Any = BuiltinData

-- | A typed validator script with its 'ValidatorScript' and 'Address'.
data TypedValidator (a :: Type) = TypedValidator
  { tvValidator :: Versioned Validator,
    tvValidatorHash :: ValidatorHash,
    tvForwardingMPS :: Versioned MintingPolicy,
    -- | The hash of the minting policy that checks whether the validator
    --   is run in this transaction
    tvForwardingMPSHash :: MintingPolicyHash
  }
  deriving stock (Show, Eq, Generic)

instance ToScript (TypedValidator a) where
  toScript = toScript . tvValidator

instance ToVersioned Script (TypedValidator a) where
  toVersioned = fmap getValidator . tvValidator

instance ToVersioned Validator (TypedValidator a) where
  toVersioned = tvValidator

instance ToScriptHash (TypedValidator a) where
  toScriptHash = toScriptHash . tvValidator

instance ToValidator (TypedValidator a) where
  toValidator = toValidator . tvValidator

instance ToValidatorHash (TypedValidator a) where
  toValidatorHash = toValidatorHash . tvValidator

instance ToCredential (TypedValidator a) where
  toCredential = toCredential . tvValidator

instance ToAddress (TypedValidator a) where
  toAddress = toAddress . tvValidator

-- | The address of the validator.
validatorCardanoAddress :: C.NetworkId -> TypedValidator a -> C.AddressInEra C.ConwayEra
validatorCardanoAddress = toCardanoAddressInConway

validatorCardanoAddressAny :: C.NetworkId -> TypedValidator a -> C.AddressAny
validatorCardanoAddressAny nid tv =
  case validatorCardanoAddress nid tv of
    C.AddressInEra C.ShelleyAddressInEra {} addr -> C.AddressShelley addr
    C.AddressInEra C.ByronAddressInAnyEra {} addr -> C.AddressByron addr

-- | The unversioned validator script itself.
validatorScript :: TypedValidator a -> Validator
validatorScript = unversioned . vValidatorScript

-- | The validator script itself.
vValidatorScript :: TypedValidator a -> Versioned Validator
vValidatorScript = tvValidator

-- | Generalise the typed validator to one that works with the 'Data' type.
generalise :: forall a. TypedValidator a -> TypedValidator Any
generalise TypedValidator {tvValidator, tvValidatorHash, tvForwardingMPS, tvForwardingMPSHash} =
  -- we can do this safely because the on-chain validators are untyped, so they always
  -- take 'BuiltinData' arguments. The validator script stays the same, so the conversion
  -- from 'BuiltinData' to 'a' still takes place, even if it's not reflected in the type
  -- signature anymore.
  TypedValidator {tvValidator, tvValidatorHash, tvForwardingMPS, tvForwardingMPSHash}

-- | The unversioned minting policy that forwards all checks to the instance's
--  validator
forwardingMintingPolicy :: TypedValidator a -> MintingPolicy
forwardingMintingPolicy = unversioned . tvForwardingMPS

-- | The minting policy that forwards all checks to the instance's
--  validator
vForwardingMintingPolicy :: TypedValidator a -> Versioned MintingPolicy
vForwardingMintingPolicy = tvForwardingMPS

-- | Hash of the minting policy that forwards all checks to the instance's
--  validator
forwardingMintingPolicyHash :: TypedValidator a -> MintingPolicyHash
forwardingMintingPolicyHash = tvForwardingMPSHash

{-# INLINEABLE tracedUnsafeFrom #-}
tracedUnsafeFrom :: forall a. (PV1.UnsafeFromData a) => BuiltinString -> BuiltinData -> a
tracedUnsafeFrom label d = trace label $ PV1.unsafeFromBuiltinData d

class (PV1.UnsafeFromData sc) => IsScriptContext sc where
  {-# INLINEABLE mkUntypedValidator #-}

  -- | Converts a custom datum and redeemer from a validator function to an
  -- untyped validator function. See Note [Scripts returning Bool].
  --
  -- Here's an example of how this function can be used:
  --
  -- @
  --   import PlutusTx qualified
  --   import PlutusLedgerApi.V2.Scripts qualified as Plutus
  --   import Plutus.Script.Utils.V2.Scripts (mkUntypedValidator)
  --
  --   newtype MyCustomDatum = MyCustomDatum Integer
  --   PlutusTx.unstableMakeIsData ''MyCustomDatum
  --   newtype MyCustomRedeemer = MyCustomRedeemer Integer
  --   PlutusTx.unstableMakeIsData ''MyCustomRedeemer
  --
  --   mkValidator :: MyCustomDatum -> MyCustomRedeemer -> Plutus.ScriptContext -> Bool
  --   mkValidator _ _ _ = True
  --
  --   validator :: Plutus.Validator
  --   validator = Plutus.mkValidatorScript
  --       $$(PlutusTx.compile [|| wrap ||])
  --    where
  --       wrap = mkUntypedValidator mkValidator
  -- @
  --
  -- Here's an example using a parameterized validator:
  --
  -- @
  --   import PlutusTx qualified
  --   import PlutusLedgerApi.V2.Scripts qualified as Plutus
  --   import Plutus.Script.Utils.V2.Scripts (mkUntypedValidator)
  --
  --   newtype MyCustomDatum = MyCustomDatum Integer
  --   PlutusTx.unstableMakeIsData ''MyCustomDatum
  --   newtype MyCustomRedeemer = MyCustomRedeemer Integer
  --   PlutusTx.unstableMakeIsData ''MyCustomRedeemer
  --
  --   mkValidator :: Int -> MyCustomDatum -> MyCustomRedeemer -> Plutus.ScriptContext -> Bool
  --   mkValidator _ _ _ _ = True
  --
  --   validator :: Int -> Plutus.Validator
  --   validator i = Plutus.mkValidatorScript
  --       $$(PlutusTx.compile [|| wrap . mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode i
  --    where
  --       wrap = mkUntypedValidator
  -- @
  --
  -- For debugging purpose, it may be of interest to know that in the default implementation,
  -- the parameters are decoded in the order they appear
  -- (data, redeemer and then script context).
  -- A log trace is generated after each successfully decoded parameter.
  -- Thus, if a parameter can't be decoded, the culprit is the first parameter in the list that doesn't appear as
  -- successfully decoded in the log trace.
  mkUntypedValidator ::
    (PV1.UnsafeFromData d, PV1.UnsafeFromData r) =>
    (d -> r -> sc -> Bool) ->
    UntypedValidator
  -- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
  mkUntypedValidator f d r p =
    check $
      f
        (tracedUnsafeFrom "Data decoded successfully" d)
        (tracedUnsafeFrom "Redeemer decoded successfully" r)
        (tracedUnsafeFrom "Script context decoded successfully" p)

  {-# INLINEABLE mkUntypedScriptChang #-}
  mkUntypedScriptChang :: (sc -> Bool) -> UntypedScriptChang
  mkUntypedScriptChang f p =
    check $
      f
        (tracedUnsafeFrom "Script context decoded successfully" p)

  {-# INLINEABLE mkUntypedStakeValidator #-}

  -- | Converts a custom redeemer from a stake validator function to an
  -- untyped stake validator function. See Note [Scripts returning Bool].
  --
  -- Here's an example of how this function can be used:
  --
  -- @
  --   import PlutusTx qualified
  --   import PlutusLedgerApi.V1.Scripts qualified as Plutus
  --   import Plutus.Script.Utils.V1.Scripts (mkUntypedStakeValidator)
  --
  --   newtype MyCustomRedeemer = MyCustomRedeemer Integer
  --   PlutusTx.unstableMakeIsData ''MyCustomRedeemer
  --
  --   mkStakeValidator :: MyCustomRedeemer -> ScriptContext -> Bool
  --   mkStakeValidator _ _ = True
  --
  --   validator :: Plutus.Validator
  --   validator = Plutus.mkStakeValidatorScript
  --       $$(PlutusTx.compile [|| wrap ||])
  --    where
  --       wrap = mkUntypedStakeValidator mkStakeValidator
  -- @
  --
  -- For debugging purpose, it may be of interest to know that in the default implementation,
  -- the parameters are decoded in the order they appear
  -- (redeemer and then script context).
  -- A log trace is generated after each successfully decoded parameter.
  -- Thus, if a parameter can't be decoded, the culprit is the first parameter in the list that doesn't appear as
  -- successfully decoded in the log trace.
  mkUntypedStakeValidator ::
    (PV1.UnsafeFromData r) =>
    (r -> sc -> Bool) ->
    UntypedStakeValidator
  mkUntypedStakeValidator f r p =
    check $
      f
        (tracedUnsafeFrom "Redeemer decoded successfully" r)
        (tracedUnsafeFrom "Script context decoded successfully" p)

  {-# INLINEABLE mkUntypedMintingPolicy #-}

  -- | Converts a custom redeemer from a minting policy function to an
  -- untyped minting policy function. See Note [Scripts returning Bool].
  --
  -- Here's an example of how this function can be used:
  --
  -- @
  --   import PlutusTx qualified
  --   import PlutusLedgerApi.V1.Scripts qualified as Plutus
  --   import Plutus.Script.Utils.V1.Scripts (mkUntypedMintingPolicy)
  --
  --   newtype MyCustomRedeemer = MyCustomRedeemer Integer
  --   PlutusTx.unstableMakeIsData ''MyCustomRedeemer
  --
  --   mkMintingPolicy :: MyCustomRedeemer -> ScriptContext -> Bool
  --   mkMintingPolicy _ _ = True
  --
  --   validator :: Plutus.Validator
  --   validator = Plutus.mkMintingPolicyScript
  --       $$(PlutusTx.compile [|| wrap ||])
  --    where
  --       wrap = mkUntypedMintingPolicy mkMintingPolicy
  -- @
  --
  -- For debugging purpose, it may be of interest to know that in the default implementation,
  -- the parameters are decoded in the order they appear
  -- (redeemer and then script context).
  -- A log trace is generated after each successfully decoded parameter.
  -- Thus, if a parameter can't be decoded, the culprit is the first parameter in the list that doesn't appear as
  -- successfully decoded in the log trace.
  mkUntypedMintingPolicy ::
    (PV1.UnsafeFromData r) =>
    (r -> sc -> Bool) ->
    UntypedMintingPolicy
  -- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
  mkUntypedMintingPolicy f r p =
    check $
      f
        (tracedUnsafeFrom "Redeemer decoded successfully" r)
        (tracedUnsafeFrom "Script context decoded successfully" p)

type ScriptContextV1 = PV1.ScriptContext

type ScriptContextV2 = PV2.ScriptContext

type ScriptContextV3 = PV3.ScriptContext

instance IsScriptContext PV1.ScriptContext

instance IsScriptContext PV2.ScriptContext

instance IsScriptContext PV3.ScriptContext
