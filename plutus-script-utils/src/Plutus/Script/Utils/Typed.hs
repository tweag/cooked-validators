{-# LANGUAGE DeriveAnyClass #-}

module Plutus.Script.Utils.Typed
  ( UntypedValidator,
    UntypedMintingPolicy,
    UntypedStakeValidator,
    ValidatorTypes (..),
    TypedValidator (..),
    toCardanoAddressAny,
    forwardingMintingPolicy,
    vForwardingMintingPolicy,
    forwardingMintingPolicyHash,
    generalise,
    Any,
    mkUntypedValidator,
    mkUntypedStakeValidator,
    mkUntypedMintingPolicy,
  )
where

import Cardano.Api qualified as C
import Data.Aeson (ToJSON)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Void (Void)
import GHC.Generics (Generic)
import Plutus.Script.Utils.Address
  ( ToAddress (toAddress),
    ToCardanoAddress (toCardanoAddress),
    ToCredential (toCredential),
  )
import Plutus.Script.Utils.Scripts
  ( MintingPolicy,
    MintingPolicyHash,
    Script,
    ToScript (toScript),
    ToScriptHash (toScriptHash),
    ToValidator (toValidator),
    ToValidatorHash (toValidatorHash),
    ToVersioned (toVersioned),
    Validator,
    ValidatorHash,
    Versioned (unversioned),
    getValidator,
  )
import PlutusLedgerApi.V1 qualified as PV1
import PlutusTx.Prelude (BuiltinData, BuiltinString, BuiltinUnit, check, trace)
import PlutusTx.Prelude qualified as PlutusTx

type UntypedValidator = BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit

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
  type DatumType a = ()

instance ValidatorTypes ()

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
instance ToCardanoAddress (TypedValidator a) where
  toCardanoAddress networkId = toCardanoAddress networkId . tvValidator

toCardanoAddressAny :: C.NetworkId -> TypedValidator a -> C.AddressAny
toCardanoAddressAny nid tv =
  case toCardanoAddress nid tv of
    C.AddressInEra C.ShelleyAddressInEra {} addr -> C.AddressShelley addr
    C.AddressInEra C.ByronAddressInAnyEra {} addr -> C.AddressByron addr

-- | Generalise the typed validator to one that works with the 'Data' type.  we
-- can do this safely because the on-chain validators are untyped, so they
-- always take 'BuiltinData' arguments. The validator script stays the same, so
-- the conversion from 'BuiltinData' to 'a' still takes place, even if it's not
-- reflected in the type signature anymore.
generalise :: TypedValidator a -> TypedValidator Any
generalise = coerce

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

{-# INLINEABLE tracedSafeFrom #-}
tracedSafeFrom :: (PV1.FromData a) => BuiltinString -> BuiltinData -> Maybe a
tracedSafeFrom label builtinData = do
  dat <- PV1.fromBuiltinData builtinData
  -- We trace after the decoding is actually successful
  return $ trace label dat

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
-- For debugging purpose, it may be of interest to know that in the default
-- implementation, the parameters are decoded in the order they appear (data,
-- redeemer and then script context). A log trace is generated after each
-- successfully decoded parameter.  Thus, if a parameter can't be decoded, the
-- culprit is the first parameter in the list that doesn't appear as
-- successfully decoded in the log trace.
{-# INLINEABLE mkUntypedValidator #-}
mkUntypedValidator ::
  (PV1.FromData datum, PV1.FromData redeemer, PV1.FromData scriptContext) =>
  (datum -> redeemer -> scriptContext -> Bool) ->
  UntypedValidator
mkUntypedValidator f d r sc =
  check $
    PlutusTx.fromMaybe
      False
      ( do
          dat <- tracedSafeFrom "Datum decoded successfully" d
          red <- tracedSafeFrom "Redeemer decoded successfully" r
          ctx <- tracedSafeFrom "Script context decoded successfully" sc
          return $ f dat red ctx
      )

-- | Converts a custom redeemer from a stake validator function to an
-- untyped stake validator function.
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
-- For debugging purpose, it may be of interest to know that in the default
-- implementation, the parameters are decoded in the order they appear
-- (redeemer and then script context). A log trace is generated after each
-- successfully decoded parameter.  Thus, if a parameter can't be decoded, the
-- culprit is the first parameter in the list that doesn't appear as
-- successfully decoded in the log trace.
{-# INLINEABLE mkUntypedStakeValidator #-}
mkUntypedStakeValidator ::
  (PV1.FromData redeemer, PV1.FromData scriptContext) =>
  (redeemer -> scriptContext -> Bool) ->
  UntypedStakeValidator
mkUntypedStakeValidator f r sc =
  check $
    PlutusTx.fromMaybe
      False
      ( do
          red <- tracedSafeFrom "Redeemer decoded successfully" r
          ctx <- tracedSafeFrom "Script context decoded successfully" sc
          return $ f red ctx
      )

-- | Converts a custom redeemer from a minting policy function to an
-- untyped minting policy function.
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
-- For debugging purpose, it may be of interest to know that in the default
-- implementation, the parameters are decoded in the order they appear
-- (redeemer and then script context). A log trace is generated after each
-- successfully decoded parameter.  Thus, if a parameter can't be decoded, the
-- culprit is the first parameter in the list that doesn't appear as
-- successfully decoded in the log trace.
{-# INLINEABLE mkUntypedMintingPolicy #-}
mkUntypedMintingPolicy ::
  (PV1.FromData redeemer, PV1.FromData scriptContext) =>
  (redeemer -> scriptContext -> Bool) ->
  UntypedMintingPolicy
mkUntypedMintingPolicy = mkUntypedStakeValidator
