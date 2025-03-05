{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains functions related to versioning scripts and
-- BuiltinData, or more specifially, 'Datum's and 'Redeemer's. These functions
-- do not depend on a particular version of Plutus.
module Plutus.Script.Utils.Scripts
  ( Script (..),
    PV1.ScriptHash (..),
    ToScript (..),
    ToScriptHash (..),
    ToCardanoScriptHash (..),
    Language (..),
    Versioned (..),
    ToVersioned (..),
    toCardanoAddressInConway,
    Validator (..),
    ToValidator (..),
    ValidatorHash (..),
    ToValidatorHash (..),
    MintingPolicy (..),
    ToMintingPolicy (..),
    MintingPolicyHash (..),
    ToMintingPolicyHash (..),
    StakeValidator (..),
    ToStakeValidator (..),
    StakeValidatorHash (..),
    ToStakeValidatorHash (..),
    scriptCredential,
    scriptCurrencySymbol,
    datumHash,
    redeemerHash,
    dataHash,
    toCardanoAPIData,
  )
where

import Cardano.Api qualified as C.Api
import Cardano.Api.Shelley qualified as C.Api
import Cardano.Ledger.Plutus.Language (Language (PlutusV1, PlutusV2, PlutusV3))
import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.String (IsString)
import GHC.Generics (Generic)
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Bytes (LedgerBytes (LedgerBytes))
import PlutusTx (CompiledCode, makeLift)
import PlutusTx qualified
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude (BuiltinUnit)
import Prettyprinter (Pretty (pretty))
import Prettyprinter.Extras (PrettyShow (PrettyShow))

{- Note [Hash computation of datums, redeemers and scripts]

We have three options for computing the hash (each with advantages and drawbacks):

1- Depend on `cardano-api` and use it's `Scripts.hashScriptData` and `Scripts.hashScript`
functions.
The good: most simplest way to compute the hashes.
The bad: this package has an additional pretty large dependency.

2- Depend on `cardano-ledger` instead and use their `hashScriptData` and `hashScript`.
The good: smaller footprint than `cardano-api`.
The bad: a lower-lever library than `cardano-api`.

3- Depend on `cardano-crypto-class`, and reimplement ourselves the hashing functions
from `cardano-ledger`.
The good: the lowest dependency footprint.
The bad: code duplication.

However, we expect that most Plutus script devs depending on this package will
also probably depend on `cardano-api`, so the dependency on `cardano-api` should
(probably) be an non-issue.

If this becomes an issue, we'll change the implementation.
-}

-- * Script hashes

-- | Extracting Plutus script hashes
class ToScriptHash a where
  toScriptHash :: a -> PV1.ScriptHash

instance ToScriptHash PV1.ScriptHash where
  toScriptHash = id

instance ToScriptHash C.Api.ScriptHash where
  toScriptHash = PV1.ScriptHash . Builtins.toBuiltin . C.Api.serialiseToRawBytes

-- | Extracting Cardano script hashes
class ToCardanoScriptHash a where
  toCardanoScriptHash :: a -> C.Api.ScriptHash

instance ToCardanoScriptHash C.Api.ScriptHash where
  toCardanoScriptHash = id

-- * Scripts

-- | Script wrapper around a ShortByteString (SerialisedScript)
newtype Script = Script {unScript :: PV1.SerialisedScript}
  deriving stock (Eq, Ord, Generic)
  deriving (Serialise) via PV1.SerialisedScript

instance Show Script where
  showsPrec _ _ = showString "<Script>"

-- | Extracting scripts
class ToScript a where
  toScript :: a -> Script

instance ToScript Script where
  toScript = id

instance ToScript PV1.SerialisedScript where
  toScript = Script

-- | Instance for staking and minting scripts in PlutusV1 and PlutusV2
instance ToScript (CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)) where
  toScript = toScript . serialiseCompiledCode

-- | Instance for spending scripts in PlutusV1 and PlutusV2
instance ToScript (CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)) where
  toScript = toScript . serialiseCompiledCode

-- | Instance for all scripts in PlutusV3
instance ToScript (CompiledCode (BuiltinData -> BuiltinUnit)) where
  toScript = toScript . serialiseCompiledCode

-- * Scripts with a version attached

deriving instance Serialise Language

instance Pretty Language where
  pretty PlutusV1 = "Plutus V1"
  pretty PlutusV2 = "Plutus V2"
  pretty PlutusV3 = "Plutus V3"

-- | A script of some kind with its Plutus language version
data Versioned script = Versioned
  { unversioned :: script,
    version :: Language
  }
  deriving stock (Show, Eq, Ord, Functor, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise)

instance (Pretty script) => Pretty (Versioned script) where
  pretty Versioned {unversioned, version} = pretty unversioned <> " (" <> pretty version <> ")"

instance (ToScript a) => ToCardanoScriptHash (Versioned a) where
  toCardanoScriptHash (Versioned (toScript -> Script script) lang) = case lang of
    PlutusV1 -> C.Api.hashScript $ C.Api.PlutusScript C.Api.PlutusScriptV1 $ C.Api.PlutusScriptSerialised script
    PlutusV2 -> C.Api.hashScript $ C.Api.PlutusScript C.Api.PlutusScriptV2 $ C.Api.PlutusScriptSerialised script
    PlutusV3 -> C.Api.hashScript $ C.Api.PlutusScript C.Api.PlutusScriptV3 $ C.Api.PlutusScriptSerialised script

instance (ToScript a) => ToScriptHash (Versioned a) where
  toScriptHash = toScriptHash . toCardanoScriptHash

instance (ToScript a) => ToScript (Versioned a) where
  toScript = toScript . unversioned

class ToVersioned s a where
  toVersioned :: a -> Versioned s

instance ToVersioned a (Versioned a) where
  toVersioned = id

-- | Anything that can be seen as a versioned script can be assigned a Cardano
-- address. However this address will bear no staking.
toCardanoAddressInConway :: (ToVersioned Script script) => C.Api.NetworkId -> script -> C.Api.AddressInEra C.Api.ConwayEra
toCardanoAddressInConway networkId (toVersioned @Script -> script) =
  C.Api.makeShelleyAddressInEra C.Api.shelleyBasedEra networkId (C.Api.PaymentCredentialByScript $ toCardanoScriptHash script) C.Api.NoStakeAddress

-- * Validators

-- | 'Validator' is a wrapper around 'Script's which are used as validators in transaction outputs.
newtype Validator = Validator {getValidator :: Script}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Serialise)
  deriving (Pretty) via (PrettyShow Validator)

instance Show Validator where
  show = const "Validator { <script> }"

class ToValidator a where
  toValidator :: a -> Validator

instance ToValidator Validator where
  toValidator = id

instance ToValidator Script where
  toValidator = Validator

instance ToScript Validator where
  toScript = getValidator

instance (ToValidator a) => ToValidator (Versioned a) where
  toValidator = toValidator . unversioned

-- | Instance for PlutusV1 and PlutusV2 validators
instance ToValidator (CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)) where
  toValidator = toValidator . toScript

-- * Validators hashes

newtype ValidatorHash = ValidatorHash {getValidatorHash :: Builtins.BuiltinByteString}
  deriving (IsString, Show, Pretty) via LedgerBytes
  deriving stock (Generic)
  deriving newtype (Eq, Ord, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

makeLift ''ValidatorHash

class ToValidatorHash a where
  toValidatorHash :: a -> ValidatorHash

instance ToValidatorHash ValidatorHash where
  toValidatorHash = id

instance ToValidatorHash PV1.ScriptHash where
  toValidatorHash = coerce

instance ToValidatorHash (Versioned Validator) where
  toValidatorHash = toValidatorHash . toScriptHash . fmap toScript

instance ToScriptHash ValidatorHash where
  toScriptHash = coerce

-- | CurrencySymbol and MintingPolicyHash are isomorphic, so anything that can be
-- translated to a MintingPolicyHash can be seen as a CurrencySymbol
{-# INLINEABLE scriptCredential #-}
scriptCredential :: (ToValidatorHash script) => script -> PV1.Credential
scriptCredential = PV1.ScriptCredential . coerce . toValidatorHash

-- * Minting policies

-- | 'MintingPolicy' is a wrapper around 'Script's which are used as validators for minting constraints.
newtype MintingPolicy = MintingPolicy {getMintingPolicy :: Script}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Serialise)
  deriving (Pretty) via (PrettyShow MintingPolicy)

instance Show MintingPolicy where
  show = const "MintingPolicy { <script> }"

class ToMintingPolicy a where
  toMintingPolicy :: a -> MintingPolicy

instance ToMintingPolicy MintingPolicy where
  toMintingPolicy = id

instance ToMintingPolicy Script where
  toMintingPolicy = MintingPolicy

instance ToScript MintingPolicy where
  toScript = getMintingPolicy

instance (ToMintingPolicy a) => ToMintingPolicy (Versioned a) where
  toMintingPolicy = toMintingPolicy . unversioned

-- | Instance for PlutusV1 and PlutusV2 minting policies
instance ToMintingPolicy (CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)) where
  toMintingPolicy = toMintingPolicy . toScript

-- * Minting policies hashes

-- | Script runtime representation of a @Digest SHA256@.
newtype MintingPolicyHash = MintingPolicyHash {getMintingPolicyHash :: Builtins.BuiltinByteString}
  deriving (IsString, Show, Pretty) via LedgerBytes
  deriving stock (Generic)
  deriving newtype (Eq, Ord, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

makeLift ''MintingPolicyHash

class ToMintingPolicyHash a where
  toMintingPolicyHash :: a -> MintingPolicyHash

instance ToMintingPolicyHash MintingPolicyHash where
  toMintingPolicyHash = id

instance ToMintingPolicyHash PV1.ScriptHash where
  toMintingPolicyHash = coerce

instance ToMintingPolicyHash (Versioned MintingPolicy) where
  toMintingPolicyHash = toMintingPolicyHash . toScriptHash . fmap toScript

instance ToScriptHash MintingPolicyHash where
  toScriptHash = coerce

-- | CurrencySymbol and MintingPolicyHash are isomorphic, so anything that can be
-- translated to a MintingPolicyHash can be seen as a CurrencySymbol
{-# INLINEABLE scriptCurrencySymbol #-}
scriptCurrencySymbol :: (ToMintingPolicyHash script) => script -> PV1.CurrencySymbol
scriptCurrencySymbol = coerce . toMintingPolicyHash

-- * Stake validators

-- | 'StakeValidator' is a wrapper around 'Script's which are used as validators
-- for any other purpose than Minting or Spending
newtype StakeValidator = StakeValidator {getStakeValidator :: Script}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Serialise)
  deriving (Pretty) via (PrettyShow MintingPolicy)

instance Show StakeValidator where
  show = const "StakeValidator { <script> }"

class ToStakeValidator a where
  toStakeValidator :: a -> StakeValidator

instance ToStakeValidator StakeValidator where
  toStakeValidator = id

instance ToStakeValidator Script where
  toStakeValidator = StakeValidator

instance ToScript StakeValidator where
  toScript = getStakeValidator

instance (ToStakeValidator a) => ToStakeValidator (Versioned a) where
  toStakeValidator = toStakeValidator . unversioned

instance ToStakeValidator (CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)) where
  toStakeValidator = toStakeValidator . toScript

-- * Stake validators hashes

-- | Script runtime representation of a @Digest SHA256@.
newtype StakeValidatorHash = StakeValidatorHash {getStakeValidatorHash :: Builtins.BuiltinByteString}
  deriving (IsString, Show, Pretty) via LedgerBytes
  deriving stock (Generic)
  deriving newtype (Eq, Ord, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

makeLift ''StakeValidatorHash

class ToStakeValidatorHash a where
  toStakeValidatorHash :: a -> StakeValidatorHash

instance ToStakeValidatorHash StakeValidatorHash where
  toStakeValidatorHash = id

instance ToStakeValidatorHash PV1.ScriptHash where
  toStakeValidatorHash = coerce

instance ToStakeValidatorHash (Versioned StakeValidator) where
  toStakeValidatorHash = toStakeValidatorHash . toScriptHash . fmap toScript

instance ToScriptHash StakeValidatorHash where
  toScriptHash = coerce

-- * Hashing data

-- | Hash a 'PV1.Datum builtin data.
datumHash :: PV1.Datum -> PV1.DatumHash
datumHash = PV1.DatumHash . dataHash . PV1.getDatum

-- | Hash a 'PV1.Redeemer' builtin data.
redeemerHash :: PV1.Redeemer -> PV1.RedeemerHash
redeemerHash = PV1.RedeemerHash . dataHash . PV1.getRedeemer

-- | Hash a 'Builtins.BuiltinData'
dataHash :: Builtins.BuiltinData -> Builtins.BuiltinByteString
dataHash = Builtins.toBuiltin . C.Api.serialiseToRawBytes . C.Api.hashScriptDataBytes . toCardanoAPIData

-- | Convert a 'Builtins.BuiltinsData' value to a 'cardano-api' script
--  data value.
--
-- For why we depend on `cardano-api`,
-- see note [Hash computation of datums, redeemers and scripts]
toCardanoAPIData :: Builtins.BuiltinData -> C.Api.HashableScriptData
toCardanoAPIData = C.Api.unsafeHashableScriptData . C.Api.fromPlutusData . Builtins.builtinDataToData
