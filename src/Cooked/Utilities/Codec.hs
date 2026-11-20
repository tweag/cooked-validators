{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

-- | This module provides a registry for codecs that can be used to serialize
-- and deserialize data types. These codecs don't explicitly expose any
-- serialization or deserialization functions, which are expected to be provided
-- through the typeclass constraints on the data types themselves. The registry
-- is used to keep track of the available codecs and their associated types.
module Cooked.Utilities.Codec
  ( Codec (..),
    Registry,
    HasTypeKey (..),
    registerType,
    mkRegistry,
  )
where

import Data.Foldable (foldl')
import Data.Kind (Constraint, Type)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Typeable (Proxy (..), Typeable, splitTyConApp, tyConModule, tyConName, typeRep)
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.V1.Typed qualified as Script (TypedValidator)
import Plutus.Script.Utils.V3 qualified as V3
import PlutusLedgerApi.V3 qualified as Api

-- | A 'Codec' is a wrapper around a type that satisfies a given constraint, and
-- is used to register types in a 'Registry'. The type must also satisfy
-- 'HasTypeKey', which is not technically needed, but requiring it is a worthy
-- tradeoff to ease the building of registries through 'mkRegistry'.
data Codec (c :: Type -> Constraint) where
  Codec :: forall t c. (c t, HasTypeKey t) => Codec c

-- | A 'Registry' is a mapping from type names to their corresponding 'Codec's.
type Registry (c :: Type -> Constraint) = Map String (Codec c)

-- | The key under which a type is registered in a 'Registry' and serialized.
-- There is no catch-all instance, so that the key of a type is always uniquely
-- determined, regardless of the instances in scope. A default key is however
-- provided for every 'Typeable' type (that is, every concrete type), so that
-- an empty instance, possibly derived, is enough:
--
-- @
-- instance HasTypeKey MyDatum
-- @
--
-- or, with @DeriveAnyClass@:
--
-- @
-- data MyDatum = ... deriving anyclass (HasTypeKey)
-- @
--
-- This default key is built from the fully qualified names of the type
-- constructors involved, so that homonymous types from different modules do not
-- collide. For instance, the default key of @Versioned Script@ would be
-- @"Plutus.Script.Utils.Scripts.Versioned Plutus.Script.Utils.Scripts.Script"@.
-- Note that the modules are the defining ones, which might be internal, so
-- moving a type to another module changes its default key. When the key needs
-- to be stable, it should be given explicitly:
--
-- @
-- instance HasTypeKey MyDatum where
--   typeKey = "my-datum"
-- @
class HasTypeKey t where
  -- | The key associated with the type
  typeKey :: String
  default typeKey :: (Typeable t) => String
  typeKey = go $ typeRep $ Proxy @t
    where
      go rep =
        let (tyCon, args) = splitTyConApp rep
         in unwords $ (tyConModule tyCon <> "." <> tyConName tyCon) : (paren <$> args)
      paren rep = case splitTyConApp rep of
        (_, []) -> go rep
        _ -> "(" <> go rep <> ")"

instance HasTypeKey () where
  typeKey = "()"

instance HasTypeKey Integer where
  typeKey = "Integer"

instance HasTypeKey Bool where
  typeKey = "Bool"

instance HasTypeKey Api.BuiltinByteString where
  typeKey = "BuiltinByteString"

instance (HasTypeKey a, HasTypeKey b) => HasTypeKey (a, b) where
  typeKey = "(" <> typeKey @a <> ", " <> typeKey @b <> ")"

instance HasTypeKey Api.BuiltinData where
  typeKey = "BuiltinData"

instance HasTypeKey (Script.Versioned Script.Script) where
  typeKey = "Versioned Script"

instance HasTypeKey (Script.Versioned Script.MintingPolicy) where
  typeKey = "Versioned MintingPolicy"

instance HasTypeKey (Script.Versioned Script.Validator) where
  typeKey = "Versioned Validator"

instance HasTypeKey (Script.Versioned Script.StakeValidator) where
  typeKey = "Versioned StakeValidator"

instance HasTypeKey V3.MultiPurposeScript where
  typeKey = "MultiPurposeScript"

instance HasTypeKey Api.PubKeyHash where
  typeKey = "PubKeyHash"

-- | Typed validators get the default key, which mentions their (usually
-- user-defined) type tag @a@.
instance (Typeable a) => HasTypeKey (Script.TypedValidator a)

-- | Register a type in the registry. The type must satisfy the given
-- constraint. If a codec was already registered under the same key (see
-- 'typeKey'), it is overridden.
registerType :: forall e c. (HasTypeKey e, c e) => Registry c -> Registry c
registerType = Map.insert (typeKey @e) (Codec @e)

-- | Create a registry from a list of codecs satisfying the given constraint.
-- Codecs are registered from left to right, so later codecs override earlier
-- ones registered under the same key (see 'typeKey').
mkRegistry :: [Codec c] -> Registry c
mkRegistry = foldl' (\reg (Codec @e) -> registerType @e reg) Map.empty
