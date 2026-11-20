{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module provides registries for datums, pubkeys and scripts that can be
-- used to persists, and in particular deserialize
-- 'Cooked.Skeleton.Output.TxSkelOut's. Although we have sensible defaults for
-- pubkeys and scripts, the user is expected to register their own,
-- domain-specific datums, unless they are using the default 'Api.BuiltinData'
-- datum type.
module Cooked.Runtime.Registries
  ( Registries (..),
    emptyRegistries,
    defaultRegistries,
    datumRegistryL,
    extendDatumRegistry,
    pubKeyRegistryL,
    extendPubKeyRegistry,
    scriptRegistryL,
    extendScriptRegistry,
  )
where

import Cooked.Skeleton.Datum
import Cooked.Skeleton.Signatory
import Cooked.Skeleton.User
import Cooked.Utilities.Codec
import Cooked.Utilities.Wallet
import Data.Map qualified as Map
import Optics.Core
import Optics.TH
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.V3 qualified as V3
import PlutusLedgerApi.V3 qualified as Api

-- | A 'Registries' is a collection of registries for datums, pubkeys and
-- scripts used to deserialize 'Cooked.Skeleton.Output.TxSkelOut's.
data Registries = Registries
  { datumRegistry :: DatumRegistry,
    pubKeyRegistry :: PubKeyRegistry,
    scriptRegistry :: ScriptRegistry
  }

-- | An empty 'Registries' with no registered codecs
emptyRegistries :: Registries
emptyRegistries =
  Registries
    { datumRegistry = Map.empty,
      pubKeyRegistry = Map.empty,
      scriptRegistry = Map.empty
    }

-- | A 'Registries' with default codecs registered for pubkeys and scripts, but
-- no existing codecs for datums, except for the 'Api.BuiltinData' datum type.
defaultRegistries :: Registries
defaultRegistries =
  Registries
    { datumRegistry =
        mkRegistry
          [ Codec @Api.BuiltinData,
            Codec @()
          ],
      pubKeyRegistry =
        mkRegistry
          [ Codec @Wallet,
            Codec @TxSkelSignatory,
            Codec @Api.PubKeyHash
          ],
      scriptRegistry =
        mkRegistry
          [ Codec @V3.MultiPurposeScript,
            Codec @(Script.Versioned Script.Script),
            Codec @(Script.Versioned Script.MintingPolicy),
            Codec @(Script.Versioned Script.Validator),
            Codec @(Script.Versioned Script.StakeValidator)
          ]
    }

-- | Combines registries pointwise. Codecs from the right-hand side override
-- those from the left-hand side registered under the same key (see
-- 'Cooked.Utilities.Codec.typeKey'), so that @r1 <> r2@ amounts to extending
-- @r1@ with @r2@.
instance Semigroup Registries where
  r1 <> r2 =
    Registries
      { datumRegistry = datumRegistry r2 <> datumRegistry r1,
        pubKeyRegistry = pubKeyRegistry r2 <> pubKeyRegistry r1,
        scriptRegistry = scriptRegistry r2 <> scriptRegistry r1
      }

instance Monoid Registries where
  mempty = emptyRegistries

-- | Focuses on the datum registry of a 'Registries'
makeLensesFor [("datumRegistry", "datumRegistryL")] ''Registries

-- | Focuses on the pubkey registry of a 'Registries'
makeLensesFor [("pubKeyRegistry", "pubKeyRegistryL")] ''Registries

-- | Focuses on the script registry of a 'Registries'
makeLensesFor [("scriptRegistry", "scriptRegistryL")] ''Registries

-- | Adds new datum codecs to a 'Registries'. The new codecs override the
-- existing ones registered under the same key (see
-- 'Cooked.Utilities.Codec.typeKey').
extendDatumRegistry :: DatumRegistry -> Registries -> Registries
extendDatumRegistry dr = over datumRegistryL (dr <>)

-- | Adds new pubkey codecs to a 'Registries'. The new codecs override the
-- existing ones registered under the same key (see
-- 'Cooked.Utilities.Codec.typeKey').
extendPubKeyRegistry :: PubKeyRegistry -> Registries -> Registries
extendPubKeyRegistry pkr = over pubKeyRegistryL (pkr <>)

-- | Adds new script codecs to a 'Registries'. The new codecs override the
-- existing ones registered under the same key (see
-- 'Cooked.Utilities.Codec.typeKey').
extendScriptRegistry :: ScriptRegistry -> Registries -> Registries
extendScriptRegistry sr = over scriptRegistryL (sr <>)
