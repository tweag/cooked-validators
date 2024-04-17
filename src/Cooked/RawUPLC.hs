{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Provides functions to create instances of 'TypedValidator' from a
--  UPLC program (either as a ShortByteString -- or its alias
--  SerialisedScript, or a Code). The "unsafe" refers to the
--  use of 'unsafeCoerce' to cast a @TypedValidator Any@ into a
--  suitable type using a @-XTypeApplications@. The programmer is
--  responsible for ensuring that the type variable @a@ gets
--  instantiated to the correct type.
module Cooked.RawUPLC
  ( typedValidatorFromBS,
    typedValidatorFromCode,
    unsafeTypedValidatorFromBS,
    unsafeTypedValidatorFromCode,
  )
where

import qualified Ledger.Typed.Scripts as Pl
import qualified Plutus.Script.Utils.Scripts as Pl
import qualified PlutusLedgerApi.V3 as Pl
import qualified PlutusTx as Pl
import Unsafe.Coerce

unsafeTypedValidatorFromBS :: forall a. Pl.SerialisedScript -> Pl.TypedValidator a
unsafeTypedValidatorFromBS = unsafeCoerce . typedValidatorFromBS

unsafeTypedValidatorFromCode :: forall a. (Pl.BuiltinData -> Pl.BuiltinData -> Pl.BuiltinData -> ()) -> Pl.TypedValidator a
unsafeTypedValidatorFromCode = unsafeCoerce . typedValidatorFromCode

typedValidatorFromBS :: Pl.SerialisedScript -> Pl.TypedValidator Pl.Any
typedValidatorFromBS = Pl.unsafeMkTypedValidator . flip Pl.Versioned Pl.PlutusV3 . Pl.Validator . Pl.Script

typedValidatorFromCode :: (Pl.BuiltinData -> Pl.BuiltinData -> Pl.BuiltinData -> ()) -> Pl.TypedValidator Pl.Any
typedValidatorFromCode validator = typedValidatorFromBS $ Pl.serialiseCompiledCode $$(Pl.compile [||validator||])
