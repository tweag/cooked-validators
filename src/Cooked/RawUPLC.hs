{-# LANGUAGE RankNTypes #-}

module Cooked.RawUPLC
  ( unsafeTypedValidatorFromUPLC,
    typedValidatorFromUPLC,
    unsafeTypedValidatorFromBS,
    typedValidatorFromBS,
  )
where

import qualified Data.ByteString as BS
import qualified Flat
import Ledger.Scripts (Language (PlutusV2), Script (..), Validator (..), Versioned (Versioned))
import qualified Ledger.Typed.Scripts as TScripts
import Unsafe.Coerce
import qualified UntypedPlutusCore as UPLC

-- | Returns a 'TypedValidator' from a UPLC program. The "unsafe" refers to the use of 'unsafeCoerce'
--  to cast a @TypedValidator Any@, resulting from 'typedValidatorFromUPLC', to a @TypedValidator a@. This
--  enables us to avoid using 'fromBuiltinData' and 'toBuiltinData' all the time.
--
--  This function is meant to be used with @-XTypeApplications@ and the programmer is responsible for
--  ensuring that the type variable @a@, below, gets instantiated
--  to the correct type.
unsafeTypedValidatorFromUPLC ::
  forall a.
  UPLC.Program UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun () ->
  TScripts.TypedValidator a
unsafeTypedValidatorFromUPLC = unsafeCoerce . typedValidatorFromUPLC

-- | Returns a 'TypedValidator' from a UPLC program. The resulting typed validator is instantiated to 'TScripts.Any',
--  which means all of its arguments receive a value of type 'BuiltinData'.
--
-- TODO: At the moment this wraps everything as a PlutusV2 script. Make this more flexible.
typedValidatorFromUPLC ::
  UPLC.Program UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun () ->
  TScripts.TypedValidator TScripts.Any
typedValidatorFromUPLC = TScripts.unsafeMkTypedValidator . flip Versioned PlutusV2 . Validator . fromPlc
  where
    -- copied from: github.com/input-output-hk/plutus/blob/1f31e640e8a258185db01fa899da63f9018c0e85/plutus-ledger-api/src/Plutus/V1/Ledger/Scripts.hs#L169
    fromPlc :: UPLC.Program UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun () -> Script
    fromPlc (UPLC.Program a v t) =
      let nameless = UPLC.termMapNames UPLC.unNameDeBruijn t
       in Script $ UPLC.Program a v nameless

-- | Loads a typed validator from a bytestring that was produced by 'Flat.flat' the outputs
--  of [getPlc](https://github.com/input-output-hk/plutus/blob/master/plutus-tx/src/PlutusTx/Code.hs#L84)
--  applied to a 'TScripts.mkTypedValidator'. If the compiled validator was /not/ wrapped,
--  stick to 'typedValidatorFromBS'.
unsafeTypedValidatorFromBS :: forall a. BS.ByteString -> Either String (TScripts.TypedValidator a)
unsafeTypedValidatorFromBS = either (Left . show) (Right . unsafeTypedValidatorFromUPLC) . Flat.unflat

-- | Loads a typed validator from a bytestring that was produced by 'Flat.flat' the outputs
--  of [getPlc](https://github.com/input-output-hk/plutus/blob/master/plutus-tx/src/PlutusTx/Code.hs#L84).
typedValidatorFromBS :: BS.ByteString -> Either String (TScripts.TypedValidator TScripts.Any)
typedValidatorFromBS = either (Left . show) (Right . unsafeTypedValidatorFromUPLC) . Flat.unflat
