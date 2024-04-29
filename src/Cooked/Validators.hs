{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module introduces standard dummy validators to be used in
-- attacks, traces or tests. More precisely, it introduces the always
-- True and always False validators, which will respectively always
-- succeed or always fail.
module Cooked.Validators
  ( alwaysTrueValidator,
    alwaysFalseValidator,
    MockContract,
  )
where

import qualified Plutus.Script.Utils.Scripts as Pl
import qualified Plutus.Script.Utils.Typed as Pl hiding (validatorHash)
import qualified Plutus.Script.Utils.V3.Typed.Scripts.MonetaryPolicies as Pl
import qualified Plutus.Script.Utils.V3.Typed.Scripts.Validators as Pl hiding (validatorHash)
import qualified PlutusLedgerApi.V3 as Pl
import qualified PlutusTx as Pl
import qualified PlutusTx.Prelude as Pl
import Unsafe.Coerce (unsafeCoerce)

validatorToTypedValidator :: Pl.Validator -> Pl.TypedValidator a
validatorToTypedValidator val =
  Pl.TypedValidator
    { Pl.tvValidator = vValidator,
      Pl.tvValidatorHash = vValidatorHash,
      Pl.tvForwardingMPS = vMintingPolicy,
      Pl.tvForwardingMPSHash = Pl.mintingPolicyHash vMintingPolicy
    }
  where
    vValidator = Pl.Versioned val Pl.PlutusV3
    vValidatorHash = Pl.validatorHash vValidator
    forwardingPolicy = Pl.mkForwardingMintingPolicy vValidatorHash
    vMintingPolicy = Pl.Versioned forwardingPolicy Pl.PlutusV3

-- -- | Everything that can be exported as a typed validator
-- data IsTypedValidator a where
--   ItvSerialisedScript :: Pl.SerialisedScript -> IsTypedValidator a
--   ItvScript :: Pl.Script -> IsTypedValidator a
--   ItvValidator :: Pl.Validator -> IsTypedValidator a
--   ItvUntypedValidator :: Pl.UntypedValidator -> IsTypedValidator a
--   ItvValidatorType :: (Pl.UnsafeFromData (Pl.DatumType a), Pl.UnsafeFromData (Pl.RedeemerType a)) => Pl.ValidatorType a -> IsTypedValidator a

-- toTypedValidator :: IsTypedValidator a -> Pl.TypedValidator a
-- toTypedValidator (ItvSerialisedScript sScript) = toTypedValidator $ ItvScript $ Pl.Script sScript
-- toTypedValidator (ItvScript script) = toTypedValidator $ ItvValidator $ Pl.Validator script
-- toTypedValidator (ItvValidator val) =
--   Pl.TypedValidator
--     { Pl.tvValidator = vValidator,
--       Pl.tvValidatorHash = vValidatorHash,
--       Pl.tvForwardingMPS = vMintingPolicy,
--       Pl.tvForwardingMPSHash = Pl.mintingPolicyHash vMintingPolicy
--     }
--   where
--     vValidator = Pl.Versioned val Pl.PlutusV3
--     vValidatorHash = Pl.validatorHash vValidator
--     forwardingPolicy = Pl.mkForwardingMintingPolicy vValidatorHash
--     vMintingPolicy = Pl.Versioned forwardingPolicy Pl.PlutusV3
-- toTypedValidator (ItvUntypedValidator val) = toTypedValidator $ ItvValidator $ Pl.mkValidatorScript $$(Pl.compile [||val||])
-- toTypedValidator (ItvValidatorType val) =
--   Pl.mkTypedValidator
--     $$(Pl.compile [||val||])
--     $$(Pl.compile [||wrap||])
--   where
--     wrap = Pl.mkUntypedValidator

-- | The trivial validator that always succeds; this is in particular
-- a sufficient target for the datum hijacking attack since we only
-- want to show feasibility of the attack.
mkAlwaysTrueValidator :: Pl.TypedValidator Pl.Any
mkAlwaysTrueValidator =
  Pl.mkTypedValidator @Pl.Any
    $$(Pl.compile [||val||])
    $$(Pl.compile [||wrap||])
  where
    val _ _ _ = True
    wrap = Pl.mkUntypedValidator

alwaysTrueValidator :: Pl.TypedValidator a
alwaysTrueValidator = unsafeCoerce mkAlwaysTrueValidator

-- -- | The trivial validator that always fails
-- alwaysFalseValidator :: Pl.TypedValidator a
-- alwaysFalseValidator = toTypedValidator $ ItvUntypedValidator (\_ _ _ -> Pl.error ())

mkAlwaysFalseValidator :: Pl.TypedValidator Pl.Any
mkAlwaysFalseValidator =
  Pl.mkTypedValidator @Pl.Any
    $$(Pl.compile [||val||])
    $$(Pl.compile [||wrap||])
  where
    val _ _ _ = False
    wrap = Pl.mkUntypedValidator

alwaysFalseValidator :: Pl.TypedValidator a
alwaysFalseValidator = unsafeCoerce mkAlwaysTrueValidator

-- -- | A Mock contract type to instantiate validators with
data MockContract

instance Pl.ValidatorTypes MockContract where
  type RedeemerType MockContract = ()
  type DatumType MockContract = ()
