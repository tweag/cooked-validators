{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Plutus.Script.Utils.Scripts qualified as Pl
import Plutus.Script.Utils.Typed qualified as Pl hiding (validatorHash)
import Plutus.Script.Utils.V3.Generators qualified as Pl
import Plutus.Script.Utils.V3.Typed.Scripts.MonetaryPolicies qualified as Pl
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.TH qualified as PlutusTx

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

-- | The trivial validator that always succeds; this is in particular
-- a sufficient target for the datum hijacking attack since we only
-- want to show feasibility of the attack.
alwaysTrueValidator :: forall a. Pl.TypedValidator a
alwaysTrueValidator = validatorToTypedValidator @a Pl.alwaysSucceedValidator

-- -- | The trivial validator that always fails
alwaysFalseValidator :: forall a. Pl.TypedValidator a
alwaysFalseValidator = validatorToTypedValidator @a $ Pl.mkValidatorScript $$(PlutusTx.compile [||\_ _ _ -> PlutusTx.error ()||])

-- -- | A Mock contract type to instantiate validators with
data MockContract

instance Pl.ValidatorTypes MockContract where
  type RedeemerType MockContract = ()
  type DatumType MockContract = ()
