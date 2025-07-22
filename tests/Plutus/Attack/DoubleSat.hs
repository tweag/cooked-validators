{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Plutus.Attack.DoubleSat where

import Plutus.Script.Utils.V2 qualified as Script
import PlutusLedgerApi.V2 qualified as Api
import PlutusTx
import PlutusTx.Prelude
import Prelude qualified as HS

-- * Mock contracts for the double satisfaction attack

-- Scenario: There are two validators, one of type A, one of type B. We want to
-- add an input belonging to the B validator to a transaction that spends from
-- the A validator.

data ADatum = ADatum deriving (HS.Show, HS.Eq)

instance Eq ADatum where
  ADatum == ADatum = True

makeLift ''ADatum
unstableMakeIsData ''ADatum

data ARedeemer = ARedeemer1 | ARedeemer2 | ARedeemer3 deriving (HS.Show)

instance Eq ARedeemer where
  ARedeemer1 == ARedeemer1 = True
  ARedeemer2 == ARedeemer2 = True
  ARedeemer3 == ARedeemer3 = True
  _ == _ = False

makeLift ''ARedeemer
unstableMakeIsData ''ARedeemer

data AContract

instance Script.ValidatorTypes AContract where
  type DatumType AContract = ADatum
  type RedeemerType AContract = ARedeemer

{-# INLINEABLE mkAValidator #-}
mkAValidator :: ADatum -> ARedeemer -> Api.ScriptContext -> Bool
mkAValidator _ _ _ = True

aValidator :: Script.TypedValidator AContract
aValidator =
  Script.mkTypedValidator @AContract
    $$(compile [||mkAValidator||])
    $$(compile [||wrap||])
  where
    wrap = Script.mkUntypedValidator

data BDatum = BDatum deriving (HS.Show, HS.Eq)

instance Eq BDatum where
  BDatum == BDatum = True

makeLift ''BDatum
unstableMakeIsData ''BDatum

data BRedeemer = BRedeemer1 | BRedeemer2 deriving (HS.Show)

instance Eq BRedeemer where
  BRedeemer1 == BRedeemer1 = True
  BRedeemer2 == BRedeemer2 = True
  _ == _ = False

makeLift ''BRedeemer
unstableMakeIsData ''BRedeemer

data BContract

instance Script.ValidatorTypes BContract where
  type DatumType BContract = BDatum
  type RedeemerType BContract = BRedeemer

{-# INLINEABLE mkBValidator #-}
mkBValidator :: BDatum -> BRedeemer -> Api.ScriptContext -> Bool
mkBValidator _ _ _ = True

bValidator :: Script.TypedValidator BContract
bValidator =
  Script.mkTypedValidator @BContract
    $$(compile [||mkBValidator||])
    $$(compile [||wrap||])
  where
    wrap = Script.mkUntypedValidator
