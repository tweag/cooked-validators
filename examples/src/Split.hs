{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

-- | Lock an amount and split it equally among two recipients.
module Split where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Ledger
import qualified Ledger.Ada as Ada
import Plutus.V1.Ledger.Contexts (ScriptContext (..))
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value (geq)
import qualified PlutusTx
import PlutusTx.Prelude hiding (Applicative (..))
import Schema (ToSchema)
import qualified Prelude as Haskell

data SplitDatum = SplitDatum
  { recipient1 :: Ledger.PubKeyHash,
    recipient2 :: Ledger.PubKeyHash,
    amount :: Integer
  }
  deriving stock (Haskell.Show, Haskell.Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Eq SplitDatum where
  SplitDatum r1'1 r2'1 a1 == SplitDatum r1'2 r2'2 a2 =
    r1'1 == r1'2
      && r2'1 == r2'2
      && a1 == a2

type SplitRedeemer = ()

{-# INLINEABLE validateSplit #-}
validateSplit :: SplitDatum -> SplitRedeemer -> ScriptContext -> Bool
validateSplit (SplitDatum r1 r2 amount) _ ScriptContext {scriptContextTxInfo} =
  let halfAda = divide amount 2
      isPaid :: Ledger.PubKeyHash -> Integer -> Bool
      isPaid recipient ada =
        Ledger.valuePaidTo scriptContextTxInfo recipient `Value.geq` Ada.lovelaceValueOf ada
   in traceIfFalse "R1 not paid enough" (r1 `isPaid` halfAda)
        && traceIfFalse "R2 not paid enough" (r2 `isPaid` (amount - halfAda))

-- Plutus boilerplate

data Split

PlutusTx.makeLift ''SplitDatum
PlutusTx.unstableMakeIsData ''SplitDatum

instance Scripts.ValidatorTypes Split where
  type RedeemerType Split = ()
  type DatumType Split = SplitDatum

splitValidator :: Scripts.TypedValidator Split
splitValidator =
  Scripts.mkTypedValidator @Split
    $$(PlutusTx.compile [||validateSplit||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator @SplitDatum @SplitRedeemer
