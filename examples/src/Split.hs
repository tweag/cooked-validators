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
import Ledger
  ( PubKey,
    PubKeyHash,
    ScriptContext (ScriptContext, scriptContextTxInfo),
    pubKeyHash,
    valuePaidTo,
  )
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value (geq)
import qualified Plutus.V1.Ledger.Ada as Ada
import qualified PlutusTx
import qualified PlutusTx.Lift.Class ()
import PlutusTx.Prelude hiding (Applicative (..))
import Schema (ToSchema)
import qualified Prelude as Haskell

data SplitParams = SplitParams
  { recipient1 :: PubKey,
    recipient2 :: PubKey,
    amount :: Integer
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SplitDatum = SplitDatum
  { datumRecipient1 :: PubKeyHash,
    datumRecipient2 :: PubKeyHash,
    datumAmount :: Integer
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

makeDatum :: SplitParams -> SplitDatum
makeDatum (SplitParams recipient1 recipient2 amount) =
  SplitDatum (pubKeyHash recipient1) (pubKeyHash recipient2) amount

type SplitRedeemer = ()

{-# INLINEABLE validateSplit #-}
validateSplit :: SplitDatum -> SplitRedeemer -> ScriptContext -> Bool
validateSplit (SplitDatum r1 r2 amount) _ ScriptContext {scriptContextTxInfo} =
  let halfAda = divide amount 2
      isPaid :: PubKeyHash -> Integer -> Bool
      isPaid recipient ada =
        valuePaidTo scriptContextTxInfo recipient `geq` Ada.lovelaceValueOf ada
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
    wrap = Scripts.wrapValidator @SplitDatum @SplitRedeemer
