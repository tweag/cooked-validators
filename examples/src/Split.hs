{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

-- | Lock and split a value equally among two recipients
module Split where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger
  ( Ada,
    PubKey,
    PubKeyHash,
    ScriptContext (ScriptContext, scriptContextTxInfo),
    pubKeyHash,
    valuePaidTo,
  )
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value (Value, gt)
import qualified Plutus.V1.Ledger.Ada as Ada
import qualified PlutusTx
import qualified PlutusTx.Lift.Class ()
import PlutusTx.Prelude hiding (Applicative (..))
import Schema (ToSchema)
import qualified Prelude as Haskell

data SplitParams = SplitParams
  { recipient1 :: PubKey,
    recipient2 :: PubKey,
    amount :: Value
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SplitDatum = SplitDatum
  { datumRecipient1 :: PubKeyHash,
    datumRecipient2 :: PubKeyHash,
    datumAmount :: Value
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

makeDatum :: SplitParams -> SplitDatum
makeDatum (SplitParams recipient1 recipient2 amount) =
  SplitDatum (pubKeyHash recipient1) (pubKeyHash recipient2) amount

type SplitRedeemer = ()

-- Note: Conversion to and from Ada to split the amount (a `Value`) in 2.
-- There may be a better way.
validateSplit :: SplitDatum -> SplitRedeemer -> ScriptContext -> Bool
validateSplit (SplitDatum r1 r2 amount) _ ScriptContext {scriptContextTxInfo} =
  let halfAda = Ada.divide (Ada.fromValue amount) 2
      isPaid :: PubKeyHash -> Ada -> Bool
      isPaid recipient ada =
        valuePaidTo scriptContextTxInfo recipient `gt` Ada.toValue ada
   in r1 `isPaid` halfAda && r2 `isPaid` (Ada.fromValue amount - halfAda)

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
