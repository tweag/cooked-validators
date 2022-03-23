{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language DeriveGeneric #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language NoImplicitPrelude #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
module Crowdfunding where

import Control.Monad.Extra (concatMapM)
import Data.Aeson
import PlutusTx.Prelude
import qualified Prelude as Haskell
import qualified GHC.Generics as Haskell

import qualified PlutusTx
import qualified Ledger
import Ledger.Typed.Scripts
import Plutus.Contract
import Schema (ToSchema)

-- DATA AND REDEEMERS
-- ==================

data ProjectData =
  ProjectData {
    minimalFunding :: Ledger.Ada
  , expiryDate     :: Ledger.POSIXTimeRange
  }
  deriving stock (Haskell.Show, Haskell.Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FundData =
  FundData {
    projectOwnerKey :: Ledger.PubKeyHash
  , funderKey       :: Ledger.PubKeyHash
  }

data OwnerRedeemer  = GetFunds
data FunderRedeemer = Cancel | Fund

concatMapM PlutusTx.unstableMakeIsData [''ProjectData, ''FundData, ''OwnerRedeemer, ''FunderRedeemer]
concatMapM PlutusTx.makeLift [''ProjectData, ''FundData, ''OwnerRedeemer, ''FunderRedeemer]

-- VALIDATORS
-- ==========

validateOwner :: ProjectData -> OwnerRedeemer -> Ledger.ScriptContext -> Bool
validateOwner (ProjectData _ _) GetFunds _ = False

validateFunder :: FundData -> FunderRedeemer -> Ledger.ScriptContext -> Bool
validateFunder FundData { funderKey } Cancel ctx = do
  let signers = Ledger.txInfoSignatories (Ledger.scriptContextTxInfo ctx)
  funderKey `elem` signers
validateFunder FundData { projectOwnerKey } Fund _ = False

data OwnerValidator
instance ValidatorTypes OwnerValidator where
  type instance RedeemerType OwnerValidator = OwnerRedeemer
  type instance DatumType    OwnerValidator = ProjectData

ownerInstance :: TypedValidator OwnerValidator
ownerInstance = mkTypedValidator @OwnerValidator
    $$(PlutusTx.compile [|| validateOwner ||])
    $$(PlutusTx.compile [|| wrap ||])
    where wrap = wrapValidator

data FunderValidator
instance ValidatorTypes FunderValidator where
  type instance RedeemerType FunderValidator = FunderRedeemer
  type instance DatumType    FunderValidator = FundData

funderInstance :: TypedValidator FunderValidator
funderInstance = mkTypedValidator @FunderValidator
    $$(PlutusTx.compile [|| validateFunder ||])
    $$(PlutusTx.compile [|| wrap ||])
    where wrap = wrapValidator

-- ENDPOINT
-- ========

data FundProjectArgs =
  FundProjectArgs {
    projectOwner :: Ledger.PaymentPubKeyHash
  , amount       :: Ledger.Ada
  }
  deriving stock (Haskell.Show, Haskell.Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type CrowdfundingSchema =
  Endpoint "startProject" ProjectData
  .\/ Endpoint "fundProject" FundProjectArgs
  .\/ Endpoint "cancelFund" ()
  .\/ Endpoint "getFunds"   ()

