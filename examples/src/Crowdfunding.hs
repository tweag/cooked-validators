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

import Control.Monad.Extra (guard, concatMapM)
import Data.Aeson
import PlutusTx.Prelude
import qualified Prelude as Haskell
import qualified GHC.Generics as Haskell

import qualified PlutusTx
import qualified Ledger
import qualified Ledger.Ada as Ada
import qualified Ledger.Typed.Scripts as Script
import Plutus.Contract
import Schema (ToSchema)

-- DATA AND REDEEMERS
-- ==================

data ProjectData =
  ProjectData {
    minimalFunding :: Ledger.Ada
  , expiryDate     :: Ledger.POSIXTime
  , ownerKey       :: Ledger.PubKeyHash
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

-- TRANSACTION-RELATED UTILITIES
-- =============================

{-# INLINABLE isSignedBy #-}
isSignedBy :: Ledger.ScriptContext -> Ledger.PubKeyHash -> Bool
ctx `isSignedBy` signer = Ledger.scriptContextTxInfo ctx `isSignedByTx` signer

{-# INLINABLE isSignedByTx #-}
isSignedByTx :: Ledger.TxInfo -> Ledger.PubKeyHash -> Bool
txInfo `isSignedByTx` signer = signer `elem` Ledger.txInfoSignatories txInfo

{-# INLINABLE findDataOf #-}
findDataOf :: forall d. PlutusTx.FromData d => Ledger.TxInfo -> [(d, Ledger.Value)]
findDataOf tx = do
  let txIns = map Ledger.txInInfoResolved (Ledger.txInfoInputs tx)
  flip mapMaybe txIns $ \txOut -> do
    hash <- Ledger.txOutDatumHash txOut
    Ledger.Datum d <- Ledger.findDatum hash tx
    (,) <$> PlutusTx.fromBuiltinData d <*> pure (Ledger.txOutValue txOut)

{-# INLINABLE run #-}
run :: Maybe a -> Bool
run = isJust

-- VALIDATORS
-- ==========

{-# INLINABLE validateOwner #-}
validateOwner :: ProjectData -> OwnerRedeemer -> Ledger.ScriptContext -> Bool
validateOwner ProjectData { ownerKey } GetFunds ctx =
  ctx `isSignedBy` ownerKey
  -- what should be checked here?

{-# INLINABLE validateFunder #-}
validateFunder :: FundData -> FunderRedeemer -> Ledger.ScriptContext -> Bool

validateFunder FundData { funderKey } Cancel ctx =
  ctx `isSignedBy` funderKey
  -- use the money as you wish
  -- since this input is consumed we know this money
  -- is never going to be used to fund a project

-- this is part of a transaction executed by the owner
-- of the project to get the funds locked from funders
validateFunder FundData { projectOwnerKey } Fund ctx = run $ do
  let tx = Ledger.scriptContextTxInfo ctx
  -- signed by the owner of the project
  guard $ tx `isSignedByTx` projectOwnerKey
  -- which is also the owner of the project
  let [ (ProjectData { ownerKey, minimalFunding, expiryDate }, _) ] 
        = findDataOf @ProjectData tx
  guard $ ownerKey == projectOwnerKey
  -- this UTxO is spent before the expiry date
  guard $ expiryDate `Ledger.before` Ledger.txInfoValidRange tx
  -- the amount should be correct
  let funders     = findDataOf @FundData tx
      totalAmount = Ada.fromValue $ foldMap snd funders
  guard $ totalAmount >= minimalFunding
  -- and all of it goes to the owner
  guard $ Ada.fromValue (Ledger.valuePaidTo tx ownerKey) >= totalAmount

data OwnerValidator
instance Script.ValidatorTypes OwnerValidator where
  type instance RedeemerType OwnerValidator = OwnerRedeemer
  type instance DatumType    OwnerValidator = ProjectData

ownerInstance :: Script.TypedValidator OwnerValidator
ownerInstance = Script.mkTypedValidator @OwnerValidator
    $$(PlutusTx.compile [|| validateOwner ||])
    $$(PlutusTx.compile [|| wrap ||])
    where wrap = Script.wrapValidator @ProjectData @OwnerRedeemer

data FunderValidator
instance Script.ValidatorTypes FunderValidator where
  type instance RedeemerType FunderValidator = FunderRedeemer
  type instance DatumType    FunderValidator = FundData

funderInstance :: Script.TypedValidator FunderValidator
funderInstance = Script.mkTypedValidator @FunderValidator
    $$(PlutusTx.compile [|| validateFunder ||])
    $$(PlutusTx.compile [|| wrap ||])
    where wrap = Script.wrapValidator @FundData @FunderRedeemer

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

