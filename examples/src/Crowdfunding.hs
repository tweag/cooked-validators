{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crowdfunding where

import Control.Monad.Extra (concatMapM, guard, void)
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Aeson (FromJSON, ToJSON)
import qualified GHC.Generics as Haskell
import qualified Ledger
import qualified Ledger.Ada as Ada
import qualified Ledger.Typed.Scripts as Script
import Playground.Contract (mkSchemaDefinitions)
import Plutus.Contract
import qualified Plutus.V1.Ledger.Ada as Script
import qualified PlutusTx
import PlutusTx.Prelude
import Schema (ToSchema)
import qualified Wallet.Emulator.Wallet as Contract
import qualified Prelude as Haskell

-- DATA AND REDEEMERS
-- ==================

data ProjectData = ProjectData
  { minimalFunding :: Ledger.Ada,
    expiryDate :: Ledger.POSIXTime,
    ownerKey :: Ledger.PaymentPubKeyHash
  }
  deriving stock (Haskell.Show, Haskell.Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FundData = FundData
  { projectOwnerKey :: Ledger.PaymentPubKeyHash,
    funderKey :: Ledger.PaymentPubKeyHash
  }
  deriving stock (Haskell.Show, Haskell.Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OwnerRedeemer = GetFunds deriving stock (Haskell.Show, Haskell.Generic)

data FunderRedeemer = Cancel | Fund deriving stock (Haskell.Show, Haskell.Generic)

concatMapM PlutusTx.unstableMakeIsData [''ProjectData, ''FundData, ''OwnerRedeemer, ''FunderRedeemer]
concatMapM PlutusTx.makeLift [''ProjectData, ''FundData, ''OwnerRedeemer, ''FunderRedeemer]

-- TRANSACTION-RELATED UTILITIES
-- =============================

{-# INLINEABLE isSignedBy #-}
isSignedBy :: Ledger.ScriptContext -> Ledger.PaymentPubKeyHash -> Bool
ctx `isSignedBy` signer = Ledger.scriptContextTxInfo ctx `isSignedByTx` signer

{-# INLINEABLE isSignedByTx #-}
isSignedByTx :: Ledger.TxInfo -> Ledger.PaymentPubKeyHash -> Bool
txInfo `isSignedByTx` signer = Ledger.unPaymentPubKeyHash signer `elem` Ledger.txInfoSignatories txInfo

{-# INLINEABLE findDataOf #-}
findDataOf :: forall d. PlutusTx.FromData d => Ledger.TxInfo -> [(d, Ledger.Value)]
findDataOf tx = do
  let txIns = map Ledger.txInInfoResolved (Ledger.txInfoInputs tx)
  flip mapMaybe txIns $ \txOut -> do
    hash <- Ledger.txOutDatumHash txOut
    Ledger.Datum d <- Ledger.findDatum hash tx
    (,) <$> PlutusTx.fromBuiltinData d <*> pure (Ledger.txOutValue txOut)

{-# INLINEABLE run #-}
run :: Maybe a -> Bool
run = isJust

-- VALIDATORS
-- ==========

{-# INLINEABLE validateOwner #-}
validateOwner :: ProjectData -> OwnerRedeemer -> Ledger.ScriptContext -> Bool
validateOwner ProjectData {ownerKey} GetFunds ctx =
  ctx `isSignedBy` ownerKey

-- what should be checked here?

{-# INLINEABLE validateFunder #-}
validateFunder :: FundData -> FunderRedeemer -> Ledger.ScriptContext -> Bool
validateFunder FundData {funderKey} Cancel ctx =
  ctx `isSignedBy` funderKey
-- use the money as you wish
-- since this input is consumed we know this money
-- is never going to be used to fund a project

-- this is part of a transaction executed by the owner
-- of the project to get the funds locked from funders
validateFunder FundData {projectOwnerKey} Fund ctx = run $ do
  let tx = Ledger.scriptContextTxInfo ctx
  -- signed by the owner of the project
  guard $ tx `isSignedByTx` projectOwnerKey
  -- which is also the owner of the project
  let [(ProjectData {ownerKey, minimalFunding, expiryDate}, _)] =
        findDataOf @ProjectData tx
  guard $ ownerKey == projectOwnerKey
  -- this UTxO is spent before the expiry date
  guard $ expiryDate `Ledger.before` Ledger.txInfoValidRange tx
  -- the amount should be correct
  let funders =
        filter (\(FundData {projectOwnerKey = k}, _) -> ownerKey == k) $
          findDataOf @FundData tx
      totalAmount = Ada.fromValue $ foldMap snd funders
  guard $ totalAmount >= minimalFunding
  -- and all of it goes to the owner
  guard $ Ada.fromValue (Ledger.valuePaidTo tx (Ledger.unPaymentPubKeyHash ownerKey)) >= totalAmount

data OwnerValidator

instance Script.ValidatorTypes OwnerValidator where
  type RedeemerType OwnerValidator = OwnerRedeemer
  type DatumType OwnerValidator = ProjectData

ownerInstance :: Script.TypedValidator OwnerValidator
ownerInstance =
  Script.mkTypedValidator @OwnerValidator
    $$(PlutusTx.compile [||validateOwner||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Script.wrapValidator @ProjectData @OwnerRedeemer

data FunderValidator

instance Script.ValidatorTypes FunderValidator where
  type RedeemerType FunderValidator = FunderRedeemer
  type DatumType FunderValidator = FundData

funderInstance :: Script.TypedValidator FunderValidator
funderInstance =
  Script.mkTypedValidator @FunderValidator
    $$(PlutusTx.compile [||validateFunder||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Script.wrapValidator @FundData @FunderRedeemer

-- ENDPOINT
-- ========

data FundProjectArgs = FundProjectArgs
  { projectOwner :: Contract.Wallet,
    amount :: Ledger.Ada
  }
  deriving stock (Haskell.Show, Haskell.Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type CrowdfundingSchema =
  Endpoint "startProject" ProjectData
    .\/ Endpoint "fundProject" FundProjectArgs
    .\/ Endpoint "cancelFund" ()
    .\/ Endpoint "getFunds" ()

mkSchemaDefinitions ''CrowdfundingSchema

{-

-- attempt at writing it using Plutus

startProject :: Promise () CrowdfundingSchema T.Text ()
startProject = endpoint @"startProject" $ \projectData@ProjectData { ownerKey } -> void $
  submitTxConstraints ownerInstance $
    mconcat [ Constraints.mustBeSignedBy ownerKey
            , Constraints.mustPayToTheScript projectData mempty ]

fundProject :: Promise () CrowdfundingSchema T.Text ()
fundProject = endpoint @"fundProject" $ \FundProjectArgs { projectOwner, amount } -> void $ do
  let fundData = FundData {
    projectOwnerKey = Wallet.mockWalletPaymentPubKeyHash projectOwner
  , funderKey = Haskell.undefined
  }
  submitTxConstraints funderInstance $
    Constraints.mustPayToTheScript fundData (Ada.toValue amount)

-}

-- attempt using cooked-validators instead

txStartProject :: (MonadBlockChain m) => ProjectData -> m ()
txStartProject datum =
  void $
    validateTxConstr
      [PaysScript ownerInstance datum (Script.lovelaceValueOf 1)]

txFundProject ::
  (MonadBlockChain m, Functor m) =>
  FundProjectArgs ->
  m ()
txFundProject FundProjectArgs {projectOwner, amount} = do
  myKey <- Ledger.PaymentPubKeyHash <$> Cooked.MockChain.ownPaymentPubKeyHash
  let ownerKey = Contract.mockWalletPaymentPubKeyHash projectOwner
  void $
    validateTxConstr
      [PaysScript funderInstance (FundData ownerKey myKey) (Ada.toValue amount)]

txCancelFund :: (MonadBlockChain m) => () -> m ()
txCancelFund _ = do
  pkh <- Cooked.MockChain.ownPaymentPubKeyHash
  [(output, fd)] <- scriptUtxosSuchThat funderInstance (\fd _ -> funderKey fd == Ledger.PaymentPubKeyHash pkh)
  void $
    validateTxConstr
      ( [SpendsScript funderInstance Cancel (output, fd), SignedBy [pkh]]
          :=>: [paysPK pkh (sOutValue output)]
      )

txGetFunds :: (MonadBlockChain m) => () -> m ()
txGetFunds _ = Haskell.undefined

endpoints :: (AsContractError e) => Promise w CrowdfundingSchema e ()
endpoints =
  endpoint @"startProject" txStartProject
    `select` endpoint @"fundProject" txFundProject
    `select` endpoint @"cancelFund" txCancelFund
    `select` endpoint @"getFunds" txGetFunds
