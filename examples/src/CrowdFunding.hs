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

-- A smart contract to model a crowd funding project with a deadline
module CrowdFunding where

-- Plutus related imports
import qualified Ledger
import qualified Ledger.Contexts as Context
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import qualified Ledger.Ada as Ada
import qualified Ledger.Credential as Credential
import qualified Ledger.Address as Address
import PlutusTx.Prelude hiding (Applicative (..))

-- Some basic types
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Prelude as Haskell

{- The datum for the CrowdFunding smart contract. A peer can either :
* propose a project with its address, an id, a goal and a time range
* contribute to a project with its address and an id -}
data CrowdFundingDatum =
  ProjectProposal Ledger.PubKeyHash BuiltinByteString Integer Ledger.POSIXTimeRange |
  Funding Ledger.PubKeyHash BuiltinByteString
  deriving stock    (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Some magic so that Plutus understands that CrowdFundingDatum is supposed to be used as a Datum
PlutusTx.unstableMakeIsData ''CrowdFundingDatum
PlutusTx.makeLift ''CrowdFundingDatum

{-# INLINEABLE findDatum #-}
-- Retrieving a CrowdFundingDatum of a given input in a transaction
findDatum :: Ledger.TxInfo -> Ledger.TxInInfo -> Maybe CrowdFundingDatum
findDatum txInfo txInInfo = do
  let txOut = Ledger.txInInfoResolved txInInfo
  hash <- Ledger.txOutDatumHash txOut
  Ledger.Datum d <- Context.findDatum hash txInfo
  PlutusTx.fromBuiltinData d

{-# INLINEABLE catMaybes #-}
catMaybes :: [ Maybe a ] -> [ a ]
catMaybes [] = []
catMaybes (Nothing : l) = catMaybes l
catMaybes ((Just x) : l) = x : catMaybes l
  
{-# INLINEABLE findAllDatums #-}
-- Retrieves all the instances of CrowdFundingDatum in a script
findAllDatums :: Ledger.TxInfo -> [ CrowdFundingDatum ]
findAllDatums txInfo = catMaybes (fmap (findDatum txInfo) (Ledger.txInfoInputs txInfo))
  
{- The redeemer for the CrowdFunding smart contract. The project can either be :
Launched or cancelled -}
data CrowdFundingRedeemer =
  Launch | Cancel
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)  

-- The same magic for the redeemer
PlutusTx.unstableMakeIsData ''CrowdFundingRedeemer
PlutusTx.makeLift ''CrowdFundingRedeemer

{-# INLINEABLE getCurrentValue #-}
-- Retrieves the value for the current input
getCurrentValue :: Context.ScriptContext -> Maybe Ledger.Value
getCurrentValue =
  (fmap (Ledger.txOutValue . Context.txInInfoResolved)) .
  Context.findOwnInput

{-# INLINEABLE validateTimeRange #-}
-- Retrieves the current transition time range and compares it to the project time range
validateTimeRange :: Ledger.POSIXTimeRange -> Context.ScriptContext -> Bool
validateTimeRange range =
  (== range) .
  (\/ range) .
  Context.txInfoValidRange .
  Context.scriptContextTxInfo

{-# INLINEABLE validateTotalSum #-}
-- The sum of all input lovelaces should be greater than the threshold
validateTotalSum :: Integer -> Context.ScriptContext -> Bool
validateTotalSum threshold =
  (<= threshold) .
  Ada.getLovelace .
  Ada.fromValue .
  mconcat .
  fmap (Context.txOutValue . Context.txInInfoResolved) .
  Context.txInfoInputs .
  Context.scriptContextTxInfo

{-# INLINEABLE validateSingleProposal #-}
-- Checks that the context only contains a single instance of a project proposal
-- and that all fundings are directed towards that proposal. This also checks
-- that there are no additionnal irrelevant inputs
validateSingleProposal :: BuiltinByteString -> Context.ScriptContext -> Bool
validateSingleProposal name ctx =
  let txInfo = Ledger.scriptContextTxInfo ctx in
    let txInInfos = Ledger.txInfoInputs txInfo in 
      let datums = findAllDatums txInfo in
        length datums == length txInInfos &&
        validateDatumsContent Nothing Nothing datums
  where
    validateDatumsContent :: Maybe BuiltinByteString -> Maybe BuiltinByteString -> [ CrowdFundingDatum ] -> Bool
    validateDatumsContent (Just ppName) (Just fpName) [] = ppName == fpName && ppName == name
    validateDatumsContent (Just _) Nothing [] = True
    validateDatumsContent Nothing _ [] = False
    validateDatumsContent (Just _) _ ((ProjectProposal _ _ _ _) : _) = False
    validateDatumsContent Nothing mfpName ((ProjectProposal _ nppName _ _) : l) =
      validateDatumsContent (Just nppName) mfpName l
    validateDatumsContent mppName (Just fpName) ((Funding _ nfpName) : l) =
      if (nfpName == fpName) then validateDatumsContent mppName (Just fpName) l else False
    validateDatumsContent mppName Nothing ((Funding _ nfpName) : l) =
      validateDatumsContent mppName (Just nfpName) l
  
{-# INLINEABLE validateSingleElt #-}
-- Checks if there is a single input or output in the context
validateSingleElt :: Bool -> Context.ScriptContext -> Bool
validateSingleElt isInput ctx = let txInfo = Ledger.scriptContextTxInfo ctx in
  if isInput then sizeOne $ Ledger.txInfoInputs $ txInfo else sizeOne $ Ledger.txInfoOutputs $ txInfo
  where
    sizeOne :: [ a ] -> Bool
    sizeOne = (== 1) . length
    
{-# INLINEABLE validateOwnerSignature #-}
-- Checks if a PubKeyHash appears in the signatures of a script
validateOwnerSignature :: Ledger.PubKeyHash -> Context.ScriptContext -> Bool
validateOwnerSignature pkh = 
    (elem pkh) .
    Ledger.txInfoSignatories .
    Ledger.scriptContextTxInfo
    
{-# INLINEABLE validateAllSameOrigin #-}
-- Check whether all inputs come from a given PubKeyHash
validateAllSameOrigin :: Ledger.PubKeyHash -> Context.ScriptContext -> Bool
validateAllSameOrigin pkh =
  validateAllSameOriginFromList .
  (fmap (Address.addressCredential . Context.txOutAddress . Context.txInInfoResolved)) .
  Ledger.txInfoInputs .
  Ledger.scriptContextTxInfo
  where
    validateAllSameOriginFromList :: [ Credential.Credential ] -> Bool
    validateAllSameOriginFromList [] = True
    validateAllSameOriginFromList ((Credential.PubKeyCredential opkh) : l) = pkh == opkh && validateAllSameOriginFromList l
    validateAllSameOriginFromList _ = False

{-# INLINEABLE validateRefunding #-}
-- Assumes that all inputs come from the input PubKeyHash, and checks wether all the spent money
-- by this peer is refunded back to it as output
validateRefunding :: Ledger.PubKeyHash -> Context.ScriptContext -> Bool
validateRefunding pkh ctx = let txInfo = Ledger.scriptContextTxInfo ctx in
  (Ada.getLovelace $ Ada.fromValue $ (flip Ledger.valuePaidTo) pkh $ txInfo) ==
  (Ada.getLovelace $ Ada.fromValue $ Ledger.valueSpent $ txInfo)

{-# INLINEABLE validateCrowdFunding #-}
validateCrowdFunding :: CrowdFundingDatum -> CrowdFundingRedeemer -> Context.ScriptContext -> Bool
validateCrowdFunding (ProjectProposal pkh _ _ _) Cancel ctx =
  -- We can cancel the project if there is only one input in the script.
  traceIfFalse "Cancelling a project requires the project proposal as a single input." (validateSingleElt True ctx) &&
  -- And if the peer that cancels it is part of the signatures for the transaction
  traceIfFalse "Only the project owner can cancel it." (validateOwnerSignature pkh ctx)
validateCrowdFunding (ProjectProposal _ pName threshold range) Launch ctx =
  -- To launch the project, we need to check if the total amount of money is enough
  traceIfFalse "Current time not in the time range." (validateTimeRange range ctx) &&
  -- We also need to check whether the current time is part of the time range
  traceIfFalse "The project has not reach its funding threshold." (validateTotalSum threshold ctx) &&
  -- We also need to check if the inputs all point toward a single project proposal
  traceIfFalse "The inputs do not correspond to a single project proposal alongside fundings for that project." (validateSingleProposal pName ctx)
validateCrowdFunding (Funding _ pName) Launch ctx =
  -- We need to check if the inputs all point toward a single project proposal, which means the project proposal was also redeemed with Launch
  traceIfFalse "The inputs do not correspond to a single project proposal alongside fundings for that project." (validateSingleProposal pName ctx)
validateCrowdFunding (Funding pkh _) Cancel ctx =
  -- We check that the current peer signed the transaction
  traceIfFalse "A peer funding can only be cancelled by the peer in question." (validateOwnerSignature pkh ctx) &&
  -- We check that there is a single output in the script.
  traceIfFalse "Cancelling a peer funding requires the refunding as a single output." (validateSingleElt False ctx) &&
  -- We have to check that all inputs belong to the peer that cancels the transaction
  traceIfFalse "Cancelling a peer funding requires the funding as a single input." (validateAllSameOrigin pkh ctx) &&
  -- We check that this output points toward the funder, and gives back his total amount of money.
  traceIfFalse "The money has to be given back to the funder when the funding is cancelled." (validateRefunding pkh ctx)
data CrowdFunding

instance Scripts.ValidatorTypes CrowdFunding where
  type RedeemerType CrowdFunding = CrowdFundingRedeemer
  type DatumType CrowdFunding = CrowdFundingDatum

crowdFundingValidator :: Scripts.TypedValidator CrowdFunding
crowdFundingValidator =
  Scripts.mkTypedValidator @CrowdFunding
    $$(PlutusTx.compile [||validateCrowdFunding||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @CrowdFundingDatum @CrowdFundingRedeemer
