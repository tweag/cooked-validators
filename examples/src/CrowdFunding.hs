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
import qualified Ledger.Contexts as Contexts
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import qualified Ledger.Ada as Ada
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
  Ledger.Datum d <- Contexts.findDatum hash txInfo
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

{-# INLINEABLE findProjectOwnerKey #-}
-- This retrieves the first PubKeyHash from a project proposal, often assuming there is only one
findProjectOwnerKey :: Ledger.TxInfo -> Maybe Ledger.PubKeyHash
findProjectOwnerKey = findFirstProjectProposal . findAllDatums
  where
    findFirstProjectProposal [] = Nothing
    findFirstProjectProposal ((Funding _ _) : l) = findFirstProjectProposal l
    findFirstProjectProposal ((ProjectProposal pkh _ _ _) : _) = Just pkh
  
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
getCurrentValue :: Contexts.ScriptContext -> Maybe Ledger.Value
getCurrentValue =
  (fmap (Ledger.txOutValue . Contexts.txInInfoResolved)) .
  Contexts.findOwnInput

{-# INLINEABLE validateTimeRange #-}
-- Retrieves the current transition time range and compares it to the project time range
validateTimeRange :: Ledger.POSIXTimeRange -> Contexts.ScriptContext -> Bool
validateTimeRange range =
  (== range) .
  (\/ range) .
  Contexts.txInfoValidRange .
  Contexts.scriptContextTxInfo

{-# INLINEABLE validateTotalSum #-}
-- The sum of all input lovelaces should be greater than the threshold
validateTotalSum :: Integer -> Contexts.ScriptContext -> Bool
validateTotalSum threshold =
  (<= threshold) .
  Ada.getLovelace .
  Ada.fromValue .
  mconcat .
  fmap (Contexts.txOutValue . Contexts.txInInfoResolved) .
  Contexts.txInfoInputs .
  Contexts.scriptContextTxInfo

{-# INLINEABLE validateSingleProposal #-}
-- Checks that the context only contains a single instance of a project proposal
-- and that all fundings are directed towards that proposal. This also checks
-- that there are no additionnal irrelevant inputs
validateSingleProposal :: Contexts.ScriptContext -> Bool
validateSingleProposal ctx =
  let txInfo = Ledger.scriptContextTxInfo ctx in
    let txInInfos = Ledger.txInfoInputs txInfo in 
      let datums = findAllDatums txInfo in
        length datums == length txInInfos &&
        validateDatumsContent Nothing Nothing datums
  where
    validateDatumsContent :: Maybe BuiltinByteString -> Maybe BuiltinByteString -> [ CrowdFundingDatum ] -> Bool
    validateDatumsContent (Just ppName) (Just fpName) [] = ppName == fpName
    validateDatumsContent (Just ppName) Nothing [] = True
    validateDatumsContent Nothing _ [] = False
    validateDatumsContent (Just ppName) mfpName ((ProjectProposal _ nppName _ _) : l) = False
    validateDatumsContent Nothing mfpName ((ProjectProposal _ nppName _ _) : l) =
      validateDatumsContent (Just nppName) mfpName l
    validateDatumsContent mppName (Just fpName) ((Funding _ nfpName) : l) =
      if (nfpName == fpName) then validateDatumsContent mppName (Just fpName) l else False
    validateDatumsContent mppName Nothing ((Funding _ nfpName) : l) =
      validateDatumsContent mppName (Just nfpName) l
  

-- Checks if there is a single input in the context
{-# INLINEABLE validateSingleInput #-}
validateSingleInput :: Contexts.ScriptContext -> Bool
validateSingleInput =
  (== 1) .
  length .
  Ledger.txInfoInputs .
  Ledger.scriptContextTxInfo

-- Checks if the peer that launched the script is also the owner of the project
{-# INLINEABLE validateOwnerIdentity #-}
validateOwnerIdentity :: Contexts.ScriptContext -> Bool
validateOwnerIdentity ctx = let txInfo = Ledger.scriptContextTxInfo ctx in
  case findProjectOwnerKey txInfo of
    Nothing -> False
    Just pkh -> pkh `elem` (Ledger.txInfoSignatories txInfo)
  
{-# INLINEABLE validateCrowdFunding #-}
validateCrowdFunding :: CrowdFundingDatum -> CrowdFundingRedeemer -> Contexts.ScriptContext -> Bool
validateCrowdFunding (ProjectProposal _ _ _ _) Cancel ctx =
  -- We can cancel the project if the current input is the only one in the script
  traceIfFalse "Cancelling a project requires the project proposal as a single input." (validateSingleInput ctx) &&
  -- And if the peer that cancels it is part of the signatures for the transaction
  traceIfFalse "Only the project owner can cancel it." (validateOwnerIdentity ctx)
validateCrowdFunding (ProjectProposal _ _ threshold range) Launch ctx =
  -- To launch the project, we need to check if the total amount of money is enough
  traceIfFalse "Current time not in the time range." (validateTimeRange range ctx) &&
  -- We also need to check whether the current time is part of the time range
  traceIfFalse "The project has not reach its funding threshold." (validateTotalSum threshold ctx) &&
  -- Finally, we need to check if the inputs all point toward a single project proposal
  traceIfFalse "There are multiple project proposals." (validateSingleProposal ctx)
validateCrowdFunding (Funding hash id) Launch ctx = True -- Haskell.undefined
validateCrowdFunding (Funding hash id) Cancel ctx = True -- Haskell.undefined

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
