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

-- Retrieving a CrowdFundingDatum of a given input in a transaction
findDatum :: Ledger.TxInfo -> Ledger.TxInInfo -> Maybe CrowdFundingDatum
findDatum txInfo txInInfo = do
  let txOut = Ledger.txInInfoResolved txInInfo
  hash <- Ledger.txOutDatumHash txOut
  Ledger.Datum d <- Contexts.findDatum hash txInfo
  PlutusTx.fromBuiltinData d

{- The redeemer for the CrowdFunding smart contract. The project can either be :
* launched, with the following parameters :
  - an AssetClass : the nature of the tokens to give to contributors
  - a power of 10 : the number of parts in which the project shall be split among contributors
* cancelled -}
data CrowdFundingRedeemer =
  Launch Ledger.AssetClass Integer |
  Cancel
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)  

-- The same magic for the redeemer
PlutusTx.unstableMakeIsData ''CrowdFundingRedeemer
PlutusTx.makeLift ''CrowdFundingRedeemer

{-# INLINEABLE getCurrentValue #-}
-- Retrieves the value for the current input
getCurrentValue :: Contexts.ScriptContext -> Maybe Ledger.Value
getCurrentValue = (fmap (Ledger.txOutValue . Contexts.txInInfoResolved)) . Contexts.findOwnInput

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

{-# INLINEABLE isProjectProposal #-}
-- Checks if the given Datum corresponds to a project proposal
isProjectProposal :: Maybe CrowdFundingDatum -> Bool
isProjectProposal (Just (ProjectProposal _ _ _ _)) = True
isProjectProposal _ = False

{-# INLINEABLE validateSingleProposal #-}
-- Checks that the context only contains a single instance of a project proposal
validateSingleProposal :: Contexts.ScriptContext -> Bool
validateSingleProposal _ = True -- Haskell.undefined
  -- (== 1) .
  -- length .
  -- (filter isProjectProposal) .
  -- (fmap (PlTx.fromBuiltinData . snd)) .
  -- txInfoData .
  -- scriptContextTxInfo
  
{-# INLINEABLE validateCrowdFunding #-}
validateCrowdFunding :: CrowdFundingDatum -> CrowdFundingRedeemer -> Contexts.ScriptContext -> Bool
-- We can always cancel the project regarding the project proposal, or not
-- Make sure there is no other input belonging to the current script when we are in this case
validateCrowdFunding (ProjectProposal _ _ _ _) Cancel _   = True
-- To launch the project, we need to check if the total amount of money is enough
-- We also need to check whether the current time is part of the time range
-- Finally, we need to check if there is a single instance of a project proposal
validateCrowdFunding (ProjectProposal _ _ threshold range) (Launch _ _) ctx =
  traceIfFalse "Current time not in the time range."              (validateTimeRange range ctx) &&
  traceIfFalse "The project has not reach its funding threshold." (validateTotalSum threshold ctx) &&
  traceIfFalse "There are multiple project proposals."            (validateSingleProposal ctx)
validateCrowdFunding (Funding hash id) (Launch _ _) ctx = True -- Haskell.undefined
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







