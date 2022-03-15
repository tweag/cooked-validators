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
import Ledger.Contexts
import qualified PlutusTx
import qualified Ledger.Typed.Scripts as Scripts

-- Some basic types
import Data.String
import Data.Int
import Data.Bool
import Data.Time
import Data.Aeson
import GHC.Generics
import Prelude

-- The datum for the Crowd Funding smart contract
-- A peer either proposes a project with an ID and a goal,
-- or a peer can contribute to a project with a certain ID
data CrowdFundingDatum =
  ProjectProposal String Integer Day |
  Funding String
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type CrowdFundingRedeemer = ()

{-# INLINEABLE validateCrowdFunding #-}
validateCrowdFunding :: CrowdFundingDatum -> CrowdFundingRedeemer -> ScriptContext -> Bool
validateCrowdFunding datum redeemer context = True

-- data CrowdFunding

-- PlutusTx.makeLift ''CrowdFundingDatum
-- PlutusTx.unstableMakeIsData ''CrowdFundingDatum

-- instance Scripts.ValidatorTypes CrowdFunding where
--   type RedeemerType CrowdFunding = CrowdFundingRedeemer
--   type DatumType CrowdFunding = CrowdFundingDatum

-- crowdFundingValidator :: Scripts.TypedValidator CrowdFunding
-- crowdFundingValidator =
--   Scripts.mkTypedValidator @CrowdFunding
--     $$(PlutusTx.compile [||validateCrowdFunding||])
--     $$(PlutusTx.compile [||wrap||])
--   where
--     wrap = Scripts.wrapValidator @CrowdFundingDatum @CrowdFundingRedeemer







