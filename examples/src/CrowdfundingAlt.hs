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

-- These language extensions are just what Split.hs uses

-- | Arrange a crowdfund with a deadline and threshold
module CrowdfundingAlt where

import qualified Ledger as L
import qualified Ledger.Ada as Ada
import qualified Ledger.Interval as Interval
import Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude as Haskell
import Test.QuickCheck (functionBoundedEnum)
import Plutus.Contracts.SimpleEscrow (EscrowParams(deadline))
import qualified PlutusTx.Numeric as L

-- * Data types

-- | All the data associated with crowdfunding that the validator needs to know
data PolicyParams = PolicyParams
  { -- | project must be funded by this time
    projectDeadline :: L.POSIXTime,
    -- | amount that must be reached for project to be funded
    threshold :: L.Value,
    -- | address to be paid to if threshold is reached by deadline
    fundingTarget :: L.PubKeyHash
  }
  deriving (Haskell.Show)

-- some Plutus magic to compile the data type
PlutusTx.makeLift ''PolicyParams
PlutusTx.unstableMakeIsData ''PolicyParams

-- | Information about the funder. This will be the 'DatumType'.
data FunderInfo = FunderInfo
  { -- | the last funder's contribution
    fund :: L.Value,
    -- | the last funder's address
    funder :: L.PubKeyHash
  }
  deriving (Haskell.Show)

PlutusTx.makeLift ''FunderInfo
PlutusTx.unstableMakeIsData ''FunderInfo

instance Eq FunderInfo where
  {-# INLINEABLE (==) #-}
  FunderInfo a b == FunderInfo x y = a == x && b == y

-- | Actions to be taken in the crowdfund. This will be the 'RedeemerType' 
data Action
  = -- | Burn master token, pay funds to owner
    Burn
  | -- | Refund all contributors
    IndividualRefund
  deriving (Haskell.Show)

PlutusTx.makeLift ''Action
PlutusTx.unstableMakeIsData ''Action

instance Eq Action where
  {-# INLINEABLE (==) #-}
  Burn == Burn = True
  IndividualRefund == IndividualRefund = True
  _ == _ = False


{- INLINEABLE crowdfundTimeRange -}
crowdfundTimeRange :: PolicyParams -> L.POSIXTimeRange
crowdfundTimeRange a = Interval.to (projectDeadline a)

{- INLINEABLE refundTimeRange -}
refundTimeRange :: PolicyParams -> L.POSIXTimeRange
refundTimeRange a = Interval.from (projectDeadline a)

-- | Test that the value paid to the given public key address is at
-- least the given value

{- INLINEABLE receivesFrom -}
receivesFrom :: L.TxInfo -> L.PubKeyHash -> L.Value -> Bool
receivesFrom txi who what = L.valuePaidTo txi who `Value.geq` what

-- | Check if the refunding of a particular address at least some amount is valid.
-- It is valid if
-- * the refunding occurs after the deadline
-- * the address is refunded at least the amount ehy contributed

{- INLINEABLE validRefund -}
validRefund :: PolicyParams -> FunderInfo -> L.ScriptContext -> Bool
validRefund cf (FunderInfo amt addr) ctx =
  let txi = L.scriptContextTxInfo ctx
      receives = receivesFrom txi
  in traceIfFalse
     "Contributor does not receive refund"
     (addr `receives` amt)
     && traceIfFalse
        "Refund before the deadline is not permitted"
        (refundTimeRange cf `Interval.contains` L.txInfoValidRange txi)

-- | Check if total funds is greater than the threshold

{- INLINEABLE getTotalContributions -}
getTotalContributions :: L.ScriptContext -> L.Value
getTotalContributions ctx = sum $ map L.txOutValue $ L.getContinuingOutputs ctx

-- | An individual contributing is valid if
-- * the contribution came before the deadline
-- * the total sum of all contributions is greater than the threshold
-- * the contributor signs the transaction

{- INLINEABLE validFund -}
validFund :: PolicyParams -> FunderInfo -> L.ScriptContext -> Bool
validFund cf (FunderInfo _ addr) ctx =
  let txi = L.scriptContextTxInfo ctx
      receives = receivesFrom txi
      total = getTotalContributions ctx
  in traceIfFalse
       "Contributions after the deadline are not permitted"
       (crowdfundTimeRange cf `Interval.contains` L.txInfoValidRange txi)
       && traceIfFalse
            "Funding transaction not signed by contributor"
            (txi `L.txSignedBy` addr)
       && traceIfFalse
            "Total contributions do not exceed threshold"
            (total `Value.geq` threshold cf)
       && traceIfFalse
            "Funding target not paid"
            (fundingTarget cf `receives` total)

{- INLINEABLE validate -}
validate :: PolicyParams -> FunderInfo -> Action -> L.ScriptContext -> Bool
validate cf datum Burn ctx =
  validFund cf datum ctx
validate cf datum IndividualRefund ctx =
  validRefund cf datum ctx

data CrowdfundingAlt

instance Scripts.ValidatorTypes CrowdfundingAlt where
  type RedeemerType CrowdfundingAlt = Action
  type DatumType CrowdfundingAlt = FunderInfo

crowdfundingValidator :: PolicyParams -> Scripts.TypedValidator CrowdfundingAlt
crowdfundingValidator =
  Scripts.mkTypedValidatorParam @CrowdfundingAlt
    $$(PlutusTx.compile [||validate||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @FunderInfo @Action
