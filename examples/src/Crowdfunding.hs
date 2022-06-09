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
module Crowdfunding where

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
import PlutusCore.Evaluation.Machine.BuiltinCostModel (BuiltinCostModelBase(paramFstPair))

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

instance Eq PolicyParams where
  {-# INLINABLE (==) #-}
  PolicyParams pd t ft == PolicyParams pd' t' ft' = pd == pd' && t == t' && ft == ft'

-- | Datum type. Either project proposal with policy params
-- or funding from address to address
data Datum =
    Proposal PolicyParams
  | Funding L.PubKeyHash L.PubKeyHash
  deriving (Haskell.Show)

PlutusTx.makeLift ''Datum
PlutusTx.unstableMakeIsData ''Datum

instance Eq Datum where
  {-# INLINABLE (==) #-}
  Proposal pp == Proposal pp' = pp == pp'
  Funding to from == Funding to' from' = to == to' && from == from'
  _ == _ = False

{- INLINEABLE getOwner -}
getOwner :: Datum -> L.PubKeyHash
getOwner (Proposal pp) = fundingTarget pp
getOwner (Funding _ to) = to

{- INLINEABLE getFunder -}
getFunder :: Datum -> Maybe L.PubKeyHash
getFunder (Proposal _) = Nothing
getFunder (Funding from _) = Just from

{- INLINEABLE getFunder' -}
getFunder' :: Datum -> L.PubKeyHash
getFunder' (Proposal p) = fundingTarget p
getFunder' (Funding from _) = from

-- | Actions to be taken in the crowdfund. This will be the 'RedeemerType' 
data Action
  = -- | Launch project, pay funds to owner or (if after deadline) refund everyone
    Launch
  | -- | Refund all contributors
    IndividualRefund
  deriving (Haskell.Show)

PlutusTx.makeLift ''Action
PlutusTx.unstableMakeIsData ''Action

instance Eq Action where
  {-# INLINEABLE (==) #-}
  Launch == Launch = True
  IndividualRefund == IndividualRefund = True
  _ == _ = False

{- INLINEABLE crowdfundTimeRange -}
crowdfundTimeRange :: PolicyParams -> L.POSIXTimeRange
crowdfundTimeRange a = Interval.to (projectDeadline a)

-- | Test that the value paid to the given public key address is at
-- least the given value

{- INLINEABLE receivesFrom -}
receivesFrom :: L.TxInfo -> L.PubKeyHash -> L.Value -> Bool
receivesFrom txi who what = L.valuePaidTo txi who `Value.geq` what

-- | Check if the refunding of a particular address at least some amount is valid.
-- It is valid if
-- * the transaction is signed by the address being refunded
-- * all inputs of the transaction point to the original funder
-- * contributor is refunded at least the amount contributed

{- INLINEABLE validIndividualRefund -}
validIndividualRefund :: L.PubKeyHash -> L.ScriptContext -> Bool
validIndividualRefund addr ctx =
  let txi = L.scriptContextTxInfo ctx
      inputs = map L.txInInfoResolved $ L.txInfoInputs txi
      inputAddrs = mapMaybe L.txOutPubKey inputs
      receives = receivesFrom txi
  in traceIfFalse
     "Transaction not signed by contributor"
     (txi `L.txSignedBy` addr)
     && traceIfFalse
        "List of input addresses is not only the person being refunded"
        (inputAddrs == [addr])
        -- TODO: this is not needed since in/out values must be preserved in UTxO
        -- && traceIfFalse
        --    "Contributor is not refunded correct amount"
        --    (addr `receives` L.valueSpent txi)

-- | Check if total funds is greater than the threshold

{- INLINEABLE getTotalContributions -}
getTotalContributions :: L.TxInfo -> L.Value
getTotalContributions txi = sum $ map (L.txOutValue . L.txInInfoResolved) $ L.txInfoInputs txi
-- getTotalContributions ctx = sum $ map L.txOutValue $ L.getContinuingOutputs ctx

-- | Launch after the deadline is valid if
-- * everyone is refunded

{- INLINEABLE validAllRefund -}
validAllRefund :: PolicyParams -> L.ScriptContext -> Bool
validAllRefund cf ctx =
  let txi = L.scriptContextTxInfo ctx
  in traceIfFalse
     "Transaction not signed by contributor"
     (txi `L.txSignedBy` fundingTarget cf)

-- | Launch before the deadline is valid if
-- * it occurs before the deadline
-- * the total sum of all contributions is greater than the threshold
-- * the funding target is paid the funds

{- INLINEABLE validLaunch -}
validLaunch :: PolicyParams -> L.ScriptContext -> Bool
validLaunch cf ctx =
  let txi = L.scriptContextTxInfo ctx
      receives = receivesFrom txi
      total = getTotalContributions txi
  in traceIfFalse
     "Contributions after the deadline are not permitted"
     (crowdfundTimeRange cf `Interval.contains` L.txInfoValidRange txi)
     && traceIfFalse
        "Total contributions do not exceed threshold"
        (total `Value.geq` threshold cf)
        -- TODO: this is not needed since in/out values must be preserved in UTxO
        -- && traceIfFalse
        --    "Funding target not paid"
        --    (fundingTarget cf `receives` total)

-- | An individual contributing is valid if the contributor signs the transaction

{- INLINEABLE validFund -}
validFund :: L.PubKeyHash -> L.PubKeyHash -> L.ScriptContext -> Bool
validFund from to ctx =
  let txi = L.scriptContextTxInfo ctx
  in traceIfFalse
     "Funding transaction not signed by contributor"
     (txi `L.txSignedBy` to)

{- INLINEABLE validate -}
validate :: Datum -> Action -> L.ScriptContext -> Bool
validate (Funding from to) Launch ctx =
  validFund from to ctx
validate (Proposal cf) Launch ctx
  | validRange = validLaunch cf ctx
  | otherwise  = validAllRefund cf ctx
  where
    txi = L.scriptContextTxInfo ctx
    validRange = crowdfundTimeRange cf `Interval.contains` L.txInfoValidRange txi
validate (Funding from _) IndividualRefund ctx =
  validIndividualRefund from ctx
validate (Proposal _) IndividualRefund ctx = traceIfFalse "propir" False

data Crowdfunding

instance Scripts.ValidatorTypes Crowdfunding where
  type RedeemerType Crowdfunding = Action
  type DatumType Crowdfunding = Datum

crowdfundingValidator :: Scripts.TypedValidator Crowdfunding
crowdfundingValidator =
  Scripts.mkTypedValidator @Crowdfunding
    $$(PlutusTx.compile [||validate||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @Datum @Action
