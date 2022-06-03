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

-- * Data types

-- | All the data associated with crowdfunding that the validator needs to know
data StaticValParams = StaticValParams
  { -- | project must be funded by this time
    projectDeadline' :: L.POSIXTime,
    -- | amount that must be reached for project to be funded
    threshold' :: L.Value,
    -- | address to be paid to if threshold is reached by deadline
    fundingTarget' :: L.PubKeyHash
  }
  deriving (Haskell.Show)

-- some Plutus magic to compile the data type
PlutusTx.makeLift ''StaticValParams
PlutusTx.unstableMakeIsData ''StaticValParams

data ValParams = ValParams
  { staticValParams :: StaticValParams,
    -- | Asset class
    threadTokenAssetClass :: Value.AssetClass
  }
  deriving (Haskell.Show)


PlutusTx.makeLift ''ValParams
PlutusTx.unstableMakeIsData ''ValParams

projectDeadline :: ValParams -> L.POSIXTime
projectDeadline = projectDeadline' . staticValParams

threshold :: ValParams -> L.Value
threshold = threshold' . staticValParams

fundingTarget :: ValParams -> L.PubKeyHash
fundingTarget = fundingTarget' . staticValParams

data PolicyParams = PolicyParams
  { -- | TokenName of the thread token
    pThreadTokenName :: Value.TokenName
  }

PlutusTx.makeLift ''PolicyParams
PlutusTx.unstableMakeIsData ''PolicyParams

-- | Information about funder.
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

-- | The state of the crowdfund. This will be the 'DatumType'.
data CrowdfundingState
  = -- | state of a crowdfund that has not yet had any contributions
    NoFunds
  | -- | state of a crowdfund that has had at least one contribution
    Funding FunderInfo
  deriving (Haskell.Show)

PlutusTx.makeLift ''CrowdfundingState
PlutusTx.unstableMakeIsData ''CrowdfundingState

instance Eq CrowdfundingState where
  {-# INLINEABLE (==) #-}
  NoFunds == NoFunds = True
  Funding a == Funding x = a == x
  _ == _ = False

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

-- * The minting policy of the thread token

-- | This minting policy controls the thread token of a crowdfund. This
-- token belongs to the validator of the crowdfund, and must be minted (exactly once)
-- in the first transaction, for which this policy ensures that
-- * exactly one thread token is minted, by forcing an UTxO to be consumed
-- * after the transaction:
--     * the validator locks the thread token and the lot of the crowdfund
-- The final "fund" transaction of the crowdfund is the one that burns
-- the thread token. This transaction has its own validator
-- 'validFund', so that this minting policy only checks that at
-- exactly one token is burned.

{-# INLINEABLE mkPolicy #-}
mkPolicy :: PolicyParams -> L.Address -> L.ScriptContext -> Bool
mkPolicy (PolicyParams tName) validator ctx
  | amnt == Just 1 =
    case filter
    (\o -> L.txOutAddress o == validator)
    (L.txInfoOutputs txi) of
      [o] ->
        traceIfFalse
          "Validator does not recieve the thread token of freshly opened crowdfund"
          (L.txOutValue o `Value.geq` token)
          && traceIfFalse
            "Validator not in 'NoFunds'-state on freshly opened crowdfund"
            (outputCrowdfundingState txi o == Just NoFunds)
      _ -> trace "There must be exactly one output to the validator on a fresh crowdfund" False
  | amnt == Just (-1) =
    True -- no further checks here; 'validFund' checks everything
  | otherwise = trace "not minting or burning the right amount" False
  where
    txi = L.scriptContextTxInfo ctx
    L.Minting me = L.scriptContextPurpose ctx

    token :: L.Value
    token = Value.singleton me tName 1

    amnt :: Maybe Integer
    amnt = case Value.flattenValue (L.txInfoMint txi) of
      [(cs, tn, a)] | cs == L.ownCurrencySymbol ctx && tn == tName -> Just a
      _ -> Nothing

{-# INLINEABLE threadTokenName #-}
threadTokenName :: Value.TokenName
threadTokenName = Value.tokenName "CrowdfundingToken"

threadTokenPolicy :: PolicyParams -> Scripts.MintingPolicy
threadTokenPolicy pars =
  L.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode pars

getThreadTokenAssetClass :: Value.AssetClass 
getThreadTokenAssetClass =
  Value.assetClass
    (L.scriptCurrencySymbol $ threadTokenPolicy $ PolicyParams threadTokenName)
    threadTokenName


{- INLINEABLE crowdfundTimeRange -}
crowdfundTimeRange :: ValParams -> L.POSIXTimeRange
crowdfundTimeRange a = Interval.to (projectDeadline a)

{- INLINEABLE refundTimeRange -}
refundTimeRange :: ValParams -> L.POSIXTimeRange
refundTimeRange a = Interval.from (projectDeadline a)

-- | Extract a crowdfunding state from an output (if it has one)

{- INLINEABLE outputCrowdfundingState -}
outputCrowdfundingState :: L.TxInfo -> L.TxOut -> Maybe CrowdfundingState
outputCrowdfundingState txi o = do
  h <- L.txOutDatum o
  L.Datum d <- L.findDatum h txi
  PlutusTx.fromBuiltinData d

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
validRefund :: ValParams -> CrowdfundingState -> L.ScriptContext -> Bool
validRefund cf (Funding (FunderInfo amt addr)) ctx =
  let txi = L.scriptContextTxInfo ctx
      receives = receivesFrom txi
  in traceIfFalse
     "Contributor does not receive refund"
     (addr `receives` amt)
     && traceIfFalse
        "Refund before the deadline is not permitted"
        (refundTimeRange cf `Interval.contains` L.txInfoValidRange txi)
validRefund _ NoFunds _ =
  -- no refunds need to be validated with zero contributions
  True

-- | Check if total funds is greater than the threshold

{- INLINEABLE getTotalContributions -}
getTotalContributions :: L.ScriptContext -> L.Value
getTotalContributions ctx = sum $ map L.txOutValue $ L.getContinuingOutputs ctx

-- | An individual contributing is valid if
-- * the contribution came before the deadline
-- * the total sum of all contributions is greater than the threshold
-- * the contributor signs the transaction

{- INLINEABLE validFund -}
validFund :: ValParams -> CrowdfundingState -> L.ScriptContext -> Bool
validFund cf (Funding (FunderInfo _ addr)) ctx =
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
       && traceIfFalse
            "Not burning exactly one thread token"
            (L.txInfoMint txi == Value.assetClassValue (threadTokenAssetClass cf) (-1))
validFund _ NoFunds _ = trace "Threshold must be nonzero" False

{- INLINEABLE validate -}
validate :: ValParams -> CrowdfundingState -> Action -> L.ScriptContext -> Bool
validate cf datum Burn ctx =
  validFund cf datum ctx
validate cf datum IndividualRefund ctx =
  validRefund cf datum ctx

data Crowdfunding

instance Scripts.ValidatorTypes Crowdfunding where
  type RedeemerType Crowdfunding = Action
  type DatumType Crowdfunding = CrowdfundingState

crowdfundingValidator :: ValParams -> Scripts.TypedValidator Crowdfunding
crowdfundingValidator =
  Scripts.mkTypedValidatorParam @Crowdfunding
    $$(PlutusTx.compile [||validate||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @CrowdfundingState @Action
