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
module CrowdfundingExt where

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
    fundingTarget :: L.PubKeyHash,
    -- | reward for contributors
    reward :: L.Value,
    -- | TokenName of the reward tokens
    rewardTokenName :: Value.TokenName
  }
  deriving (Haskell.Show)

PlutusTx.makeLift ''PolicyParams
PlutusTx.unstableMakeIsData ''PolicyParams

-- | Datum type. Either project proposal with policy params
-- or funding from address to address
data Datum =
    Proposal ValParams
  | Funding L.PubKeyHash L.PubKeyHash
  deriving (Haskell.Show)

PlutusTx.makeLift ''Datum
PlutusTx.unstableMakeIsData ''Datum

instance Eq Datum where
  {-# INLINABLE (==) #-}
  Proposal vp == Proposal vp' = vp == vp'
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
  = -- | Burn master token, pay funds to owner and reward contributors,
    -- or (if after deadline) refund everyone
    Burn
  | -- | Refund contributors
    IndividualRefund
    -- TODO: probably add another action for get reward
  deriving (Haskell.Show)

PlutusTx.makeLift ''Action
PlutusTx.unstableMakeIsData ''Action

instance Eq Action where
  {-# INLINEABLE (==) #-}
  Burn == Burn = True
  IndividualRefund == IndividualRefund = True
  _ == _ = False

-- * The minting policy of the thread token

-- | This minting policy controls the reward tokens of a crowdfund. These
-- tokens are minted n times during the project's launch, where n is the number
-- of contributors. It's valid if
-- * exactly n tokens are minted

-- TODO: also need to make sure one contributor doesn't get multiple reward tokens

{-# INLINEABLE mkPolicy #-}
mkPolicy :: PolicyParams -> L.ScriptContext -> Bool
mkPolicy (PolicyParams _ _ _ _ tName) ctx
  | amnt == Just numContributors = True
  | otherwise = trace "not minting the right amount" False 
  where
    txi = L.scriptContextTxInfo ctx
    L.Minting me = L.scriptContextPurpose ctx
    numContributors = getTotalContributors txi

    token :: L.Value
    token = Value.singleton me tName numContributors

    amnt :: Maybe Integer
    amnt = case Value.flattenValue (L.txInfoMint txi) of
      [(cs, otn, a)] | cs == L.ownCurrencySymbol ctx && tn == tName -> Just a
      _ -> Nothing

{-# INLINEABLE threadTokenName #-}
rewardTokenName :: Value.TokenName
rewardTokenName = Value.tokenName "RewardToken"

threadTokenPolicy :: PolicyParams -> Scripts.MintingPolicy
threadTokenPolicy pars =
  L.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode pars

getThreadTokenAssetClass :: Value.AssetClass 
getThreadTokenAssetClass =
  Value.assetClass
    (L.scriptCurrencySymbol $ threadTokenPolicy $ PolicyParams rewardTokenName)
    threadTokenName


{- INLINEABLE crowdfundTimeRange -}
crowdfundTimeRange :: ValParams -> L.POSIXTimeRange
crowdfundTimeRange a = Interval.to (projectDeadline a)

-- | Extract a datum state from an output (if it has one)

{- INLINEABLE outputCrowdfundingState -}
outputDatumState :: L.TxInfo -> L.TxOut -> Maybe Datum
outputDatumState txi o = do
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

{- INLINEABLE getTotalContributors -}
-- TODO: make sure to remove duplicates
getTotalContributors :: L.TxInfo -> Integer
getTotalContributors txi = length $ L.txInfoInputs txi

{- INLINEABLE getTotalContributions -}
getTotalContributions :: L.TxInfo -> L.Value
getTotalContributions txi = sum $ map (L.txOutValue . L.txInInfoResolved) $ L.txInfoInputs txi

-- | An individual contributing is valid if
-- * the contribution came before the deadline
-- * the total sum of all contributions is greater than the threshold
-- * the contributor signs the transaction
-- * the contributor receives a fund token

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

data CrowdfundingExt

instance Scripts.ValidatorTypes CrowdfundingExt where
  type RedeemerType CrowdfundingExt = Action
  type DatumType CrowdfundingExt = CrowdfundingState

crowdfundingValidator :: ValParams -> Scripts.TypedValidator CrowdfundingExt
crowdfundingValidator =
  Scripts.mkTypedValidatorParam @CrowdfundingExt
    $$(PlutusTx.compile [||validate||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @CrowdfundingState @Action
