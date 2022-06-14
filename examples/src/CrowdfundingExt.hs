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
data ValParams = ValParams
  { -- | project must be funded by this time
    projectDeadline :: L.POSIXTime,
    -- | amount that must be reached for project to be funded
    threshold :: L.Value,
    -- | minimum contribution that can be made
    minContribution :: L.Value,
    -- | address to be paid to if threshold is reached by deadline
    fundingTarget :: L.PubKeyHash,
    -- | TokenName of the reward tokens
    rewardTokenAssetClass :: Value.AssetClass
  }
  deriving (Haskell.Show)

PlutusTx.makeLift ''ValParams
PlutusTx.unstableMakeIsData ''ValParams

instance Eq ValParams where
  {-# INLINEABLE (==) #-}
  ValParams pd t mc ft ac == ValParams pd' t' mc' ft' ac' =
    pd == pd' && t == t' && mc == mc' && ft == ft' && ac == ac'

-- TODO: no master token, only reward tokens

-- | All data the minting policy of the thread token needs to
-- know. These are known after the opening transaction
data PolicyParams = PolicyParams
  { -- | TokenName of the thread token
    pRewardTokenName :: Value.TokenName
  }

PlutusTx.makeLift ''PolicyParams
PlutusTx.unstableMakeIsData ''PolicyParams

-- | Datum type. Either project proposal with policy params
-- or funding from address to address with value contributed
-- TODO: is value needed here? Only used to check min contribution
data Datum =
    Proposal ValParams
  | Funding L.PubKeyHash L.PubKeyHash L.Value
  deriving (Haskell.Show)

PlutusTx.makeLift ''Datum
PlutusTx.unstableMakeIsData ''Datum

instance Eq Datum where
  {-# INLINABLE (==) #-}
  Proposal vp == Proposal vp' = vp == vp'
  Funding to from val == Funding to' from' val' = to == to' && from == from' && val == val'
  _ == _ = False

{- INLINEABLE getOwner -}
getOwner :: Datum -> L.PubKeyHash
getOwner (Proposal vp) = fundingTarget vp
getOwner (Funding _ to _) = to

{- INLINEABLE getFunder -}
getFunder :: Datum -> Maybe L.PubKeyHash
getFunder (Funding from _ _) = Just from
getFunder _ = Nothing

{- INLINEABLE getValParams -}
getValParams :: Datum -> Maybe ValParams
getValParams (Proposal vp) = Just vp
getValParams _ = Nothing

-- | Retrieve funder from 'Funding' datum and owner from 'Proposal' datum

{- INLINEABLE getFunder' -}
getFunderOwner :: Datum -> L.PubKeyHash
getFunderOwner (Proposal vp) = fundingTarget vp
getFunderOwner (Funding from _ _) = from

-- | Actions to be taken in the crowdfund. This will be the 'RedeemerType' 
data Action
  = -- | Pay funds to owner and reward contributors or (if after deadline) refund everyone
    Launch
  | -- | Refund contributors
    IndividualRefund
  deriving (Haskell.Show)

PlutusTx.makeLift ''Action
PlutusTx.unstableMakeIsData ''Action

instance Eq Action where
  {-# INLINEABLE (==) #-}
  Launch == Launch = True
  IndividualRefund == IndividualRefund = True
  _ == _ = False

-- * The minting policy of the thread token

-- | This minting policy controls the reward tokens of a crowdfund. These
-- tokens are minted n times during the project's launch, where n is the number
-- of contributors. It's valid if
-- * exactly n tokens are minted
-- * all contributors receive a token

-- TODO: is Address a necessary parameter?

{-# INLINEABLE mkPolicy #-}
mkPolicy :: PolicyParams -> L.Address -> L.ScriptContext -> Bool
mkPolicy (PolicyParams tName) _ ctx
  | amnt == Just (length contributors) =
      let outs = L.txInfoOutputs txi
          outKeys = nub $ mapMaybe L.txOutPubKey outs
      in traceIfFalse
         "Not all contributors receive token"
         -- TODO: is this a sufficient check?
         -- Why is outKeys a (strict) superset of contributors?
         (all (`elem` outKeys) contributors)
  | otherwise = trace "not minting the right amount" False 
  where
    txi = L.scriptContextTxInfo ctx
    contributors = getUniqueContributors txi

    amnt :: Maybe Integer
    amnt = case Value.flattenValue (L.txInfoMint txi) of
      [(cs, tn, a)] | cs == L.ownCurrencySymbol ctx && tn == tName -> Just a
      _ -> Nothing

{-# INLINEABLE rewardTokenName #-}
rewardTokenName :: Value.TokenName
rewardTokenName = Value.tokenName "RewardToken"

rewardTokenPolicy :: PolicyParams -> Scripts.MintingPolicy
rewardTokenPolicy pars =
  L.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode pars

getRewardTokenAssetClass :: Value.AssetClass
getRewardTokenAssetClass =
  Value.assetClass
    (L.scriptCurrencySymbol $ rewardTokenPolicy $ PolicyParams rewardTokenName)
    rewardTokenName

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

-- | Get *unique* contributors from a transaction

{- INLINEABLE getUniqueContributors -}
getUniqueContributors :: L.TxInfo -> [L.PubKeyHash]
getUniqueContributors txi =
  let outputs = map L.txInInfoResolved $ L.txInfoInputs txi
      datums = mapMaybe (outputDatumState txi) outputs
      addrs = mapMaybe getFunder datums
  in nub addrs

{- INLINEABLE getTotalContributions -}
getTotalContributions :: L.TxInfo -> L.Value
getTotalContributions txi = sum $ map (L.txOutValue . L.txInInfoResolved) $ L.txInfoInputs txi

-- | Check if the refunding of a particular address at least some amount is valid.
-- It is valid if
-- * the transaction is signed by the address being refunded
-- * all inputs of the transaction point to the original funder
-- * contributor is refunded the amount contributed

{- INLINEABLE validIndividualRefund -}
validIndividualRefund :: L.PubKeyHash -> L.ScriptContext -> Bool
validIndividualRefund addr ctx =
  let txi = L.scriptContextTxInfo ctx
      inputs = map L.txInInfoResolved $ L.txInfoInputs txi
      inputAddrs = mapMaybe L.txOutPubKey inputs
      outputAddrs = mapMaybe L.txOutPubKey $ L.txInfoOutputs txi
      receives = receivesFrom txi
  in traceIfFalse
     "Transaction not signed by contributor"
     (txi `L.txSignedBy` addr)
     && traceIfFalse
        "List of input addresses is not only the person being refunded"
        (inputAddrs == [addr])
     && traceIfFalse
        "List of output addresses is not only the person being refunded"
        (nub outputAddrs == [addr])
     && traceIfFalse
        "Contributor is not refunded correct amount"
        (addr `receives` (L.valueSpent txi - L.txInfoFee txi))

-- | Launch after the deadline is valid if
-- * the owner signs the transaction

{- INLINEABLE validAllRefund -}
validAllRefund :: ValParams -> L.ScriptContext -> Bool
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
validLaunch :: ValParams -> L.ScriptContext -> Bool
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
     -- TODO: need to subtract token value
     -- && traceIfFalse
     --    "Funding target not paid"
     --    (fundingTarget cf `receives` (total - L.txInfoFee txi))

-- | An individual contributing is valid if the contributor signs the transaction

{- INLINEABLE validFund -}
validFund :: L.PubKeyHash -> L.PubKeyHash -> L.Value -> L.ScriptContext -> Bool
validFund from to val ctx =
  let txi = L.scriptContextTxInfo ctx
      outputs = map L.txInInfoResolved $ L.txInfoInputs txi
      datums = mapMaybe (outputDatumState txi) outputs
      allParams = mapMaybe getValParams datums
      [currParams] = filter (\vp -> fundingTarget vp == to) allParams
  in traceIfFalse
     "Funding transaction not signed by contributor"
     (txi `L.txSignedBy` to)
     && traceIfFalse
        "Contribution is not at least the minimum value"
        (val `Value.geq` minContribution currParams)

{- INLINEABLE validate -}
validate :: Datum -> Action -> L.ScriptContext -> Bool
validate (Funding from to val) Launch ctx =
  validFund from to val ctx
validate (Proposal cf) Launch ctx
  | validRange = validLaunch cf ctx
  | otherwise  = validAllRefund cf ctx
  where
    txi = L.scriptContextTxInfo ctx
    validRange = crowdfundTimeRange cf `Interval.contains` L.txInfoValidRange txi
validate (Funding from _ _) IndividualRefund ctx =
  validIndividualRefund from ctx
validate (Proposal _) IndividualRefund ctx =
  -- TODO: this should be when the owner tries to cancel project before deadline
  traceIfFalse "proposal datum with individual refund action" False

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
