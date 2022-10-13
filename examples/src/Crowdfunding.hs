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
import qualified Ledger.Interval as Interval
import Ledger.Scripts as Pl
import Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import qualified Plutus.Script.Utils.V1.Scripts as Pl (scriptCurrencySymbol)
import qualified Plutus.V1.Ledger.Api as Api
import qualified PlutusTx
import qualified PlutusTx.Numeric as Pl
import PlutusTx.Prelude
import qualified Prelude as Haskell

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
    -- | currency symbol of the thread token
    threadCS :: L.CurrencySymbol
  }
  deriving (Haskell.Show)

PlutusTx.makeLift ''ValParams
PlutusTx.unstableMakeIsData ''ValParams

instance Eq ValParams where
  {-# INLINEABLE (==) #-}
  ValParams pd t mc ft cs == ValParams pd' t' mc' ft' cs' =
    pd == pd' && t == t' && mc == mc' && ft == ft' && cs == cs'

-- | All data associated with an individual fund
data FundingParams = FundingParams
  { -- | public key that is contributing
    from :: L.PubKeyHash,
    -- | public key representing project funder is contributing to
    to :: L.PubKeyHash,
    -- | value funder is contributing
    val :: L.Value,
    -- | utxo representing the crowdfund
    txOut :: L.TxOutRef
  }
  deriving (Haskell.Show)

PlutusTx.makeLift ''FundingParams
PlutusTx.unstableMakeIsData ''FundingParams

instance Eq FundingParams where
  {-# INLINEABLE (==) #-}
  FundingParams from to val txOut == FundingParams from' to' val' txOut' =
    from == from' && to == to' && val == val' && txOut == txOut'

-- | Datum type. Either a pre-proposal, a project proposal with policy params,
-- or funding with funding params. The pre-proposal is used in a two-step transaction
-- process in order to avoid allowing an attacker to redirect the thread token.
-- For more information, see the Auction contract.
data CfDatum
  = PreProposal ValParams
  | Proposal ValParams
  | Funding FundingParams
  deriving (Haskell.Show)

PlutusTx.makeLift ''CfDatum
PlutusTx.unstableMakeIsData ''CfDatum

instance Eq CfDatum where
  {-# INLINEABLE (==) #-}
  Proposal vp == Proposal vp' = vp == vp'
  Funding fp == Funding fp' = fp == fp'
  _ == _ = False

{-# INLINEABLE getOwner #-}
getOwner :: CfDatum -> L.PubKeyHash
getOwner (PreProposal vp) = fundingTarget vp
getOwner (Proposal vp) = fundingTarget vp
getOwner (Funding fp) = to fp

{-# INLINEABLE getFunder #-}
getFunder :: CfDatum -> Maybe L.PubKeyHash
getFunder (Funding fp) = Just (from fp)
getFunder _ = Nothing

{-# INLINEABLE getUtxoDatum #-}
getUtxoDatum :: CfDatum -> Maybe L.TxOutRef
getUtxoDatum (Funding fp) = Just (txOut fp)
getUtxoDatum _ = Nothing

{-# INLINEABLE getValParams #-}
getValParams :: CfDatum -> Maybe ValParams
getValParams (Proposal vp) = Just vp
getValParams _ = Nothing

-- | Retrieve funder from 'Funding' datum and owner from 'Proposal' datum
{-# INLINEABLE getFunderOwner #-}
getFunderOwner :: CfDatum -> L.PubKeyHash
getFunderOwner (PreProposal vp) = fundingTarget vp
getFunderOwner (Proposal vp) = fundingTarget vp
getFunderOwner (Funding fp) = from fp

{-# INLINEABLE getValue #-}
getValue :: CfDatum -> Maybe L.Value
getValue (Funding fp) = Just (val fp)
getValue _ = Nothing

-- | Actions to be taken in the crowdfund. This will be the 'RedeemerType'
data Action
  = -- | Pay funds to owner and reward contributors or (if after deadline) refund everyone
    Launch L.TxOutRef
  | -- | Minting thread token
    Minting
  | -- | Refund contributors
    IndividualRefund
  deriving (Haskell.Show)

PlutusTx.makeLift ''Action
PlutusTx.unstableMakeIsData ''Action

instance Eq Action where
  {-# INLINEABLE (==) #-}
  Launch txOut == Launch txOut' = txOut == txOut'
  IndividualRefund == IndividualRefund = True
  _ == _ = False

-- * The minting policy of the thread token

-- | This minting policy controls the thread token of the
-- crowdfund. This NFT will belong to the 'crowdfundingValidator'; its
-- presence proves the authenticity of the crowdfund. Here, we check
-- that exactly one thread token is minted, enforcing that the
-- appropriate offer utxo, whose hash as computed by
-- 'tokenNameFromTxOutRef' must be the token name of the minted token,
-- is consumed.
{-# INLINEABLE mkThreadTokenPolicy #-}
mkThreadTokenPolicy :: L.TxOutRef -> L.ScriptContext -> Bool
mkThreadTokenPolicy txOut ctx
  | amnt == 1 =
    traceIfFalse
      "Proposal UTxO not consumed"
      (any (\i -> L.txInInfoOutRef i == txOut) $ L.txInfoInputs txi)
  | amnt == -1 = True
  | otherwise = trace "Not minting or burning the right amount" False
  where
    txi = L.scriptContextTxInfo ctx
    L.Minting me = L.scriptContextPurpose ctx
    tName = tokenNameFromTxOutRef txOut
    amnt = getMintedAmount txi me tName

-- | Parameterized minting policy of thread token
threadTokenPolicy :: Scripts.MintingPolicy
threadTokenPolicy =
  Api.mkMintingPolicyScript
    $$(PlutusTx.compile [||Scripts.mkUntypedMintingPolicy mkThreadTokenPolicy||])

getThreadTokenAssetClass :: L.TxOutRef -> Value.AssetClass
getThreadTokenAssetClass txOut =
  Value.assetClass
    (Pl.scriptCurrencySymbol threadTokenPolicy)
    (tokenNameFromTxOutRef txOut)

threadToken :: L.TxOutRef -> L.Value
threadToken txOut = Value.assetClassValue (getThreadTokenAssetClass txOut) 1

-- | Compute the thread token of a crowdfund from the currency symbol and proposal UTxO.
{-# INLINEABLE threadTokenOnChain #-}
threadTokenOnChain :: L.CurrencySymbol -> L.TxOutRef -> L.Value
threadTokenOnChain threadCS txOut =
  Value.assetClassValue (Value.AssetClass (threadCS, tokenNameFromTxOutRef txOut)) 1

-- | Compute the thread token of the auction with the givven proposal UTxO
-- threadToken :: L.TxOutRef -> L.Value
-- threadToken txOut = Value.assetClassValue (threadTokenAssetClas

-- * The minting policy of the reward token

-- | This minting policy controls the reward tokens of a crowdfund. There are n tokens
-- minted during the project's launch, where n is the number of contributors. It's valid if
-- * exactly n tokens are minted
-- * the transaction has at least one input
-- * all contributors receive one token + amount contributed - amount in funding datum
--   + note: this result must be at least 2 ada. if not, transaction will fail earlier
-- * this should throw an error in traces where funds were locked in the script
{-# INLINEABLE mkRewardTokenPolicy #-}
mkRewardTokenPolicy :: L.TxOutRef -> () -> L.ScriptContext -> Bool
mkRewardTokenPolicy txOut _ ctx
  | amnt == length contributors =
    traceIfFalse
      "Transaction does not have at least one input"
      (length (L.txInfoInputs txi) > 0)
      && traceIfFalse
        "Not all contributors receive token + leftover value"
        (all validContribution contributors)
  | otherwise = trace "Not minting the right amount" False
  where
    txi = L.scriptContextTxInfo ctx
    contributors = getUniqueContributors txi
    L.Minting me = L.scriptContextPurpose ctx
    tName = tokenNameFromTxOutRef txOut
    token = Value.singleton me tName 1
    amnt = getMintedAmount txi me tName

    validContribution :: L.PubKeyHash -> Bool
    validContribution addr =
      let receives = receivesFrom txi
          inputs = L.txInfoInputs txi
          inputsAddr =
            filter
              (\i -> L.toPubKeyHash (L.txOutAddress $ L.txInInfoResolved i) == Just addr)
              inputs
          inputsTotal = sum $ map (L.txOutValue . L.txInInfoResolved) inputsAddr
          datums = getAllDatums txi
          funderDatums = filter (\x -> getFunder x == Just addr) datums
          datumTotal = getTotalValue funderDatums
       in addr `receives` (token <> inputsTotal <> Pl.negate datumTotal)

-- | Parameterized minting policy of reward token
rewardTokenPolicy :: L.TxOutRef -> Scripts.MintingPolicy
rewardTokenPolicy txOut =
  Api.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.mkUntypedMintingPolicy . mkRewardTokenPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode txOut

getRewardTokenAssetClass :: L.TxOutRef -> Value.AssetClass
getRewardTokenAssetClass txOut =
  Value.assetClass
    (Pl.scriptCurrencySymbol $ rewardTokenPolicy txOut)
    (rewardTokenName txOut)

rewardToken :: L.TxOutRef -> Integer -> L.Value
rewardToken txOut = Value.assetClassValue (getRewardTokenAssetClass txOut)

-- | Get the amount of minted tokens of a specific TokenName
{-# INLINEABLE getMintedAmount #-}
getMintedAmount :: L.TxInfo -> L.CurrencySymbol -> L.TokenName -> Integer
getMintedAmount txi me tName =
  foldr
    ( \(cs, tn, a) n ->
        if cs == me && tn == tName then n + a else n
    )
    0
    $ Value.flattenValue (L.txInfoMint txi)

-- | Copied from the Auction contract
{-# INLINEABLE rewardTokenName #-}
rewardTokenName :: L.TxOutRef -> Value.TokenName
rewardTokenName (L.TxOutRef (L.TxId tid) i) =
  Value.TokenName $ appendByteString tid $ appendByteString "-" $ encodeInteger i
  where
    -- we know that the numbers (indices of transaction outputs) we're working
    -- with here are non-negative.
    encodeInteger :: Integer -> BuiltinByteString
    encodeInteger n
      | n `quotient` 10 == 0 = encodeDigit n
      | otherwise = encodeInteger (n `quotient` 10) <> encodeDigit (n `remainder` 10)
      where
        encodeDigit :: Integer -> BuiltinByteString
        -- 48 is the ASCII code for '0'
        encodeDigit d = consByteString (d + 48) emptyByteString

-- | Compute the token name of the thread token of an auction from its proposal UTxO.
-- Copied from the Auction contract.
{-# INLINEABLE tokenNameFromTxOutRef #-}
tokenNameFromTxOutRef :: L.TxOutRef -> L.TokenName
tokenNameFromTxOutRef (L.TxOutRef (L.TxId tid) i) =
  Value.TokenName $ appendByteString tid $ appendByteString "-" $ encodeInteger i
  where
    -- we know that the numbers (indices of transaction outputs) we're working
    -- with here are non-negative.
    encodeInteger :: Integer -> BuiltinByteString
    encodeInteger n
      | n `quotient` 10 == 0 = encodeDigit n
      | otherwise = encodeInteger (n `quotient` 10) <> encodeDigit (n `remainder` 10)
      where
        encodeDigit :: Integer -> BuiltinByteString
        -- 48 is the ASCII code for '0'
        encodeDigit d = consByteString (d + 48) emptyByteString

{-# INLINEABLE crowdfundTimeRange #-}
crowdfundTimeRange :: ValParams -> L.POSIXTimeRange
crowdfundTimeRange a = Interval.to (projectDeadline a)

-- | Extract a datum state from an output (if it has one)
{-# INLINEABLE outputDatumState #-}
outputDatumState :: L.TxInfo -> L.TxOut -> Maybe CfDatum
outputDatumState txi o = do
  h <- L.txOutDatum o
  Pl.Datum d <- L.findDatum h txi
  PlutusTx.fromBuiltinData d

-- | Test that the value paid to the given public key address is at
-- least the given value
{-# INLINEABLE receivesFrom #-}
receivesFrom :: L.TxInfo -> L.PubKeyHash -> L.Value -> Bool
receivesFrom txi who what = L.valuePaidTo txi who `Value.geq` what

-- | Get list of all datums consumed by a transaction
{-# INLINEABLE getAllDatums #-}
getAllDatums :: L.TxInfo -> [CfDatum]
getAllDatums txi =
  mapMaybe (outputDatumState txi) (map L.txInInfoResolved $ L.txInfoInputs txi)

-- | Get *unique* contributors from a transaction
{-# INLINEABLE getUniqueContributors #-}
getUniqueContributors :: L.TxInfo -> [L.PubKeyHash]
getUniqueContributors txi = nub $ mapMaybe getFunder $ getAllDatums txi

-- | Get total value contributed from a list of datums
{-# INLINEABLE getTotalValue #-}
getTotalValue :: [CfDatum] -> L.Value
getTotalValue = sum . mapMaybe getValue

-- | Check if the refunding of a particular address at least some amount is valid.
-- It is valid if
-- * the transaction is signed by the address being refunded
-- * all inputs of the transaction point to the original funder
-- * the output points to the original funder
-- * contributor is refunded the amount contributed
{-# INLINEABLE validIndividualRefund #-}
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
-- NOTE: this is a vulnerability. The owner can create a transaction that pays
-- themself all the contributed funds without rewarding the contributors with
-- reward tokens. See trace "ownerRefundsVulnerability" that exhibits this
-- vulnerabilty. With this as the validator instead of the fixed version below,
-- that trace will succeed when it is expected to fail.

{-
{-# INLINEABLE validAllRefund #-}
validAllRefund :: ValParams -> L.ScriptContext -> Bool
validAllRefund cf ctx =
  let txi = L.scriptContextTxInfo ctx
   in traceIfFalse
        "Transaction not signed by owner"
        (txi `L.txSignedBy` fundingTarget cf)
-}

-- | Fixed version of above. Launch after the deadline is valid if
-- * the owner signs the transaction
-- * all contributors are refunded what they contributed
{-# INLINEABLE validAllRefund #-}
validAllRefund :: ValParams -> L.TxOutRef -> L.ScriptContext -> Bool
validAllRefund cf txOut ctx =
  traceIfFalse
    "Transaction not signed by owner"
    (txi `L.txSignedBy` fundingTarget cf)
    && traceIfFalse
      "Contributor is not refunded correct amount"
      (all validAllRefundAddress contributors)
    && traceIfFalse
      "Not burning thread token"
      (L.txInfoMint txi == Pl.negate token)
  where
    txi = L.scriptContextTxInfo ctx
    contributors = getUniqueContributors txi
    token = threadTokenOnChain (threadCS cf) txOut

    validAllRefundAddress :: L.PubKeyHash -> Bool
    validAllRefundAddress addr =
      let receives = receivesFrom txi
          funderDatums = filter (\x -> getFunder x == Just addr) (getAllDatums txi)
          datumTotal = getTotalValue funderDatums
       in addr `receives` datumTotal

-- | Launch before the deadline is valid if
-- * it occurs before the deadline
-- * the total sum of all contributions is greater than the threshold
-- * the funding target is paid the funds
{-# INLINEABLE validLaunch #-}
validLaunch :: ValParams -> L.TxOutRef -> L.ScriptContext -> Bool
validLaunch cf txOut ctx =
  let txi = L.scriptContextTxInfo ctx
      receives = receivesFrom txi
      datums = getAllDatums txi
      total = getTotalValue datums
      funderDatums =
        filter (\d -> isJust (getFunder d) && getOwner d == fundingTarget cf) datums
      vals = mapMaybe getValue funderDatums
      token = threadTokenOnChain (threadCS cf) txOut
   in traceIfFalse
        "Transaction not signed by owner"
        (txi `L.txSignedBy` fundingTarget cf)
        && traceIfFalse
          "Contributions after the deadline are not permitted"
          (crowdfundTimeRange cf `Interval.contains` L.txInfoValidRange txi)
        && traceIfFalse
          "Total contributions do not exceed threshold"
          (total `Value.geq` threshold cf)
        && traceIfFalse
          "Funding target not paid total contributions"
          (fundingTarget cf `receives` (total - L.txInfoFee txi))
        && traceIfFalse
          "Contribution is not at least the minimum value"
          (all (`Value.geq` minContribution cf) vals)
        && traceIfFalse
          "Not burning thread token"
          (fst (Value.split $ L.txInfoMint txi) == token)

-- | An individual contributing during launch is valid if
-- * the owner signs the transaction
-- * the contribution is at least the minimum value
{-# INLINEABLE validFund #-}
validFund :: L.PubKeyHash -> L.PubKeyHash -> L.Value -> L.ScriptContext -> Bool
validFund _ to _ ctx =
  let txi = L.scriptContextTxInfo ctx
   in traceIfFalse
        "Transaction not signed by owner"
        (txi `L.txSignedBy` to)

-- | Minting thread token is valid if it consumes the correct proposal utxo
{-# INLINEABLE validMinting #-}
validMinting :: ValParams -> L.ScriptContext -> Bool
validMinting vp@(ValParams _ _ _ ft threadCS) ctx =
  let txi = L.scriptContextTxInfo ctx
      Just (L.TxInInfo txOut _) = L.findOwnInput ctx
   in traceIfFalse
        "Transaction not signed by owner"
        (txi `L.txSignedBy` ft)
        && traceIfFalse
          "There must be a 'Proposal' output identical to the input containing the thread token"
          ( any
              ( \o ->
                  L.txOutValue o `Value.geq` threadTokenOnChain threadCS txOut
                    && case outputDatumState txi o of
                      Just (Proposal vp') -> vp == vp'
                      _ -> False
              )
              (L.getContinuingOutputs ctx)
          )

{-# INLINEABLE validate #-}
validate :: CfDatum -> Action -> L.ScriptContext -> Bool
validate (Funding fp) (Launch _) ctx =
  validFund (from fp) (to fp) (val fp) ctx
validate (Proposal cf) (Launch txOut) ctx
  | validRange = validLaunch cf txOut ctx
  | otherwise = validAllRefund cf txOut ctx
  where
    txi = L.scriptContextTxInfo ctx
    validRange = crowdfundTimeRange cf `Interval.contains` L.txInfoValidRange txi
validate (Funding fp) IndividualRefund ctx =
  validIndividualRefund (from fp) ctx
validate (PreProposal cf) Minting ctx = validMinting cf ctx
validate (PreProposal _) _ _ = trace "Only 'Minting' allowed in 'PreProposal' state" False
validate _ Minting _ = trace "'Minting' only allowed in 'PreProposal' state" False
validate (Proposal _) IndividualRefund _ =
  -- disallowed, though this could be allowing the owner to cancel the project
  -- before the deadline, which is currently impossible
  traceIfFalse "Proposal datum with individual refund action not allowed" False

data Crowdfunding

instance Scripts.ValidatorTypes Crowdfunding where
  type RedeemerType Crowdfunding = Action
  type DatumType Crowdfunding = CfDatum

crowdfundingValidator :: Scripts.TypedValidator Crowdfunding
crowdfundingValidator =
  Scripts.mkTypedValidator @Crowdfunding
    $$(PlutusTx.compile [||validate||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator @CfDatum @Action
