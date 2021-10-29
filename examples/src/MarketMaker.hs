{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MarketMaker where

-- Here are the necessary imports. It is VERY IMPORTANT to refrain from using
-- anything that is not a simple accessor from Ledger; this will not
-- compile to PlutusCore.
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Ledger
import qualified Ledger.Ada as Ada
import qualified Ledger.Contexts as Contexts
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import qualified Ledger.Tx as Tx
-- The PlutusTx and its prelude provide the functions we can use for on-chain computations.

import qualified PlutusTx
import PlutusTx.Prelude hiding (Applicative (..))
import Schema (ToSchema)
import qualified Prelude as Haskell

-- In this example, the equation for the market maker is:
-- `nbTokens * tokenPrice = constant`
data MarketParams = MarketParams
  { paramsMarketNFT :: Value.AssetClass,
    paramsAsset :: Value.AssetClass,
    paramsConstant :: Integer
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''MarketParams

newtype MarketDatum = MarketDatum
  {datumNbAsset :: Integer}
  deriving stock (Haskell.Show)

data MarketRedeemer = Buy | Sell

-- Validator
validateMarket :: MarketParams -> MarketDatum -> MarketRedeemer -> Contexts.ScriptContext -> Bool

-- Note: only works with indivudual tx outputs with the seller/buyer

validateMarket (MarketParams nft asset constant) (MarketDatum nbAsset) Buy context =
  let info = Contexts.scriptContextTxInfo context in
  case findNewDatum nft info of
    Nothing -> traceError "No script output with the NFT and datum."
    Just (MarketDatum newAssetNb) ->
      case findSellerInput nft asset info of
        Nothing -> traceError "No seller input."
        Just sellerInput ->
          case findSellerOutput nft asset info of
            Nothing -> traceError "No seller output."
            Just sellerOutput ->
              traceIfFalse
                "Datum (nb of tokens in stock) is not well updated."
                (newAssetNb == nbAsset + nbAssetBought) &&
              traceIfFalse
                "Market equation is not respected."
                (nbAsset * adaPaidToBuyer >= constant * nbAssetBought)
              where
                adaPaidToBuyer = adaSpent sellerInput sellerOutput
                (currencySymbol, tokenName) = Value.unAssetClass asset
                nbAssetBought =
                  Value.valueOf (Tx.txOutValue sellerInput) currencySymbol tokenName
                  - Value.valueOf (Tx.txOutValue sellerOutput) currencySymbol tokenName

validateMarket (MarketParams nft asset constant) (MarketDatum nbAsset) Sell context =
  let info = Contexts.scriptContextTxInfo context in
  case findNewDatum nft info of
    Nothing -> traceError "No script output with the NFT and datum."
    Just (MarketDatum newAssetNb) ->
      case findBuyerOutput nft asset info of
        Nothing -> traceError "No buyer output."
        Just buyerOutput ->
          case findBuyerInput nft asset info of
            Nothing -> traceError "No buyer input."
            Just buyerInput ->
              traceIfFalse
                "Datum (nb of tokens in stock) is not well updated."
                (newAssetNb == nbAsset - nbAssetSold) &&
              traceIfFalse
                "Market equation is not respected."
                (nbAsset * adaSpentByBuyer >= constant * nbAssetSold)
              where
                adaSpentByBuyer = adaReceived buyerInput buyerOutput
                (currencySymbol, tokenName) = Value.unAssetClass asset
                nbAssetSold = assetSpent asset buyerInput buyerOutput

-- Helpers

{-# INLINEABLE findDatumFromOutput #-}
findDatumFromOutput :: (PlutusTx.FromData a) => Ledger.TxOut -> Contexts.TxInfo -> Maybe a
findDatumFromOutput output info = do
  datHash <- Contexts.txOutDatumHash output
  dat <- Contexts.findDatum datHash info
  PlutusTx.fromBuiltinData $ Ledger.getDatum dat

{-# INLINEABLE hasAsset #-}
hasAsset :: Value.AssetClass -> Ledger.TxOut -> Bool
hasAsset asset txO = Value.assetClassValueOf (Ledger.txOutValue txO) asset > 0

{-# INLINEABLE hasAddress #-}
hasAddress :: Ledger.Address -> Ledger.TxOut -> Bool
hasAddress address = (== address) . Ledger.txOutAddress

{-# INLINEABLE hasNotAsset #-}
hasNotAsset :: Value.AssetClass -> Ledger.TxOut -> Bool
hasNotAsset asset txO = Value.assetClassValueOf (Ledger.txOutValue txO) asset == 0

{-# INLINEABLE inputSuchThat #-}
inputSuchThat :: (Ledger.TxOut -> Bool) -> Contexts.TxInfo -> Maybe Ledger.TxOut
inputSuchThat condition =
  uniqueElement .
  filter condition .
  map Contexts.txInInfoResolved .
  Contexts.txInfoInputs

{-# INLINEABLE outputSuchThat #-}
outputSuchThat :: (Ledger.TxOut -> Bool) -> Contexts.TxInfo -> Maybe Ledger.TxOut
outputSuchThat condition =
  uniqueElement .
  filter condition .
  Contexts.txInfoOutputs

{-# INLINEABLE findScriptOutput #-}
findScriptOutput :: Value.AssetClass -> Contexts.TxInfo -> Maybe Ledger.TxOut
findScriptOutput nftClass =
  outputSuchThat (hasAsset nftClass)

{-# INLINEABLE findNewDatum #-}
findNewDatum :: (PlutusTx.FromData a) => Value.AssetClass -> Contexts.TxInfo -> Maybe a
findNewDatum nftClass info = do
  scriptOutput <- findScriptOutput nftClass info
  findDatumFromOutput scriptOutput info

{-# INLINEABLE findBuyerOutput #-}
findBuyerOutput :: Value.AssetClass -> Value.AssetClass -> Contexts.TxInfo -> Maybe Ledger.TxOut
findBuyerOutput nftClass assetClass =
  outputSuchThat (\tx -> hasAsset assetClass tx && hasNotAsset nftClass tx)

{-# INLINEABLE findBuyerInput #-}
findBuyerInput :: Value.AssetClass -> Value.AssetClass -> Contexts.TxInfo -> Maybe Ledger.TxOut
findBuyerInput nftClass assetClass info = do
  buyerOutput <-findBuyerOutput nftClass assetClass info
  inputSuchThat (hasAddress (Ledger.txOutAddress buyerOutput)) info

{-# INLINEABLE findSellerInput #-}
findSellerInput :: Value.AssetClass -> Value.AssetClass -> Contexts.TxInfo -> Maybe Ledger.TxOut
findSellerInput nftClass assetClass =
  inputSuchThat (\tx -> hasAsset assetClass tx && hasNotAsset nftClass tx)

{-# INLINEABLE findSellerOutput #-}
findSellerOutput :: Value.AssetClass -> Value.AssetClass -> Contexts.TxInfo -> Maybe Ledger.TxOut
findSellerOutput nftClass assetClass info = do
  sellerInput <-findSellerInput nftClass assetClass info
  outputSuchThat (hasAddress (Ledger.txOutAddress sellerInput)) info

{-# INLINEABLE adaReceived #-}
adaReceived :: Ledger.TxOut -> Ledger.TxOut -> Integer
adaReceived input output = adaIn input - adaIn output
  where
    adaIn = Ada.getLovelace . Ada.fromValue . Tx.txOutValue

{-# INLINEABLE adaSpent #-}
adaSpent :: Ledger.TxOut -> Ledger.TxOut -> Integer
adaSpent input output = adaIn output - adaIn input
  where
    adaIn = Ada.getLovelace . Ada.fromValue . Tx.txOutValue

{-# INLINEABLE assetReceived #-}
assetReceived :: Value.AssetClass -> Ledger.TxOut -> Ledger.TxOut -> Integer
assetReceived asset input output = nbAssetIn input - nbAssetIn output
  where
    (currencySymbol, tokenName) = Value.unAssetClass asset
    nbAssetIn tx = Value.valueOf (Tx.txOutValue tx) currencySymbol tokenName

{-# INLINEABLE assetSpent #-}
assetSpent :: Value.AssetClass -> Ledger.TxOut -> Ledger.TxOut -> Integer
assetSpent asset input output = nbAssetIn output - nbAssetIn input
  where
    (currencySymbol, tokenName) = Value.unAssetClass asset
    nbAssetIn tx = Value.valueOf (Tx.txOutValue tx) currencySymbol tokenName

-- Plutus boilerplate

data Market

PlutusTx.makeLift ''MarketDatum
PlutusTx.unstableMakeIsData ''MarketDatum

PlutusTx.makeLift ''MarketRedeemer
PlutusTx.unstableMakeIsData ''MarketRedeemer

instance Scripts.ValidatorTypes Market where
  type RedeemerType Market = MarketRedeemer
  type DatumType Market = MarketDatum

marketValidator :: MarketParams -> Scripts.TypedValidator Market
marketValidator =
  Scripts.mkTypedValidatorParam @Market
    $$(PlutusTx.compile [||validateMarket||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @MarketDatum @MarketRedeemer
