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

validateMarket (MarketParams nft asset constant) (MarketDatum nbAsset) Buy context =
  traceIfFalse
    "Datum (nb of tokens in stock) is not well updated"
    (datumNbAsset newDatum == nbAssetBought + nbAsset) &&
  traceIfFalse
    "Market equation is not respected"
    (nbAsset * adaPaid == constant * nbAssetBought)
  where
    info = Contexts.scriptContextTxInfo context
    Just newDatum = findDatumFromOutputWithAsset nft info
    Just seller = findTheInputWithAsset asset info
    nbAssetBought =
      let (currencySymbol, tokenName) = Value.unAssetClass asset in
      Value.valueOf (Tx.txOutValue seller) currencySymbol tokenName
    valuePaid = Tx.txOutValue seller
    adaPaid = Ada.getLovelace . Ada.fromValue $ valuePaid

validateMarket (MarketParams nft asset constant) (MarketDatum nbAsset) Sell context =
  let info = Contexts.scriptContextTxInfo context in
  case findNewDatum nft info of
    Nothing -> traceError "No script output with the NFT and datum."
    Just newDatum ->
      case findBuyerInputOutput nft asset info of
        Nothing -> traceError "No buyer input or output."
        Just (buyerInput, buyerOutput) ->
          traceIfFalse
            "Datum (nb of tokens in stock) is not well updated"
            (datumNbAsset newDatum == nbAsset - nbAssetSold) &&
          traceIfFalse
            "Market equation is not respected"
            (nbAsset * adaReceived == constant * nbAssetSold)
          where
            adaReceived = Ada.getLovelace . Ada.fromValue . Tx.txOutValue $ buyerInput
            (currencySymbol, tokenName) = Value.unAssetClass asset
            nbAssetSold = Value.valueOf (Tx.txOutValue buyerOutput) currencySymbol tokenName

-- Helpers
{-# INLINEABLE isInExactlyOneInput #-}
isInExactlyOneInput :: Value.AssetClass -> Contexts.TxInfo -> Bool
isInExactlyOneInput asset info =
  case filter (hasAsset asset) $ Contexts.txInfoOutputs info of
    [_] -> True
    _ -> False

{-# INLINEABLE findTheInputWithAsset #-}
findTheInputWithAsset :: Value.AssetClass -> Contexts.TxInfo -> Maybe Ledger.TxOut
findTheInputWithAsset asset info =
  case filter (hasAsset asset) $ Contexts.txInInfoResolved <$> Contexts.txInfoInputs info of
    [o] -> Just o
    _ -> Nothing

{-# INLINEABLE findTheOutputWithAsset #-}
findTheOutputWithAsset :: Value.AssetClass -> Contexts.TxInfo -> Maybe Ledger.TxOut
findTheOutputWithAsset asset info =
  case filter (hasAsset asset) $ Contexts.txInfoOutputs info of
    [o] -> Just o
    _ -> Nothing

{-# INLINEABLE findDatumFromOutputWithAsset #-}
findDatumFromOutputWithAsset :: (PlutusTx.FromData a) => Value.AssetClass -> Contexts.TxInfo -> Maybe a
findDatumFromOutputWithAsset asset info = do
  output <- findTheOutputWithAsset asset info
  findDatumFromOutput output info

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

{-# INLINEABLE findBuyerInputOutput #-}
findBuyerInputOutput :: Value.AssetClass -> Value.AssetClass -> Contexts.TxInfo -> Maybe (Ledger.TxOut, Ledger.TxOut)
findBuyerInputOutput nftClass assetClass info = do
  buyerOutput <- outputSuchThat (\tx -> hasAsset nftClass tx && hasNotAsset assetClass tx) info
  buyerInput <- inputSuchThat (hasAddress (Ledger.txOutAddress buyerOutput)) info
  return (buyerInput, buyerOutput)

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
