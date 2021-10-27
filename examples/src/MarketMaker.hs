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
import qualified Ledger.Contexts as Validation
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
validateMarket :: MarketParams -> MarketDatum -> MarketRedeemer -> Validation.ScriptContext -> Bool

validateMarket (MarketParams nft asset constant) (MarketDatum nbAsset) Buy context =
  traceIfFalse
    "Datum (nb of tokens in stock) is not well updated"
    (datumNbAsset newDatum == nbAssetBought + nbAsset) &&
  traceIfFalse
    "Market equation is not respected"
    (nbAsset * adaPaid == constant * nbAssetBought)
  where
    info = Validation.scriptContextTxInfo context
    Just newDatum = findDatumFromOutputWithAsset nft info
    Just seller = findTheInputWithAsset asset info
    nbAssetBought =
      let (currencySymbol, tokenName) = Value.unAssetClass asset in
      Value.valueOf (Tx.txOutValue seller) currencySymbol tokenName
    valuePaid = Tx.txOutValue seller
    adaPaid = Ada.getLovelace . Ada.fromValue $ valuePaid

validateMarket (MarketParams nft asset constant) (MarketDatum nbAsset) Sell context =
  traceIfFalse
    "Datum (nb of tokens in stock) is not well updated"
    (datumNbAsset newDatum == nbAsset - nbAssetSold) &&
  traceIfFalse
    "Market equation is not respected"
    (nbAsset * adaReceived == constant * nbAssetSold)
  where
    info = Validation.scriptContextTxInfo context
    Just newDatum = findDatumFromOutputWithAsset nft info
    Just buyer = findTheOutputWithAsset asset info
    nbAssetSold =
      let (currencySymbol, tokenName) = Value.unAssetClass asset in
      Value.valueOf (Tx.txOutValue buyer) currencySymbol tokenName
    valueReceived = Tx.txOutValue buyer
    adaReceived = Ada.getLovelace . Ada.fromValue $ valueReceived

-- Helpers
{-# INLINEABLE hasAsset #-}
hasAsset :: Value.AssetClass -> Ledger.TxOut -> Bool
hasAsset asset txO = Value.assetClassValueOf (Ledger.txOutValue txO) asset > 0

{-# INLINEABLE isInExactlyOneInput #-}
isInExactlyOneInput :: Value.AssetClass -> Validation.TxInfo -> Bool
isInExactlyOneInput asset info =
  case filter (hasAsset asset) $ Validation.txInfoOutputs info of
    [_] -> True
    _ -> False

{-# INLINEABLE findTheInputWithAsset #-}
findTheInputWithAsset :: Value.AssetClass -> Validation.TxInfo -> Maybe Ledger.TxOut
findTheInputWithAsset asset info =
  case filter (hasAsset asset) $ Validation.txInInfoResolved <$> Validation.txInfoInputs info of
    [o] -> Just o
    _ -> Nothing

{-# INLINEABLE findTheOutputWithAsset #-}
findTheOutputWithAsset :: Value.AssetClass -> Validation.TxInfo -> Maybe Ledger.TxOut
findTheOutputWithAsset asset info =
  case filter (hasAsset asset) $ Validation.txInfoOutputs info of
    [o] -> Just o
    _ -> Nothing

{-# INLINEABLE findDatumFromOutput #-}
findDatumFromOutput :: (PlutusTx.FromData a) => Ledger.TxOut -> Validation.TxInfo -> Maybe a
findDatumFromOutput output info = do
  datHash <- Validation.txOutDatumHash output
  dat <- Validation.findDatum datHash info
  PlutusTx.fromBuiltinData $ Ledger.getDatum dat

{-# INLINEABLE findDatumFromOutputWithAsset #-}
findDatumFromOutputWithAsset :: (PlutusTx.FromData a) => Value.AssetClass -> Validation.TxInfo -> Maybe a
findDatumFromOutputWithAsset asset info = do
  output <- findTheOutputWithAsset asset info
  findDatumFromOutput output info

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
