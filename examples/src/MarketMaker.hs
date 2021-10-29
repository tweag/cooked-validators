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

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Ledger
import qualified Ledger.Ada as Ada
import qualified Ledger.Contexts as Contexts
import qualified Ledger.Tx as Tx
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
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

fee :: Integer
fee = 10

-- Validator

validateMarket :: MarketParams -> MarketDatum -> MarketRedeemer -> Contexts.ScriptContext -> Bool

validateMarket (MarketParams nft asset constant) (MarketDatum nbAsset) Buy context =
  let info = Contexts.scriptContextTxInfo context
   in case findNewDatum nft info of
        Nothing -> traceError "No script output with the NFT and datum."
        Just (MarketDatum newAssetNb) ->
          traceIfFalse
            "Datum (nb of tokens in stock) is not well updated."
            (newAssetNb == nbAsset + nbAssetBought)
            && traceIfFalse
              "Market equation is not respected."
              -- FIXME:
              -- (adaSpentByScript == 0)
              -- this holds for the last transaction in the example run
              (nbAsset * adaSpentByScript == constant * nbAssetBought)
          where
            sellerInputs = inputsSuchThat (hasNotAsset nft) info
            sellerOutputs = outputsSuchThat (hasNotAsset nft) info
            adaSpentByScript = adaSpent sellerInputs sellerOutputs
            nbAssetBought = negate $ assetSpent asset sellerInputs sellerOutputs

validateMarket (MarketParams nft asset constant) (MarketDatum nbAsset) Sell context =
  let info = Contexts.scriptContextTxInfo context
   in case findNewDatum nft info of
        Nothing -> traceError "No script output with the NFT and datum."
        Just (MarketDatum newAssetNb) ->
          traceIfFalse
            "Datum (nb of tokens in stock) is not well updated."
            (newAssetNb == nbAsset - nbAssetSold)
            && traceIfFalse
              "Market equation is not respected."
              (nbAsset * adaSpentByBuyer == constant * nbAssetSold)
          where
            buyerInputs = inputsSuchThat (hasNotAsset nft) info
            buyerOutputs = outputsSuchThat (hasNotAsset nft) info
            adaSpentByBuyer = negate $ adaSpent buyerInputs buyerOutputs
            nbAssetSold = assetSpent asset buyerInputs buyerOutputs

-- Helpers

{-# INLINEABLE outputsSuchThat #-}
outputsSuchThat :: (Ledger.TxOut -> Bool) -> Contexts.TxInfo -> [Ledger.TxOut]
outputsSuchThat condition = filter condition . Contexts.txInfoOutputs

{-# INLINEABLE outputSuchThat #-}
outputSuchThat :: (Ledger.TxOut -> Bool) -> Contexts.TxInfo -> Maybe Ledger.TxOut
outputSuchThat condition = uniqueElement . outputsSuchThat condition

{-# INLINEABLE inputsSuchThat #-}
inputsSuchThat :: (Ledger.TxOut -> Bool) -> Contexts.TxInfo -> [Ledger.TxOut]
inputsSuchThat condition =
  filter condition
    . map Contexts.txInInfoResolved
    . Contexts.txInfoInputs

{-# INLINEABLE hasAsset #-}
hasAsset :: Value.AssetClass -> Ledger.TxOut -> Bool
hasAsset asset txO = Value.assetClassValueOf (Ledger.txOutValue txO) asset > 0

{-# INLINEABLE hasAddress #-}
hasAddress :: Ledger.Address -> Ledger.TxOut -> Bool
hasAddress address = (== address) . Ledger.txOutAddress

{-# INLINEABLE findNewDatum #-}
findNewDatum :: (PlutusTx.FromData a) => Value.AssetClass -> Contexts.TxInfo -> Maybe a
findNewDatum nftClass info = do
  scriptOutput <- outputSuchThat (hasAsset nftClass) info
  findDatumFromOutput scriptOutput info

{-# INLINEABLE assetSpent #-}
assetSpent :: Value.AssetClass -> [Ledger.TxOut] -> [Ledger.TxOut] -> Integer
assetSpent asset inputs outputs = nbAssetInTxs outputs - nbAssetInTxs inputs
  where
    (currencySymbol, tokenName) = Value.unAssetClass asset
    nbAssetInTxs :: [Ledger.TxOut] -> Integer
    nbAssetInTxs =
      sum
        . map (\tx -> Value.valueOf (Tx.txOutValue tx) currencySymbol tokenName)

{-# INLINEABLE adaSpent #-}
adaSpent :: [Ledger.TxOut] -> [Ledger.TxOut] -> Integer
adaSpent inputs outputs = adaInTxs outputs - (adaInTxs inputs - fee)
  where
    adaInTxs = sum . map (Ada.getLovelace . Ada.fromValue . Tx.txOutValue)

{-# INLINEABLE findDatumFromOutput #-}
findDatumFromOutput :: (PlutusTx.FromData a) => Ledger.TxOut -> Contexts.TxInfo -> Maybe a
findDatumFromOutput output info = do
  datHash <- Contexts.txOutDatumHash output
  dat <- Contexts.findDatum datHash info
  PlutusTx.fromBuiltinData $ Ledger.getDatum dat

{-# INLINEABLE hasNotAsset #-}
hasNotAsset :: Value.AssetClass -> Ledger.TxOut -> Bool
hasNotAsset asset txO = Value.assetClassValueOf (Ledger.txOutValue txO) asset == 0

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
