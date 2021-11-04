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

-- | A market maker script that trades some asset in exchange of Ada according
-- to an exchange rate specified by and equation involving the current pool of
-- asset detained by the market maker.
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

-- | The market maker is identified by a unique NFT.
-- It trades some assets of a certain asset class.
-- It is characterised by a constant in this example where the equation for the
-- market maker is: `nbTokens * tokenPrice = constant`
data MarketParams = MarketParams
  { paramsMarketNFT :: Value.AssetClass
  , paramsAsset :: Value.AssetClass
  , paramsConstant :: Integer
  }
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''MarketParams

-- | The datum contains the current quantity of assets detained by the market
-- maker.
newtype MarketDatum = MarketDatum
  {datumNbAsset :: Integer}
  deriving stock (Haskell.Show)

data MarketRedeemer = Buy | Sell

-- | Constant fee in Ada applied to every transaction.
fee :: Integer
fee = 10

-- | Market maker validator
validateMarket :: MarketParams -> MarketDatum -> MarketRedeemer -> Contexts.ScriptContext -> Bool
-- Validator for a Buy transaction: the market buys assets from a third party
validateMarket (MarketParams nft asset constant) (MarketDatum nbAsset) Buy context =
  let info = Contexts.scriptContextTxInfo context
   in -- Check that there is a unique output for the market maker which is
      -- identified by its NFT.
      case findNewDatum nft info of
        Nothing -> traceError "No script output with the NFT and datum."
        Just (MarketDatum newAssetNb) ->
          -- Check that the datum evolves to account for the new quantity of
          -- assets gained through the buying by the market maker.
          traceIfFalse
            "Datum (nb of tokens in store) is not well updated."
            (newAssetNb == nbAsset + nbAssetBought)
            -- Check that the Ada spent by the market maker to buy the assets is
            -- in line with the current price per asset specified by the market
            -- maker equation.
            && traceIfFalse
              "Market equation is not respected."
              (nbAsset * adaSpentByScript == constant * nbAssetBought)
          where
            -- To check how much assets and ada have been traded, we take
            -- the inputs and outputs not from the market maker, that is the
            -- inputs and outputs which do not have the market maker's NFT.
            -- The accumulated value inside the outputs and inputs is compared
            -- to get how much ada has been spent and how much asset obtained
            -- in exchange.
            sellerInputs = inputsSuchThat (hasNotAsset nft) info
            sellerOutputs = outputsSuchThat (hasNotAsset nft) info
            adaSpentByScript = adaSpent sellerInputs sellerOutputs
            nbAssetBought = negate $ assetSpent asset sellerInputs sellerOutputs

-- Validator for a Sell transaction: the market buys assets from a third party
validateMarket (MarketParams nft asset constant) (MarketDatum nbAsset) Sell context =
  let info = Contexts.scriptContextTxInfo context
   in -- Check that there is a unique output for the market maker which is
      -- identified by its NFT.
      case findNewDatum nft info of
        Nothing -> traceError "No script output with the NFT and datum."
        Just (MarketDatum newAssetNb) ->
          -- Check that the datum evolves to account for the new quantity of
          -- assets gained through the buying by the market maker.
          traceIfFalse
            "Datum (nb of tokens in store) is not well updated."
            (newAssetNb == nbAsset - nbAssetSold)
            -- Check that the Ada spent by the buyer to buy the assets is in line
            -- with the current price per asset specified by the market maker
            -- equation.
            && traceIfFalse
              "Market equation is not respected."
              (nbAsset * adaSpentByBuyer == constant * nbAssetSold)
          where
            -- To check how much assets and ada have been traded, we take
            -- the inputs and outputs not from the market maker, that is the
            -- inputs and outputs which do not have the market maker's NFT.
            -- The accumulated value inside the outputs and inputs is compared
            -- to get how much ada has been spent and how much asset obtained
            -- in exchange.
            buyerInputs = inputsSuchThat (hasNotAsset nft) info
            buyerOutputs = outputsSuchThat (hasNotAsset nft) info
            adaSpentByBuyer = negate $ adaSpent buyerInputs buyerOutputs
            nbAssetSold = assetSpent asset buyerInputs buyerOutputs

-- Helpers

{-# INLINEABLE outputsSuchThat #-}

-- | The outputs of a transaction that satisfy a condition.
outputsSuchThat :: (Ledger.TxOut -> Bool) -> Contexts.TxInfo -> [Ledger.TxOut]
outputsSuchThat condition = filter condition . Contexts.txInfoOutputs

{-# INLINEABLE outputSuchThat #-}

-- | The single output of a transaction that satisfy a condition or Nothing.
outputSuchThat :: (Ledger.TxOut -> Bool) -> Contexts.TxInfo -> Maybe Ledger.TxOut
outputSuchThat condition = uniqueElement . outputsSuchThat condition

{-# INLINEABLE inputsSuchThat #-}

-- | The inputs of a transaction that satisfy a condition.
inputsSuchThat :: (Ledger.TxOut -> Bool) -> Contexts.TxInfo -> [Ledger.TxOut]
inputsSuchThat condition =
  filter condition
    . map Contexts.txInInfoResolved
    . Contexts.txInfoInputs

{-# INLINEABLE hasAsset #-}

-- | Whether a transaction has asset of a given asset class.
hasAsset :: Value.AssetClass -> Ledger.TxOut -> Bool
hasAsset asset txO = Value.assetClassValueOf (Ledger.txOutValue txO) asset > 0

{-# INLINEABLE hasNotAsset #-}

-- | Whether a transaction has no asset of a given asset class.
hasNotAsset :: Value.AssetClass -> Ledger.TxOut -> Bool
hasNotAsset asset txO = Value.assetClassValueOf (Ledger.txOutValue txO) asset == 0

{-# INLINEABLE findDatumFromOutput #-}

-- | The datum contained in a script output.
findDatumFromOutput :: (PlutusTx.FromData a) => Ledger.TxOut -> Contexts.TxInfo -> Maybe a
findDatumFromOutput output info = do
  datHash <- Contexts.txOutDatumHash output
  dat <- Contexts.findDatum datHash info
  PlutusTx.fromBuiltinData $ Ledger.getDatum dat

{-# INLINEABLE findNewDatum #-}

-- | The datum from the only script output carrying a given asset (typically an NFT).
findNewDatum :: (PlutusTx.FromData a) => Value.AssetClass -> Contexts.TxInfo -> Maybe a
findNewDatum nftClass info = do
  scriptOutput <- outputSuchThat (hasAsset nftClass) info
  findDatumFromOutput scriptOutput info

{-# INLINEABLE assetSpent #-}

-- | Combined quantity of asset of a given asset class that have been spent
-- between a set of inputs and outputs.
assetSpent :: Value.AssetClass -> [Ledger.TxOut] -> [Ledger.TxOut] -> Integer
assetSpent asset inputs outputs = nbAssetInTxs outputs - nbAssetInTxs inputs
  where
    (currencySymbol, tokenName) = Value.unAssetClass asset
    nbAssetInTxs :: [Ledger.TxOut] -> Integer
    nbAssetInTxs =
      sum
        . map (\tx -> Value.valueOf (Tx.txOutValue tx) currencySymbol tokenName)

{-# INLINEABLE adaSpent #-}

-- | Ada spent between a set of inputs and outputs, taking into account a fee.
adaSpent :: [Ledger.TxOut] -> [Ledger.TxOut] -> Integer
adaSpent inputs outputs = adaInTxs outputs - (adaInTxs inputs - fee)
  where
    adaInTxs = sum . map (Ada.getLovelace . Ada.fromValue . Tx.txOutValue)

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
