{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module MarketSpec where

import qualified Ledger
import qualified Ledger.Ada                as Ada
import PlutusTx.Prelude
import qualified Plutus.Contracts.Currency as Currency
import qualified Ledger.Value              as Value
import qualified Ledger.Contexts           as Contexts
import qualified PlutusTx.AssocMap         as AssocMap
import qualified Ledger.Typed.Scripts     as TScripts

import Cooked.MockChain
import Cooked.Tx.Constraints
import Cooked.Tx.Generator

import qualified MarketMaker as Market

import Test.Hspec

-- Parameters of the tests

nbCoinsInit :: Integer
nbCoinsInit = 50

adaForACoinInit :: Integer
adaForACoinInit = 100

constant :: Integer
constant = nbCoinsInit * adaForACoinInit

mParams :: Maybe Market.MarketParams
mParams = do
  nftAssetClass <- mNftAssetClass
  coinsAssetClass <- mCoinsAssetClass
  return $ Market.MarketParams nftAssetClass coinsAssetClass constant

mMarketValidator :: Maybe (TScripts.TypedValidator Market.Market)
mMarketValidator = Market.marketValidator <$> mParams

-- Minting policy
nftName :: Value.TokenName
nftName = Value.TokenName "MarketNFT"

coinsName :: Value.TokenName
coinsName = Value.TokenName "GoldenCoins"

mPolicy :: Maybe TScripts.MintingPolicy
mPolicy = do
  txInputToSpendRef <- mTxInputToSpendRef
  txInputToSpendRef <- txInputToSpendRef
  return $ Currency.curPolicy $
    Currency.OneShotCurrency
      txInputToSpendRef -- Reference to tx input to be spent when minting
      (AssocMap.fromList [(nftName, 1), (coinsName, nbCoinsInit)]) -- How many to mint: 1
  where
    mTxInputToSpendRef = either (const Nothing) (Just . fst) eitherTxRef
    eitherTxRef = runMockChain $ do
      utxos <- pkUtxos (walletPKHash $ wallet 1)
      case utxos of
        [(Contexts.TxOutRef txId i, _)] -> return (Just (txId, i))
        _ -> return Nothing

mCurrencySymbol :: Maybe Ledger.CurrencySymbol
mCurrencySymbol = Contexts.scriptCurrencySymbol <$> mPolicy

mNftAssetClass :: Maybe Ledger.AssetClass
mNftAssetClass = do
  nftCurrencySymbol <- mCurrencySymbol
  return $ Value.assetClass nftCurrencySymbol nftName

mCoinsAssetClass :: Maybe Ledger.AssetClass
mCoinsAssetClass = do
  nftCurrencySymbol <- mCurrencySymbol
  return $ Value.assetClass nftCurrencySymbol coinsName

-- Example run

run1 :: Maybe (Either MockChainError ((), UtxoState))
run1 = do
  marketValidator     <- mMarketValidator
  policy              <- mPolicy
  nftAssetClass       <- mNftAssetClass
  coinsAssetClass     <- mCoinsAssetClass
  let oneNft = Value.assetClassValue nftAssetClass 1

  let coins = Value.assetClassValue coinsAssetClass
  let ada = Ada.lovelaceValueOf

  let initialValue = oneNft <> coins nbCoinsInit
  let initialDatum = Market.MarketDatum nbCoinsInit
  return . runMockChain $ do

    -- Transaction 0: minting
    validateTxFromSkeleton $ TxSkel
      (wallet 1)
      [ PaysPK (walletPKHash $ wallet 1) mempty
      , Mints [policy] initialValue
      , PaysScript marketValidator [(initialDatum, initialValue)]
      ]

    -- Transaction 1: the market sells 10 coins to wallet 2
    [(output, datum)] <- scriptUtxosSuchThat marketValidator (\_ _ -> True)
    validateTxFromSkeleton $ TxSkel
      (wallet 2)
      [ SpendsScript marketValidator Market.Sell (output, datum)
      , PaysScript marketValidator [(Market.MarketDatum 40, ada 1000 <> oneNft <> coins 40)]
      , PaysPK (walletPKHash (wallet 2)) (coins 10)
      ]

    -- Transaction 2: the market sells 4 coins to wallet 3
    [(output, datum)] <- scriptUtxosSuchThat marketValidator (\_ _ -> True)
    validateTxFromSkeleton $ TxSkel
      (wallet 3)
      [ SpendsScript marketValidator Market.Sell (output, datum)
      , PaysScript marketValidator [(Market.MarketDatum 36, ada (4 * 125) <> oneNft <> coins 36)]
      , PaysPK (walletPKHash (wallet 3)) (coins 4)
      ]

    -- Transaction 3: the market buys 8 coins to wallet 2
    [(output, datum)] <- scriptUtxosSuchThat marketValidator (\_ _ -> True)
    validateTxFromSkeleton $ TxSkel
      (wallet 2)
      [ SpendsScript marketValidator Market.Buy (output, datum)
      , PaysScript marketValidator [(Market.MarketDatum 44, oneNft <> coins 8)]
      , PaysPK (walletPKHash (wallet 2)) (ada 1350)
      ]
  
-- Test spec
spec :: Spec
spec = do
  it "succeeds on the example run" $ do
    run1 `shouldSatisfy` maybe False isRight
