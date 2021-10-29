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

-- Parameters of the tests -----

-- Some coins for the market
nbCoinsMarketInit :: Integer
nbCoinsMarketInit = 50

-- Some coins for a wallet
nbCoinsWalletInit :: Integer
nbCoinsWalletInit = 10

constant :: Integer
constant = 5000

--------------------------------

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
      -- Reference to tx input to be spent when minting
      txInputToSpendRef
      -- How many to mint
      (AssocMap.fromList [(nftName, 1), (coinsName, nbCoinsMarketInit + nbCoinsWalletInit)])
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

  return . runMockChain $ do

    -- Transaction 0: minting
    -- Wallet 1 mints the nft and coins and shares it among the market and wallet 2
    validateTxFromSkeleton $ TxSkel
      (wallet 1)
      [ PaysPK (walletPKHash $ wallet 1) mempty
      , Mints [policy] (oneNft <> coins (nbCoinsWalletInit + nbCoinsMarketInit))
      , PaysScript marketValidator
        [ (Market.MarketDatum nbCoinsMarketInit
        , oneNft <> coins nbCoinsMarketInit) ]
      , PaysPK (walletPKHash $ wallet 2) (coins nbCoinsWalletInit)
      ]

    -- Transaction 1: the market sells 10 coins to wallet 2
    -- A golden coin is worth 100 Ada
    [(output, datum)] <- scriptUtxosSuchThat marketValidator (\_ _ -> True)
    validateTxFromSkeleton $ TxSkel
      (wallet 2)
      [ SpendsScript marketValidator Market.Sell (output, datum)
      , PaysScript marketValidator [(Market.MarketDatum 40, oneNft <> ada 1000 <> coins 40)]
      , PaysPK (walletPKHash (wallet 2)) (coins 10)
      ]

    -- Transaction 2: the market sells 4 coins to wallet 3
    -- A golden coin is now worth 125 Ada (500 for 4 coins)
    -- The market output has also 1000 Ada to be given back (so 1500 in total)
    [(output, datum)] <- scriptUtxosSuchThat marketValidator (\_ _ -> True)
    validateTxFromSkeleton $ TxSkel
      (wallet 3)
      [ SpendsScript marketValidator Market.Sell (output, datum)
      , PaysScript marketValidator [(Market.MarketDatum 36, ada 1500 <> oneNft <> coins 36)]
      , PaysPK (walletPKHash (wallet 3)) (coins 4)
      ]

    -- Transaction 3: the market buys 9 coins to wallet 2
    -- A golden coin is now worth 1250/9
    -- The market output had 1500 ada and 36 golden coins
    -- so that's 250 ada and 45 coins to give back
    [(output, datum)] <- scriptUtxosSuchThat marketValidator (\_ _ -> True)
    validateTxFromSkeleton $ TxSkel
      (wallet 2)
      [ SpendsScript marketValidator Market.Buy (output, datum)
      , PaysScript marketValidator [(Market.MarketDatum 45, oneNft <> coins 45 <> ada 250)]
      , PaysPK (walletPKHash (wallet 2)) (ada 1250 <> coins 1)
      ]
  
-- Test spec
spec :: Spec
spec = do
  it "succeeds on the example run" $ do
    run1 `shouldSatisfy` maybe False isRight
