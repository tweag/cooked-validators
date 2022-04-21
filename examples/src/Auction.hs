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

-- These language extensions are just what Split.hs uses, I honestly
-- have no idea what most of them do or if I actually need them.

-- | Arrange an auction with a preset deadline and minimum bid.
module Auction where

import qualified Ledger as L
import qualified Ledger.Ada as Ada
import qualified Ledger.Interval as Interval
import Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude as Haskell

-- * Data types

-- | All the statically known data associated with an auction
data Parameters' = Parameters'
  { -- | no bids are accepted later than this
    bidDeadline' :: L.POSIXTime,
    -- | minimum bid in Lovelace
    minBid' :: Integer,
    -- | value that is being auctioned
    lot' :: L.Value
  }
  deriving (Haskell.Show)

-- | All data associated with an auction, including the data only
-- known after the opening transaction
data Parameters = Parameters
  { staticParameters :: Parameters',
    -- | address of the seller
    seller :: L.PubKeyHash,
    -- | autref of the utxo the seller spent the lot from (needed as a parameter for the minting policy of the thread token)
    lotOutRef :: L.TxOutRef,
    -- | assert class of the threadToken
    threadTokenAssetClass :: Value.AssetClass
  }
  deriving (Haskell.Show)

bidDeadline :: Parameters -> L.POSIXTime
bidDeadline = bidDeadline' . staticParameters

minBid :: Parameters -> Integer
minBid = minBid' . staticParameters

lot :: Parameters -> L.Value
lot = lot' . staticParameters

data BidderInfo = BidderInfo
  { -- | the last bidder's offer in Ada
    bid :: Integer,
    -- | the last bidder's address
    bidder :: L.PubKeyHash
  }
  deriving (Haskell.Show)

instance Eq BidderInfo where
  BidderInfo a b == BidderInfo x y = a == x && b == y

-- | The state of the auction. This will be the 'DatumType'.
data AuctionState
  = -- | state of an auction that has not yet had any bids
    NoBids
  | -- | state of an auction that has had at least one bid
    Bidding BidderInfo
  deriving (Haskell.Show)

instance Eq AuctionState where
  {- INLINEABLE (==) -}
  NoBids == NoBids = True
  Bidding a == Bidding x = a == x
  _ == _ = False

-- | Actions to be taken in an auction. This will be the 'RedeemerType'.
data Action
  = -- | redeemer to make a bid (before the 'bidDeadline')
    Bid BidderInfo
  | -- | redeemer to close the auction (after the 'bidDeadline')
    Hammer
  deriving (Haskell.Show)

-- * The minting policy of the thread token

-- | This minting policy controls the thread token of an auction. This
-- token belongs to the validator of the auction, and must be minted
-- in the first transaction, for which this policy ensures that
-- * exactly one thread token is minted, by forcing an UTxO to be consumed
-- * after the transaction:
--     * the validator locks the thread token and the lot of the auction
--     * the validator is in 'NoBids' state
-- The final "hammer" transaction of the auction is the one that burns
-- the thread token. This transaction has its own validator
-- 'validHammer', so that this minting policy only checks that at
-- exactly one token is burned.
{-# INLINEABLE mkPolicy #-}
mkPolicy :: Value.TokenName -> L.TxOutRef -> L.Value -> L.Address -> L.ScriptContext -> Bool
mkPolicy tName lotOref lot validator ctx
  | amnt == Just 1 =
    traceIfFalse
      "Lot UTxO not consumed"
      (any (\i -> L.txInInfoOutRef i == lotOref) $ L.txInfoInputs txi)
      && case filter
        (\o -> L.txOutAddress o == validator)
        (L.txInfoOutputs txi) of
        [o] ->
          traceIfFalse
            "Validator does not receive the lot and the thread token of freshly opened auction"
            (L.txOutValue o `Value.geq` (lot <> token))
            && traceIfFalse
              "Validator not in 'NoBids'-state on freshly opened auction"
              (outputAuctionState txi o == Just NoBids)
        _ -> trace "There must be exactly one output to the validator on a fresh auction" False
  | amnt == Just (-1) =
    True -- no further checks here; 'validHammer' checks everything
  | otherwise = trace "not minting or burning the right amount" False
  where
    txi = L.scriptContextTxInfo ctx
    L.Minting me = L.scriptContextPurpose ctx

    token :: L.Value
    token = Value.singleton me tName 1

    amnt :: Maybe Integer
    amnt = case Value.flattenValue (L.txInfoMint txi) of
      [(cs, tn, a)] | cs == L.ownCurrencySymbol ctx && tn == tName -> Just a
      _ -> Nothing

{-# INLINEABLE threadTokenName #-}
threadTokenName :: Value.TokenName
threadTokenName = Value.tokenName "AuctionToken"

threadTokenPolicy :: Value.TokenName -> L.TxOutRef -> L.Value -> Scripts.MintingPolicy
threadTokenPolicy tName oref lot =
  L.mkMintingPolicyScript $
    $$(PlutusTx.compile [||\n o l -> Scripts.wrapMintingPolicy $ mkPolicy n o l||])
      `PlutusTx.applyCode` PlutusTx.liftCode tName
      `PlutusTx.applyCode` PlutusTx.liftCode oref
      `PlutusTx.applyCode` PlutusTx.liftCode lot

threadTokenSymbol :: L.TxOutRef -> L.Value -> L.CurrencySymbol
threadTokenSymbol oref lot = L.scriptCurrencySymbol $ threadTokenPolicy threadTokenName oref lot

threadTokenAssetClassFromOrefAndLot :: L.TxOutRef -> L.Value -> Value.AssetClass
threadTokenAssetClassFromOrefAndLot lotOutRef lot =
  Value.assetClass
    (threadTokenSymbol lotOutRef lot)
    threadTokenName

-- * The validator and its helpers

{- INLINEABLE bidTimeRange -}
bidTimeRange :: Parameters -> L.POSIXTimeRange
bidTimeRange a = Interval.to (bidDeadline a)

{- INLINEABLE hammerTimeRange -}
hammerTimeRange :: Parameters -> L.POSIXTimeRange
hammerTimeRange a = Interval.from (bidDeadline a)

-- | Extract an auction state from an output (if it has one)

{- INLINEABLE outputAuctionState -}
outputAuctionState :: L.TxInfo -> L.TxOut -> Maybe AuctionState
outputAuctionState txi o = do
  h <- L.txOutDatum o
  L.Datum d <- L.findDatum h txi
  PlutusTx.fromBuiltinData d

-- | Test that the value paid to the giv,en public key address is at
-- least the given value

{- INLINEABLE receivesFrom -}
receivesFrom :: L.TxInfo -> L.PubKeyHash -> L.Value -> Bool
receivesFrom txi who what = L.valuePaidTo txi who `Value.geq` what

-- | A new bid is valid if
-- * it is made before the bidding deadline
-- * it has been signed by the bidder
-- * it is greater than maximum of the lastBid and the minBid
-- * after the transaction:
--    * the state of the auction is 'Bidding' with the new bid and bidder
--    * the validator locks the lot, the new bid, and the thread token
--    * the last bidder has gotten their money back from the validator

{- INLINEABLE validBid -}
validBid :: Parameters -> AuctionState -> Integer -> L.PubKeyHash -> L.ScriptContext -> Bool
validBid auction datum bid bidder ctx =
  let txi = L.scriptContextTxInfo ctx
      selfh = L.ownHash ctx
      receives = receivesFrom txi
   in traceIfFalse
        "Bidding past the deadline is not permitted"
        (bidTimeRange auction `Interval.contains` L.txInfoValidRange txi)
        && traceIfFalse "Bid transaction not signed by bidder" (txi `L.txSignedBy` bidder)
        && traceIfFalse
          "Validator does not lock lot, bid, and thread token"
          ( L.valueLockedBy txi selfh
              `Value.geq` ( lot auction <> Ada.lovelaceValueOf bid
                              <> Value.assetClassValue (threadTokenAssetClass auction) 1
                          )
          )
        && case L.getContinuingOutputs ctx of
          [o] ->
            traceIfFalse
              "Not in the correct 'Bidding'-state after bidding"
              (outputAuctionState txi o == Just (Bidding (BidderInfo bid bidder)))
          _ -> trace "There has to be exactly one continuing output to the validator itself" False
        && case datum of
          NoBids ->
            traceIfFalse "Cannot bid less than the minimum bid" (minBid auction <= bid)
          Bidding (BidderInfo lastBid lastBidder) ->
            traceIfFalse "Must bid more than the last bid" (lastBid < bid)
              && traceIfFalse
                "Last bidder is not paid back"
                (lastBidder `receives` Ada.lovelaceValueOf lastBid)

-- | A hammer ends the auction. It is valid if
-- * it is made after the bidding deadline
-- * it burns the thread NFT associated with the auction
-- * after the transaction, if there have been bids:
--    * the last bidder has received the lot
--    * the seller has received payment of the highest bid
-- * afer the transaction, if there have been no bids:
--    * the seller gets the lot

{- INLINEABLE validHammer -}
validHammer :: Parameters -> AuctionState -> L.ScriptContext -> Bool
validHammer auction datum ctx =
  let txi = L.scriptContextTxInfo ctx
      receives = receivesFrom txi
   in traceIfFalse
        "Hammer before the deadline is not permitted"
        (hammerTimeRange auction `Interval.contains` L.txInfoValidRange txi)
        && traceIfFalse
          "Hammer does not burn exactly one thread token"
          (L.txInfoMint txi == Value.assetClassValue (threadTokenAssetClass auction) (-1))
        && case datum of
          NoBids ->
            traceIfFalse
              "Seller does not get locked lot back"
              (seller auction `receives` lot auction)
          Bidding (BidderInfo lastBid lastBidder) ->
            traceIfFalse
              "Last bidder does not receive the lot"
              (lastBidder `receives` lot auction)
              && traceIfFalse
                "Seller does not receive last bid"
                (seller auction `receives` Ada.lovelaceValueOf lastBid)

{- INLINEABLE validate -}
validate :: Parameters -> AuctionState -> Action -> L.ScriptContext -> Bool
validate auction datum redeemer ctx = case redeemer of
  Bid (BidderInfo bid bidder) -> validBid auction datum bid bidder ctx
  Hammer -> validHammer auction datum ctx

-- Plutus boilerplate

data Auction

PlutusTx.makeLift ''BidderInfo
PlutusTx.unstableMakeIsData ''BidderInfo

PlutusTx.makeLift ''AuctionState
PlutusTx.unstableMakeIsData ''AuctionState

PlutusTx.makeLift ''Action
PlutusTx.unstableMakeIsData ''Action

PlutusTx.makeLift ''Parameters'
PlutusTx.unstableMakeIsData ''Parameters'

PlutusTx.makeLift ''Parameters
PlutusTx.unstableMakeIsData ''Parameters

instance Scripts.ValidatorTypes Auction where
  type RedeemerType Auction = Action
  type DatumType Auction = AuctionState

auctionValidator :: Parameters -> Scripts.TypedValidator Auction
auctionValidator =
  Scripts.mkTypedValidatorParam @Auction
    $$(PlutusTx.compile [||validate||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @AuctionState @Action
