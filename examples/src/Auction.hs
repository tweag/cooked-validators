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

-- | All the data associated with an auction.
data Parameters = Parameters
  { -- | no bids are accepted later than this
    deadline :: L.POSIXTime,
    -- | minimum bid in the auction
    minBid :: Integer,
    -- | address of the seller
    seller :: L.PubKeyHash,
    -- | value that is being auctioned
    lot :: L.Value
  }

-- | The state of the auction. This will be the DatumType.
data AuctionState
  = -- | state of an auction that has not yet had any bids
    NoBids
  | -- | state of an auction that has had at least one bid
    Bidding
      { -- | the last bidder's offer in Ada
        lastBid :: Integer,
        -- | the last bidder's address
        lastBidder :: L.PubKeyHash
      }

instance Eq AuctionState where
  {- INLINEABLE (==) -}
  NoBids == NoBids = True
  Bidding a b == Bidding x y = a == x && b == y
  _ == _ = False

-- | Actions to be taken in an auction. This will be the RedeemerType.
data Action
  = -- | redeemer to make a bid (before the 'deadline')
    Bid
      { -- | the new bidder's offer in Ada
        bid :: Integer,
        -- | the new bidder's address
        bidder :: L.PubKeyHash
      }
  | -- | redeemer to close the auction (after the 'deadline')
    Hammer

{- INLINEABLE bidTimeRange -}
bidTimeRange :: Parameters -> L.POSIXTimeRange
bidTimeRange a = Interval.to (deadline a)

{- INLINEABLE hammerTimeRange -}
hammerTimeRange :: Parameters -> L.POSIXTimeRange
hammerTimeRange a = Interval.from (deadline a)

-- | Extract an auction state from an output (if it has one)

{- INLINEABLE outputAuctionState -}
outputAuctionState :: L.TxInfo -> L.TxOut -> Maybe AuctionState
outputAuctionState txi o = do
  h <- L.txOutDatum o
  L.Datum d <- L.findDatum h txi
  PlutusTx.fromBuiltinData d

-- | Test that the value paid to the given public key address is at
-- least the given value

{- INLINEABLE receivesFrom -}
receivesFrom :: L.TxInfo -> L.PubKeyHash -> L.Value -> Bool
receivesFrom txi who what = L.valuePaidTo txi who `Value.geq` what

-- | A new bid is valid if
-- * it is made before the deadline
-- * it is greater than maximum of the lastBid and the minBid
-- * it has been signed by the bidder
-- * after the transaction
--    * the validator locks both the lot and the new bid
--    * the last bidder has gotten their money back from the validator
--    * the state of the auction is 'Bidding' with the new bid and bidder

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
          "Validator does not lock lot and bid"
          (L.valueLockedBy txi selfh == lot auction <> Ada.lovelaceValueOf bid)
        && case L.getContinuingOutputs ctx of
          [o] ->
            traceIfFalse
              "Not in 'Bidding'-state after bidding"
              (outputAuctionState txi o == Just (Bidding bid bidder))
          _ -> trace "There has to be exactly one continuing output to the validator itself" False
        && case datum of
          NoBids ->
            traceIfFalse "Cannot bid less than the minimum bid" (minBid auction < bid)
          Bidding {lastBid, lastBidder} ->
            traceIfFalse "Cannot bid less than the last bid" (lastBid < bid)
              && traceIfFalse
                "Last bidder is not paid enough"
                (lastBidder `receives` Ada.lovelaceValueOf lastBid)

-- | A hammer ends the auction. It is valid if
-- * it is made after the deadline
-- * after the transaction
--    * the last bidder has received the lot
--    * the seller has received payment

{- INLINEABLE validHammer -}
validHammer :: Parameters -> AuctionState -> L.ScriptContext -> Bool
validHammer auction datum ctx =
  let txi = L.scriptContextTxInfo ctx
      receives = receivesFrom txi
   in traceIfFalse
        "Hammer befofer the deadline is not permitted"
        (hammerTimeRange auction `Interval.contains` L.txInfoValidRange txi)
        && case datum of
          NoBids -> True
          Bidding {lastBid, lastBidder} ->
            traceIfFalse
              "Last bidder does not receive the lot"
              (lastBidder `receives` lot auction)
              && traceIfFalse
                "Seller does not receive last bid"
                (seller auction `receives` Ada.lovelaceValueOf lastBid)

{- INLINEABLE validate -}
validate :: Parameters -> AuctionState -> Action -> L.ScriptContext -> Bool
validate auction datum redeemer ctx = case redeemer of
  Bid {bid, bidder} -> validBid auction datum bid bidder ctx
  Hammer -> validHammer auction datum ctx

-- Plutus boilerplate

data Auction

PlutusTx.makeLift ''AuctionState
PlutusTx.unstableMakeIsData ''AuctionState

PlutusTx.makeLift ''Action
PlutusTx.unstableMakeIsData ''Action

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
