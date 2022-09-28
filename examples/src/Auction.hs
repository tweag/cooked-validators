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

-- These language extensions are just what Split.hs uses

-- | Arrange an auction with a preset deadline and minimum bid.
module Auction where

import qualified Ledger as Pl
import qualified Ledger.Ada as Ada
import qualified Ledger.Interval as Interval
import Ledger.Scripts as Pl
import Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import qualified Plutus.Script.Utils.V1.Scripts as Pl (scriptCurrencySymbol)
import qualified Plutus.V1.Ledger.Api as Api
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude as Haskell

-- * Data types

-- | All the statically known data associated with an auction that the
-- validator needs to know
data StaticValParams = StaticValParams
  { -- | no bids are accepted later than this
    bidDeadline' :: Pl.POSIXTime,
    -- | minimum bid in Lovelace
    minBid' :: Integer,
    -- | value that is being auctioned
    lot' :: Pl.Value
  }
  deriving (Haskell.Show)

-- some Plutus magic to compile the data type
PlutusTx.makeLift ''StaticValParams
PlutusTx.unstableMakeIsData ''StaticValParams

-- | All data for the validator associated with an auction, including
-- the data only known after the opening transaction
data ValParams = ValParams
  { staticValParams :: StaticValParams,
    -- | address of the seller
    seller :: Pl.PubKeyHash,
    -- | the asset class of the thread token
    threadTokenAssetClass :: Value.AssetClass
  }
  deriving (Haskell.Show)

PlutusTx.makeLift ''ValParams
PlutusTx.unstableMakeIsData ''ValParams

bidDeadline :: ValParams -> Pl.POSIXTime
bidDeadline = bidDeadline' . staticValParams

minBid :: ValParams -> Integer
minBid = minBid' . staticValParams

lot :: ValParams -> Pl.Value
lot = lot' . staticValParams

-- | All data the minting policy of the thread token needs to
-- know. These are known after the opening transaction
data PolicyParams = PolicyParams
  { -- | TokenName of the thread token
    pThreadTokenName :: Value.TokenName,
    -- | outref of the utxo the seller spent the lot from
    pLotOutRef :: Pl.TxOutRef
  }

PlutusTx.makeLift ''PolicyParams
PlutusTx.unstableMakeIsData ''PolicyParams

data BidderInfo = BidderInfo
  { -- | the last bidder's offer in Ada
    bid :: Integer,
    -- | the last bidder's address
    bidder :: Pl.PubKeyHash
  }
  deriving (Haskell.Show)

PlutusTx.makeLift ''BidderInfo
PlutusTx.unstableMakeIsData ''BidderInfo

instance Eq BidderInfo where
  {-# INLINEABLE (==) #-}
  BidderInfo a b == BidderInfo x y = a == x && b == y

-- | The state of the auction. This will be the 'DatumType'.
data AuctionState
  = -- | state of an auction where an offer has already been made (but the offer
    -- UTxO has not yet been checked, the thread NFT has not yet been minted)
    Offer
  | -- | state of an auction that has not yet had any bids
    NoBids
  | -- | state of an auction that has had at least one bid
    Bidding BidderInfo
  deriving (Haskell.Show)

PlutusTx.makeLift ''AuctionState
PlutusTx.unstableMakeIsData ''AuctionState

instance Eq AuctionState where
  {-# INLINEABLE (==) #-}
  Offer == Offer = True
  NoBids == NoBids = True
  Bidding a == Bidding x = a == x
  _ == _ = False

-- | Actions to be taken in an auction. This will be the 'RedeemerType'.
data Action
  = -- |
    CheckOffer
  | -- | redeemer to make a bid (before the 'bidDeadline')
    Bid BidderInfo
  | -- | redeemer to close the auction (after the 'bidDeadline')
    Hammer
  deriving (Haskell.Show)

instance Eq Action where
  {-# INLINEABLE (==) #-}
  CheckOffer == CheckOffer = True
  Bid bi1 == Bid bi2 = bi1 == bi2
  Hammer == Hammer = True
  _ == _ = False

PlutusTx.makeLift ''Action
PlutusTx.unstableMakeIsData ''Action

-- * The minting policy of the thread token

-- | This minting policy controls the thread token of the auction. The token
-- will belong auction validator, and must be minted in the second trancation
-- (which consumes an "unchecked" UTxO with the 'Offer' datum and pays a
-- 'NoBids' Utxo is back to the auction validator). The minting policy only
-- checks that this transaction mints exactly one thread token, by forcing an
-- UTxO to be consumed. The rest of the necessary checks are performed by
-- 'validCheckOffer'.
--
-- The final 'Hammer' transaction of the auction burns the thread token. This
-- transaction has its own validator 'validHammer', so that this minting policy
-- only checks that at exactly one token is burned.
{-# INLINEABLE mkPolicy #-}
mkPolicy :: PolicyParams -> () -> Pl.ScriptContext -> Bool
mkPolicy (PolicyParams tName lotOref) _ ctx
  | amnt == Just 1 =
    traceIfFalse
      "Lot UTxO not consumed"
      (any (\i -> Pl.txInInfoOutRef i == lotOref) $ Pl.txInfoInputs txi)
  -- no further checks here; 'validCheckOffer' checks the remaining condition
  -- (that the auction validator sends a checked UTxO in 'NoBids' state back
  -- to itself)

  -- && case filter
  --   (\o -> Pl.txOutAddress o == validator)
  --   (Pl.txInfoOutputs txi) of
  --   [o] ->
  --     traceIfFalse
  --       "Validator does not receive the lot and the thread token of freshly opened auction"
  --       (Pl.txOutValue o `Value.geq` (lot <> token))
  --       && traceIfFalse
  --         "Validator not in 'NoBids'-state on freshly opened auction"
  --         (outputAuctionState txi o == Just NoBids)
  --   _ -> trace "There must be exactly one output to the validator on a fresh auction" False
  | amnt == Just (-1) =
    True -- no further checks here; 'validHammer' checks everything
  | otherwise = trace "not minting or burning the right amount" False
  where
    txi = Pl.scriptContextTxInfo ctx

    amnt :: Maybe Integer
    amnt = case Value.flattenValue (Pl.txInfoMint txi) of
      [(cs, tn, a)] | cs == Pl.ownCurrencySymbol ctx && tn == tName -> Just a
      _ -> Nothing

{-# INLINEABLE threadTokenName #-}
threadTokenName :: Value.TokenName
threadTokenName = Value.tokenName "AuctionToken"

threadTokenPolicy :: PolicyParams -> Scripts.MintingPolicy
threadTokenPolicy pars =
  Api.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.mkUntypedMintingPolicy . mkPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode pars

threadTokenAssetClassFromOref :: Pl.TxOutRef -> Value.AssetClass
threadTokenAssetClassFromOref lotOutRef =
  Value.assetClass
    (Pl.scriptCurrencySymbol $ threadTokenPolicy $ PolicyParams threadTokenName lotOutRef)
    threadTokenName

-- * The validator and its helpers

{-# INLINEABLE bidTimeRange #-}
bidTimeRange :: ValParams -> Pl.POSIXTimeRange
bidTimeRange a = Interval.to (bidDeadline a)

{-# INLINEABLE hammerTimeRange #-}
hammerTimeRange :: ValParams -> Pl.POSIXTimeRange
hammerTimeRange a = Interval.from (bidDeadline a)

-- | Extract an auction state from an output (if it has one)
{-# INLINEABLE outputAuctionState #-}
outputAuctionState :: Pl.TxInfo -> Pl.TxOut -> Maybe AuctionState
outputAuctionState txi o = do
  h <- Pl.txOutDatum o
  Pl.Datum d <- Pl.findDatum h txi
  PlutusTx.fromBuiltinData d

-- | Test that the value paid to the giv,en public key address is at
-- least the given value
{-# INLINEABLE receivesFrom #-}
receivesFrom :: Pl.TxInfo -> Pl.PubKeyHash -> Pl.Value -> Bool
receivesFrom txi who what = Pl.valuePaidTo txi who `Value.geq` what

validCheckOffer :: ValParams -> AuctionState -> Pl.ScriptContext -> Bool
validCheckOffer _auction datum ctx =
  case datum of
    Offer ->
      let Just (Pl.TxInInfo _ consumed) = Pl.findOwnInput ctx
       in traceIfFalse "there must be at least one continuing output in 'NoBids' state" $
            any
              ((`Value.geq` Pl.txOutValue consumed) . Pl.txOutValue)
              (Pl.getContinuingOutputs ctx)
    NoBids -> trace "Cannot re-check in 'NoBids' state" False
    Bidding _ -> trace "Cannot re-check in 'Bidding' state" False

-- | A new bid is valid if
-- * it is made before the bidding deadline
-- * it has been signed by the bidder
-- * it is greater than maximum of the lastBid and the minBid
-- * after the transaction:
--    * the state of the auction is 'Bidding' with the new bid and bidder
--    * the validator locks the lot, the new bid, and the thread token
--    * the last bidder has gotten their money back from the validator
{-# INLINEABLE validBid #-}
validBid :: ValParams -> AuctionState -> Integer -> Pl.PubKeyHash -> Pl.ScriptContext -> Bool
validBid auction datum bid bidder ctx =
  let txi = Pl.scriptContextTxInfo ctx
      selfh = Pl.ownHash ctx
      receives = receivesFrom txi
   in traceIfFalse
        "Bidding past the deadline is not permitted"
        (bidTimeRange auction `Interval.contains` Pl.txInfoValidRange txi)
        && traceIfFalse "Bid transaction not signed by bidder" (txi `Pl.txSignedBy` bidder)
        && traceIfFalse
          "Validator does not lock lot, bid, and thread token"
          ( Pl.valueLockedBy txi selfh
              `Value.geq` ( lot auction <> Ada.lovelaceValueOf bid
                              <> Value.assetClassValue (threadTokenAssetClass auction) 1
                          )
          )
        && case Pl.getContinuingOutputs ctx of
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
{-# INLINEABLE validHammer #-}
validHammer :: ValParams -> AuctionState -> Pl.ScriptContext -> Bool
validHammer auction datum ctx =
  let txi = Pl.scriptContextTxInfo ctx
      receives = receivesFrom txi
   in traceIfFalse
        "Hammer before the deadline is not permitted"
        (hammerTimeRange auction `Interval.contains` Pl.txInfoValidRange txi)
        && traceIfFalse
          "Hammer does not burn exactly one thread token"
          (Pl.txInfoMint txi == Value.assetClassValue (threadTokenAssetClass auction) (-1))
        && case datum of
          Offer -> trace "Cannot hammer on an unchecked offer" False
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

{-# INLINEABLE validate #-}
validate :: ValParams -> AuctionState -> Action -> Pl.ScriptContext -> Bool
validate auction datum redeemer ctx = case redeemer of
  CheckOffer -> validCheckOffer auction datum ctx
  Bid (BidderInfo bid bidder) -> validBid auction datum bid bidder ctx
  Hammer -> validHammer auction datum ctx

{-# INLINEABLE isBid #-}
isBid :: Action -> Bool
isBid (Bid _) = True
isBid _ = False

{-# INLINEABLE receivesToken #-}
receivesToken :: ValParams -> Pl.ScriptContext -> Bool
receivesToken auction ctx =
  let txi = Pl.scriptContextTxInfo ctx
      selfh = Pl.ownHash ctx
   in Value.assetClassValueOf (Pl.valueLockedBy txi selfh) (threadTokenAssetClass auction) == 1

-- Plutus boilerplate to compile the validator

data Auction

instance Scripts.ValidatorTypes Auction where
  type RedeemerType Auction = Action
  type DatumType Auction = AuctionState

compiledValidate :: PlutusTx.CompiledCode (ValParams -> AuctionState -> Action -> Pl.ScriptContext -> Bool)
compiledValidate = $$(PlutusTx.compile [||validate||])

auctionValidator :: ValParams -> Scripts.TypedValidator Auction
auctionValidator =
  Scripts.mkTypedValidatorParam @Auction
    compiledValidate
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator @AuctionState @Action
