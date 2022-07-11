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
    -- | The asset class of the thread token. It's is needed here to
    -- break a circular dependency between the vaildator and the
    -- minting policy: The minting policy needs to know the (hash of
    -- the) validator, in order to ensure that the thread token is
    -- locked by the validator after the initial transaction. The
    -- validator needs to know the AssetClass of the thread token, so
    -- that it can ensure the token is correctly passed on. In
    -- principle, the validator could compute this AssetClass, if it
    -- knows the minting policy and the TokenName (using
    -- 'Pl.scriptCurrencySymbol' and 'Value.assetClass'). This would
    -- make the minting policy a parameter of the validator, so that
    -- each would depend on the other. This is a way out.
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
    pLotOutRef :: Pl.TxOutRef,
    -- | lot of the auction
    pLot :: Pl.Value
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
  = -- | state of an auction that has not yet had any bids
    NoBids
  | -- | state of an auction that has had at least one bid
    Bidding BidderInfo
  deriving (Haskell.Show)

PlutusTx.makeLift ''AuctionState
PlutusTx.unstableMakeIsData ''AuctionState

instance Eq AuctionState where
  {-# INLINEABLE (==) #-}
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

instance Eq Action where
  {-# INLINEABLE (==) #-}
  Bid bi1 == Bid bi2 = bi1 == bi2
  Hammer == Hammer = True
  _ == _ = False

PlutusTx.makeLift ''Action
PlutusTx.unstableMakeIsData ''Action

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
mkPolicy :: PolicyParams -> Pl.Address -> Pl.ScriptContext -> Bool
mkPolicy (PolicyParams tName lotOref lot) validator ctx
  | amnt == Just 1 =
    traceIfFalse
      "Lot UTxO not consumed"
      (any (\i -> Pl.txInInfoOutRef i == lotOref) $ Pl.txInfoInputs txi)
      && case filter
        (\o -> Pl.txOutAddress o == validator)
        (Pl.txInfoOutputs txi) of
        [o] ->
          traceIfFalse
            "Validator does not receive the lot and the thread token of freshly opened auction"
            (Pl.txOutValue o `Value.geq` (lot <> token))
            && traceIfFalse
              "Validator not in 'NoBids'-state on freshly opened auction"
              (outputAuctionState txi o == Just NoBids)
        _ -> trace "There must be exactly one output to the validator on a fresh auction" False
  | amnt == Just (-1) =
    True -- no further checks here; 'validHammer' checks everything
  | otherwise = trace "not minting or burning the right amount" False
  where
    txi = Pl.scriptContextTxInfo ctx
    Pl.Minting me = Pl.scriptContextPurpose ctx

    token :: Pl.Value
    token = Value.singleton me tName 1

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

threadTokenAssetClassFromOrefAndLot :: Pl.TxOutRef -> Pl.Value -> Value.AssetClass
threadTokenAssetClassFromOrefAndLot lotOutRef lot =
  Value.assetClass
    (Pl.scriptCurrencySymbol $ threadTokenPolicy $ PolicyParams threadTokenName lotOutRef lot)
    threadTokenName

-- * The validator and its helpers

{- INLINEABLE bidTimeRange -}
bidTimeRange :: ValParams -> Pl.POSIXTimeRange
bidTimeRange a = Interval.to (bidDeadline a)

{- INLINEABLE hammerTimeRange -}
hammerTimeRange :: ValParams -> Pl.POSIXTimeRange
hammerTimeRange a = Interval.from (bidDeadline a)

-- | Extract an auction state from an output (if it has one)

{- INLINEABLE outputAuctionState -}
outputAuctionState :: Pl.TxInfo -> Pl.TxOut -> Maybe AuctionState
outputAuctionState txi o = do
  h <- Pl.txOutDatum o
  Pl.Datum d <- Pl.findDatum h txi
  PlutusTx.fromBuiltinData d

-- | Test that the value paid to the giv,en public key address is at
-- least the given value

{- INLINEABLE receivesFrom -}
receivesFrom :: Pl.TxInfo -> Pl.PubKeyHash -> Pl.Value -> Bool
receivesFrom txi who what = Pl.valuePaidTo txi who `Value.geq` what

-- | A new bid is valid if
-- * it is made before the bidding deadline
-- * it has been signed by the bidder
-- * it is greater than maximum of the lastBid and the minBid
-- * after the transaction:
--    * the state of the auction is 'Bidding' with the new bid and bidder
--    * the validator locks the lot, the new bid, and the thread token
--    * the last bidder has gotten their money back from the validator

{- INLINEABLE validBid -}
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

{- INLINEABLE validHammer -}
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
validate :: ValParams -> AuctionState -> Action -> Pl.ScriptContext -> Bool
validate auction datum redeemer ctx = case redeemer of
  Bid (BidderInfo bid bidder) -> validBid auction datum bid bidder ctx
  Hammer -> validHammer auction datum ctx

-- Plutus boilerplate to compile the validator

data Auction

instance Scripts.ValidatorTypes Auction where
  type RedeemerType Auction = Action
  type DatumType Auction = AuctionState

auctionValidator :: ValParams -> Scripts.TypedValidator Auction
auctionValidator =
  Scripts.mkTypedValidatorParam @Auction
    $$(PlutusTx.compile [||validate||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator @AuctionState @Action
