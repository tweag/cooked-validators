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
import Plutus.Script.Utils.V1.Scripts (scriptHash)
import qualified PlutusTx
import qualified PlutusTx.Numeric as Pl
import PlutusTx.Prelude
import qualified Prelude as Haskell

-- * Data types

-- | Information on the last bidder and their bid.
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
  = -- | state of an auction where an offer has already been made. The address
    -- is the seller's, the integer is the minimum bid in Lovelaces.
    Offer Pl.PubKeyHash Integer
  | -- | state of an auction with a given seller, minimum bid, and deadline that
    -- has not yet had any bids
    NoBids Pl.PubKeyHash Integer Pl.POSIXTime
  | -- | state of an auction with a given seller and deadline that has had at
    -- least one bid.
    Bidding Pl.PubKeyHash Pl.POSIXTime BidderInfo
  deriving (Haskell.Show)

-- {-# INLINEABLE seller #-}
-- seller :: AuctionState -> Pl.PubKeyHash
-- seller (Offer s _) = s
-- seller (NoBids s _ _) = s
-- seller (Bidding s _ _) = s

-- {-# INLINEABLE bidDeadline #-}
-- bidDeadline :: AuctionState -> Maybe Pl.POSIXTime
-- bidDeadline (Offer _ _) = Nothing
-- bidDeadline (NoBids _ _ t) = Just t
-- bidDeadline (Bidding _ t _) = Just t

PlutusTx.makeLift ''AuctionState
PlutusTx.unstableMakeIsData ''AuctionState

instance Eq AuctionState where
  {-# INLINEABLE (==) #-}
  Offer s m == Offer s' m' = s == s' && m == m'
  NoBids s m t == NoBids s' m' t' = s == s' && m == m' && t == t'
  Bidding s t b == Bidding s' t' b' = s == s' && t == t' && b == b'
  _ == _ = False

-- | Actions to be taken in an auction. This will be the 'RedeemerType'.
data Action
  = -- | redeemer to set the deadline of the auction
    SetDeadline
  | -- | redeemer to make a bid
    Bid BidderInfo
  | -- | redeemer to close the auction. The 'TxOutRef' points to the original
    --  'Offer' UTxO.
    Hammer Pl.TxOutRef
  deriving (Haskell.Show)

instance Eq Action where
  {-# INLINEABLE (==) #-}
  SetDeadline == SetDeadline = True
  Bid bi1 == Bid bi2 = bi1 == bi2
  Hammer o1 == Hammer o2 = o1 == o2
  _ == _ = False

PlutusTx.makeLift ''Action
PlutusTx.unstableMakeIsData ''Action

-- * The minting policy of the thread token

-- | This minting policy controls the thread token of the auction. From the
-- transaction that sets the deadline onwrads, this NFT will belong to the
-- auction validator; its presence proves the authenticity of the auction. Here,
-- we only check that exactly one thread token is minted, enforcing that the
-- appropriate offer utxo, whose hash must be the token name, is consumed. The
-- rest of the necessary checks are performed by 'validSetDeadline'.
--
-- The final 'Hammer' transaction of the auction burns the thread token. This
-- transaction is checked by 'validHammer', so that this minting policy only
-- checks that at exactly one token is burned.
{-# INLINEABLE mkPolicy #-}
mkPolicy :: Pl.TxOutRef -> Pl.ScriptContext -> Bool
mkPolicy offerOref ctx
  | amnt == 1 =
    traceIfFalse
      "Offer UTxO not consumed"
      (any (\i -> Pl.txInInfoOutRef i == offerOref) $ Pl.txInfoInputs txi)
  -- no further checks here since 'validSetDeadline' checks the remaining conditions
  | amnt == -1 =
    True -- no further checks here; 'validHammer' checks everything
  | otherwise = trace "not minting or burning the right amount" False
  where
    txi = Pl.scriptContextTxInfo ctx

    -- the amount of minted tokens whose token name is the hash of the
    -- 'offerOref'
    amnt :: Integer
    amnt = 1

-- foldr
--   ( \(cs, tn, a) n ->
--       if cs == Pl.ownCurrencySymbol ctx && tn == tokenNameFromTxOutRef offerOref
--         then n + a
--         else n
--   )
--   0
--   $ Value.flattenValue (Pl.txInfoMint txi)

threadTokenPolicy :: Scripts.MintingPolicy
threadTokenPolicy =
  Pl.mkMintingPolicyScript
    $$(PlutusTx.compile [||Scripts.mkUntypedMintingPolicy mkPolicy||])

{- Remark, because I spent quite some time digging for this information: Why do we
  directly use the constructors for 'CurrencySymbol' and 'TokenName' in the
  definition of 'threadTokenAssetClassFromOref' and 'tokenNameFromTxOutref'? --
  The point is that we want to use these functions on-chain, and that
  'tokenName' and 'currencySymbol' (note the lower-case initial letters!) expect
  *Haskell* byte strings. See this issue on plutus-apps for some background:

  https://github.com/input-output-hk/plutus-apps/issues/498

  This is also why we can't use 'scriptCurrencySymbol' in the definition of
  'threadTokenAssetClassFromOref'.
-}

-- | Compute the thread token of the auction that sells the lot that was
-- originally paid to the auction validator with the given 'TxOutRef'. TODO Explain this better
{-# INLINEABLE threadToken #-}
threadToken :: Pl.TxOutRef -> Pl.Value
threadToken offerOref = Value.assetClassValue (threadTokenAssetClassFromOref offerOref) 1

{-# INLINEABLE threadTokenAssetClassFromOref #-}
threadTokenAssetClassFromOref :: Pl.TxOutRef -> Value.AssetClass
threadTokenAssetClassFromOref offerOref =
  Value.AssetClass
    ( Value.CurrencySymbol $ getScriptHash $ scriptHash $ getMintingPolicy threadTokenPolicy,
      tokenNameFromTxOutRef offerOref
    )

{-# INLINEABLE tokenNameFromTxOutRef #-}
tokenNameFromTxOutRef :: Pl.TxOutRef -> Pl.TokenName
tokenNameFromTxOutRef (Pl.TxOutRef (Pl.TxId tid) i) =
  (fromBuiltin $ appendByteString tid $ appendByteString "-" $ encodeInteger i)
  where
    -- we know that the numbers we're working with here are non-negative.
    encodeInteger :: Integer -> BuiltinByteString
    encodeInteger n
      | n `quotient` 10 == 0 = encodeDigit n
      | otherwise = encodeInteger (n `quotient` 10) <> encodeDigit (n `remainder` 10)
      where
        encodeDigit :: Integer -> BuiltinByteString
        -- 48 is the ASCII code for '0'
        encodeDigit d = consByteString (d + 48) emptyByteString

-- * The validator and its helpers

-- | Extract an auction state from an output (if it has one)
{-# INLINEABLE outputAuctionState #-}
outputAuctionState :: Pl.TxInfo -> Pl.TxOut -> Maybe AuctionState
outputAuctionState txi o = do
  h <- Pl.txOutDatum o
  Pl.Datum d <- Pl.findDatum h txi
  PlutusTx.fromBuiltinData d

-- | Test that the value paid to the paid public key address is at least the
-- given value
{-# INLINEABLE receivesFrom #-}
receivesFrom :: Pl.TxInfo -> Pl.PubKeyHash -> Pl.Value -> Bool
receivesFrom txi who what = Pl.valuePaidTo txi who `Value.geq` what

-- | To set the deadline of an auction, you must
-- * consume an UTxO with the 'Offer' datum
-- * pay back with the 'NoBids' datum for the same seller and minimum bid, and
--   add the thread token
-- * sign the transaction as the seller
{-# INLINEABLE validSetDeadline #-}
validSetDeadline :: AuctionState -> Pl.ScriptContext -> Bool
validSetDeadline datum ctx =
  let txi = Pl.scriptContextTxInfo ctx
   in case datum of
        Offer seller minbid ->
          let Just (Pl.TxInInfo offerOref offerOut) = Pl.findOwnInput ctx
           in traceIfFalse
                "SetDeadline transaction must be signed by seller"
                (txi `Pl.txSignedBy` seller)
                && traceIfFalse
                  "there must be a 'NoBids' output containing the lot and the thread token"
                  ( any
                      ( \o ->
                          Pl.txOutValue o
                            `Value.geq` (threadToken offerOref <> Pl.txOutValue offerOut)
                            && case outputAuctionState txi o of
                              Just (NoBids seller' minbid' _deadline) ->
                                (seller, minbid) == (seller', minbid')
                              _ -> False
                      )
                      (Pl.getContinuingOutputs ctx)
                  )
        NoBids {} -> trace "Cannot re-set the deadline in 'NoBids' state" False
        Bidding {} -> trace "Cannot re-set the deadline in 'Bidding' state" False

-- | A new bid is valid if
-- * it is made before the bidding deadline
-- * it has been signed by the bidder
-- * it is greater than the last bid (or the minium bid, if it's the first one)
-- * after the transaction:
--    * the state of the auction is 'Bidding' with the new bid and bidder
--    * the validator locks the lot, the new bid, and the thread token with that datum
--    * the last bidder has gotten their money back from the validator
{-# INLINEABLE validBid #-}
validBid :: AuctionState -> Integer -> Pl.PubKeyHash -> Pl.ScriptContext -> Bool
validBid datum bid bidder ctx =
  let txi = Pl.scriptContextTxInfo ctx
      Just (Pl.TxInInfo _ Pl.TxOut {Pl.txOutValue = lockedValue}) = Pl.findOwnInput ctx
   in case datum of
        Offer {} -> trace "Cannot bid on an auction that hasn't yet got a deadline" False
        NoBids seller minBid deadline ->
          traceIfFalse
            "Bidding past the deadline is not permitted"
            (Pl.to deadline `Interval.contains` Pl.txInfoValidRange txi)
            && traceIfFalse "Bid transaction not signed by bidder" (txi `Pl.txSignedBy` bidder)
            && traceIfFalse "Cannot bid less than the minimum bid" (minBid <= bid)
            && traceIfFalse
              "Validator does not lock lot, bid, and thread token with the correct 'Bidding' datum"
              ( any
                  ( \o ->
                      outputAuctionState txi o == Just (Bidding seller deadline (BidderInfo bid bidder))
                        && Pl.txOutValue o `Value.geq` (lockedValue <> Ada.lovelaceValueOf bid)
                  )
                  (Pl.getContinuingOutputs ctx)
              )
        Bidding seller deadline (BidderInfo prevBid prevBidder) ->
          traceIfFalse
            "Bidding past the deadline is not permitted"
            (Pl.to deadline `Interval.contains` Pl.txInfoValidRange txi)
            && traceIfFalse "Bid transaction not signed by bidder" (txi `Pl.txSignedBy` bidder)
            && traceIfFalse "Must bid strictly more than the previous bid" (prevBid < bid)
            && traceIfFalse
              "Validator does not lock lot, bid, and thread token with the correct 'Bidding' datum"
              ( any
                  ( \o ->
                      outputAuctionState txi o == Just (Bidding seller deadline (BidderInfo bid bidder))
                        && Pl.txOutValue o
                          `Value.geq` ( lockedValue
                                          <> Pl.negate (Ada.lovelaceValueOf prevBid)
                                          <> Ada.lovelaceValueOf bid
                                      )
                  )
                  (Pl.getContinuingOutputs ctx)
              )
            && traceIfFalse
              "Previous bidder must get their money back"
              (receivesFrom txi prevBidder $ Ada.lovelaceValueOf prevBid)

-- | A hammer ends the auction. It is valid if
-- * it is made after the bidding deadline
-- * it burns the thread NFT associated with the auction
-- * after the transaction, if there have been bids:
--    * the last bidder has received the lot
--    * the seller has received payment of the highest bid
-- * afer the transaction, if there have been no bids:
--    * the seller gets the lot
{-# INLINEABLE validHammer #-}
validHammer :: AuctionState -> Pl.TxOutRef -> Pl.ScriptContext -> Bool
validHammer datum offerOref ctx =
  let txi = Pl.scriptContextTxInfo ctx
      receives = receivesFrom txi
      Just (Pl.TxInInfo _ Pl.TxOut {Pl.txOutValue = lockedValue}) = Pl.findOwnInput ctx
      threadTokenIsBurned = Pl.txInfoMint txi == Pl.negate (threadToken offerOref)
   in case datum of
        Offer seller _minbid ->
          traceIfFalse "Seller must get the offer back" $
            seller `receives` lockedValue
        NoBids seller _minbid deadline ->
          traceIfFalse
            "Hammer before the deadline is not permitted"
            (Pl.from deadline `Interval.contains` Pl.txInfoValidRange txi)
            && traceIfFalse
              "Hammer does not burn exactly one thread token"
              threadTokenIsBurned
            && traceIfFalse
              "Seller must get the offer back"
              (seller `receives` (lockedValue <> Pl.negate (threadToken offerOref)))
        Bidding seller deadline (BidderInfo lastBid lastBidder) ->
          traceIfFalse
            "Hammer before the deadline is not permitted"
            (Pl.from deadline `Interval.contains` Pl.txInfoValidRange txi)
            && traceIfFalse
              "Hammer does not burn exactly one thread token"
              threadTokenIsBurned
            && traceIfFalse
              "last bidder must get the lot"
              ( lastBidder
                  `receives` ( lockedValue <> Pl.negate (threadToken offerOref)
                                 <> Pl.negate (Ada.lovelaceValueOf lastBid)
                             )
              )
            && traceIfFalse
              "Seller must get the last bid"
              (seller `receives` Ada.lovelaceValueOf lastBid)

{-# INLINEABLE validate #-}
validate :: AuctionState -> Action -> Pl.ScriptContext -> Bool
validate datum redeemer ctx = case redeemer of
  SetDeadline -> validSetDeadline datum ctx
  _ -> True

-- Bid (BidderInfo bid bidder) -> validBid datum bid bidder ctx
-- Hammer offerOref -> validHammer datum offerOref ctx

-- Plutus boilerplate to compile the validator

data Auction

instance Scripts.ValidatorTypes Auction where
  type RedeemerType Auction = Action
  type DatumType Auction = AuctionState

auctionValidator :: Scripts.TypedValidator Auction
auctionValidator =
  Scripts.mkTypedValidator @Auction
    $$(PlutusTx.compile [||validate||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator @AuctionState @Action
