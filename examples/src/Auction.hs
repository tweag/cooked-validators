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
import qualified Plutus.Script.Utils.V1.Scripts as Pl
import qualified PlutusTx
import qualified PlutusTx.Numeric as Pl
import PlutusTx.Prelude
import qualified Prelude as Haskell

{-

Brief overview of the contract
------------------------------

There are four transactions involved in this contract:

1. Making an offer (off-chain implemented by 'txOffer'). Anyone who wishes to
   sell something can just pay it to the 'auctionValidator' with the 'Offer'
   datum specifiying the seller's address (which is the address that will at the
   end of the auction be paid the winning bid, or, if there have been no bids,
   receive the offer back) and the minimum bid. No checks are involved in this
   transaction.

The UTxO at the 'auctionValidator' that contains this initial offer
parameterises the rest of the auction. We shall call it the _offer UTxO_ of the
auction.

2. Setting the deadline of the auction (off-chain implemented by
   'txSetDeadline'). This transaction consumes the offer UTxO and returns an
   UTxO to the 'auctionValidator' with the 'NoBids' datum, which contains

   - the value originally offered, and

   - the thread NFT. This NFT ensures the authenticity of the auction from that
     point onwards, and its token name is uniquely derived from the offer UTxO
     (it is computed by 'tokenNameFromTxOutRef').

3. Bidding on the auction (off-chain implemented by 'txBid'). This transaction
   consumes an UTxO at the 'auctionValidator' with either 'NoBids' or 'Bidding'
   datum, and returns an UTxO with strictly greater value to the validator (in
   particular, it has to return the thread NFT) with a 'Bidding' datum recording
   the new highest bid and bidder. If there was a previous bidder, they are paid
   back their bid.

4. Hammer to end the auction (off-chain implemented by 'txHammer'). This
   transaction consumes an UTxO at the 'auctionValidator', and pays the Ada
   amount corresponding to the highest bid to the seller, and the value
   originally offered to the highest bidder. If there were no bids, the offer is
   returned to the seller.

Further details of these transactions are explained at the relevant places in
the code.

Remark on the design of the first two transactions
--------------------------------------------------

The following discussion is rather technical and not specific to this
contract. Rather, it describes a general design problem for smart contracts on
Cardano, so feel free to skip this if you merely want to get to know the
contract.

On a previous version of this contract, there was only one transaction to make
the offer and set the deadline. That version of the contract had the following
problem, which we think is fundamentally unsolvable with only one transaction: If
we want to use only one transaction to mint some tokens with a policy P and make
sure that they end up at the correct validator V,

- the minting policy P has to know the address of V, which is the hash of V's
  (compiled and normalised) source code. In particular, there is no way to
  compute this address on-chain, which means that this can only be accomplished
  by parameterising the P the address of V.

- Conversely, the validator V needs to know the 'CurrencySymbol' of the thread
  token, which is the hash of the (compiled and normalised) code of P.

So, each of the two scripts P and V has to have the other's hash as a parameter,
and have it known at compile time. This is is patently an impossible cycle.

The only generic solution that we know of is to turn any initial payment of
freshly minted tokens to the validator into a two-transaction process: The first
transaction does not involve any checks at all, does not mint any tokens that
should be locked in the validator script, and creates "unchecked" UTxOs (Here,
these are the UTxOs with the 'Offer' datum). The second transaction consumes
unchecked UTxOs (with an additional redeemer, here, that is 'SetDeadline'),
mints the required tokens, and pays a checked UTxO back to the same validator,
which contains the newly minted tokens as a proof of their soundness, and a
datum signalling that they have been checked (here, that datum is
'NoBids'). Since the second transaction uses a redeemer, it can make whatever
checks are needed to ensure the tokens are minted correctly and paid to the
correct script.

This solves the issue because the validator knows its own address and can thus
ensure that the minted tokens are given to itself and cannot be redirected to
any other address by an attacker.

-}

-- * Data types

-- | Parameters for the validator. Currently, the only information it is
-- parameterised by is the currency symbol of the thread token, and that's only
-- a trick to get that currency symbol into the validator, because it can not be
-- computed on-chain. It is constant, and if you look at the very bottom of this
-- file, you will find 'auctionValidator' defined with the constant currency
-- symbol derived from the 'threadTokenPolicy'.
newtype ValParams = ValParams Pl.CurrencySymbol

PlutusTx.makeLift ''ValParams
PlutusTx.unstableMakeIsData ''ValParams

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

getSeller :: AuctionState -> Pl.PubKeyHash
getSeller (Offer s _) = s
getSeller (NoBids s _ _) = s
getSeller (Bidding s _ _) = s

getBidDeadline :: AuctionState -> Maybe Pl.POSIXTime
getBidDeadline (Offer _ _) = Nothing
getBidDeadline (NoBids _ _ t) = Just t
getBidDeadline (Bidding _ t _) = Just t

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
-- transaction that sets the deadline onwards, this NFT will belong to the
-- 'auctionValidator'; its presence proves the authenticity of the
-- auction. Here, we only check that exactly one thread token is minted,
-- enforcing that the appropriate offer utxo, whose hash as computed by
-- 'tokenNameFromTxOutRef' must be the token name of the minted token, is
-- consumed. The rest of the necessary checks are performed by
-- 'validSetDeadline'.
--
-- The final 'Hammer' transaction of the auction burns the thread token. This
-- transaction is checked by 'validHammer', so that this minting policy only has
-- to check that at exactly one token is burned.
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
    -- 'offerOref'.
    --
    -- ############################################################
    -- # This introduces a KNOWN VULNERABILITY into the contract: Since we do not
    -- # check that no tokens of other token names are minted, it'll be possible
    -- # to forge the thread NFT of one auction while seting the deadline of
    -- # another auction, for example. See the "known vulnerabilities and
    -- # exploits" section in "tests/AuctionSpec.hs" for a worked-out exploit.
    -- ############################################################
    --
    amnt :: Integer
    amnt =
      foldr
        ( \(cs, tn, a) n ->
            if cs == Pl.ownCurrencySymbol ctx && tn == tokenNameFromTxOutRef offerOref
              then n + a
              else n
        )
        0
        $ Value.flattenValue (Pl.txInfoMint txi)

threadTokenPolicy :: Scripts.MintingPolicy
threadTokenPolicy =
  Pl.mkMintingPolicyScript
    $$(PlutusTx.compile [||Scripts.mkUntypedMintingPolicy mkPolicy||])

-- | Compute the thread token of the auction with the given offer UTxO.
threadToken :: Pl.TxOutRef -> Pl.Value
threadToken offerOref = Value.assetClassValue (threadTokenAssetClassFromOref offerOref) 1

threadTokenAssetClassFromOref :: Pl.TxOutRef -> Value.AssetClass
threadTokenAssetClassFromOref offerOref =
  Value.assetClass
    threadCurrencySymbol
    (tokenNameFromTxOutRef offerOref)

threadCurrencySymbol :: Pl.CurrencySymbol
threadCurrencySymbol = Pl.scriptCurrencySymbol threadTokenPolicy

-- | Compute the token name of the thread token of an auction from its offer Utxo.
{-# INLINEABLE tokenNameFromTxOutRef #-}
tokenNameFromTxOutRef :: Pl.TxOutRef -> Pl.TokenName
tokenNameFromTxOutRef (Pl.TxOutRef (Pl.TxId tid) i) =
  -- Remark, because I spent quite some time digging for this information: Why
  -- do we directly use the constructor 'TokenName' here? -- The point is that
  -- we want to use this function on-chain, and that the library function
  -- 'tokenName' (note the lower-case initial letter!) expects *Haskell* byte
  -- strings. See this issue on plutus-apps for some background:
  --
  -- https://github.com/input-output-hk/plutus-apps/issues/498
  Value.TokenName $ appendByteString tid $ appendByteString "-" $ encodeInteger i
  where
    -- we know that the numbers (indices of transaction outputs) we're working
    -- with here are non-negative.
    encodeInteger :: Integer -> BuiltinByteString
    encodeInteger n
      | n `quotient` 10 == 0 = encodeDigit n
      | otherwise = encodeInteger (n `quotient` 10) <> encodeDigit (n `remainder` 10)
      where
        encodeDigit :: Integer -> BuiltinByteString
        -- 48 is the ASCII code for '0'
        encodeDigit d = consByteString (d + 48) emptyByteString

-- | Compute the thread token of an auction from the currency symbol and the
-- original offer UTxO. This is for on-chain computations of the thread token,
-- where the currency symbol is known as a parameter.
{-# INLINEABLE threadTokenOnChain #-}
threadTokenOnChain :: Pl.CurrencySymbol -> Pl.TxOutRef -> Pl.Value
threadTokenOnChain threadCS offerOref = Value.assetClassValue (Value.AssetClass (threadCS, tokenNameFromTxOutRef offerOref)) 1

-- * The validator and its helpers

-- | Extract an auction state from an output (if it has one)
{-# INLINEABLE outputAuctionState #-}
outputAuctionState :: Pl.TxInfo -> Pl.TxOut -> Maybe AuctionState
outputAuctionState txi o = do
  h <- Pl.txOutDatum o
  Pl.Datum d <- Pl.findDatum h txi
  PlutusTx.fromBuiltinData d

-- | Test that the value paid to the public key address is at least the
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
validSetDeadline :: Pl.CurrencySymbol -> AuctionState -> Pl.ScriptContext -> Bool
validSetDeadline threadCS datum ctx =
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
                            `Value.geq` (threadTokenOnChain threadCS offerOref <> Pl.txOutValue offerOut)
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
-- * it is greater than the last bid (or at least the minimum bid, if it's the first one)
-- * after the transaction:
--    * the state of the auction is 'Bidding' with the new bid and bidder
--    * the validator locks the lot, the new bid, and the thread token with that datum
--    * the last bidder has gotten their money back from the validator
{-# INLINEABLE validBid #-}
validBid :: AuctionState -> Integer -> Pl.PubKeyHash -> Pl.ScriptContext -> Bool
validBid datum bid bidder ctx =
  let txi = Pl.scriptContextTxInfo ctx
      Just (Pl.TxInInfo _ Pl.TxOut {Pl.txOutValue = originalLockedValue}) = Pl.findOwnInput ctx
      checkDeadlineAndSignature deadline =
        traceIfFalse
          "Bidding past the deadline is not permitted"
          (Pl.to deadline `Interval.contains` Pl.txInfoValidRange txi)
          && traceIfFalse "Bid transaction not signed by bidder" (txi `Pl.txSignedBy` bidder)
      checkLocked seller deadline v =
        traceIfFalse
          "Validator does not lock lot, bid, and thread token with the correct 'Bidding' datum"
          ( any
              ( \o ->
                  outputAuctionState txi o == Just (Bidding seller deadline (BidderInfo bid bidder))
                    && Pl.txOutValue o `Value.geq` v
              )
              (Pl.getContinuingOutputs ctx)
          )
   in case datum of
        Offer {} -> trace "Cannot bid on an auction that hasn't yet got a deadline" False
        NoBids seller minBid deadline ->
          checkDeadlineAndSignature deadline
            && traceIfFalse "Cannot bid less than the minimum bid" (minBid <= bid)
            && checkLocked seller deadline (originalLockedValue <> Ada.lovelaceValueOf bid)
        Bidding seller deadline (BidderInfo prevBid prevBidder) ->
          checkDeadlineAndSignature deadline
            && traceIfFalse "Must bid strictly more than the previous bid" (prevBid < bid)
            && checkLocked
              seller
              deadline
              ( originalLockedValue
                  <> Pl.negate (Ada.lovelaceValueOf prevBid)
                  <> Ada.lovelaceValueOf bid
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
validHammer :: Pl.CurrencySymbol -> AuctionState -> Pl.TxOutRef -> Pl.ScriptContext -> Bool
validHammer threadCS datum offerOref ctx =
  let txi = Pl.scriptContextTxInfo ctx
      receives = receivesFrom txi
      theNFT = threadTokenOnChain threadCS offerOref
      Just (Pl.TxInInfo _ Pl.TxOut {Pl.txOutValue = lockedValue}) = Pl.findOwnInput ctx
      threadTokenIsBurned = Pl.txInfoMint txi == Pl.negate theNFT
      checkDeadlineAndBurn deadline =
        traceIfFalse
          "Hammer before the deadline is not permitted"
          (Pl.from deadline `Interval.contains` Pl.txInfoValidRange txi)
          && traceIfFalse
            "Hammer does not burn exactly one thread token"
            threadTokenIsBurned
   in case datum of
        Offer seller _minbid ->
          traceIfFalse "Seller must sign the hammer to withdraw the offer" (txi `Pl.txSignedBy` seller)
            && traceIfFalse "Seller must get the offer back" (seller `receives` lockedValue)
        NoBids seller _minbid deadline ->
          checkDeadlineAndBurn deadline
            && traceIfFalse
              "Seller must get the offer back"
              (seller `receives` (lockedValue <> Pl.negate theNFT))
        Bidding seller deadline (BidderInfo lastBid lastBidder) ->
          checkDeadlineAndBurn deadline
            && traceIfFalse
              "last bidder must get the lot"
              ( lastBidder
                  `receives` ( lockedValue <> Pl.negate theNFT
                                 <> Pl.negate (Ada.lovelaceValueOf lastBid)
                             )
              )
            && traceIfFalse
              "Seller must get the last bid"
              (seller `receives` Ada.lovelaceValueOf lastBid)

{-# INLINEABLE validate #-}
validate :: ValParams -> AuctionState -> Action -> Pl.ScriptContext -> Bool
validate (ValParams threadCS) datum redeemer ctx = case redeemer of
  SetDeadline -> validSetDeadline threadCS datum ctx
  Bid (BidderInfo bid bidder) -> validBid datum bid bidder ctx
  Hammer offerOref -> validHammer threadCS datum offerOref ctx

-- Plutus boilerplate to compile the validator

data Auction

instance Scripts.ValidatorTypes Auction where
  type RedeemerType Auction = Action
  type DatumType Auction = AuctionState

auctionValidator' :: ValParams -> Scripts.TypedValidator Auction
auctionValidator' =
  Scripts.mkTypedValidatorParam @Auction
    $$(PlutusTx.compile [||validate||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator @AuctionState @Action

auctionValidator :: Scripts.TypedValidator Auction
auctionValidator = auctionValidator' $ ValParams $ Pl.scriptCurrencySymbol threadTokenPolicy
