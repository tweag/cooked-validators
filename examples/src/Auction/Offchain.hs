{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Auction.Offchain where

import qualified Auction as A
import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
import Data.Maybe
import qualified Ledger as L
import Ledger.Ada as Ada
import qualified Ledger.Value as Value
import qualified PlutusTx.Numeric as Pl

-- | Make an offer. There are no checks with this transaction. Anyone is allowed
-- to pay the 'auctionValidator' with something they want to sell, using the
-- 'Offer' datum to specify the seller of the auction.
--
-- This transaction returns the 'SpendableOut' of the 'Offer' UTxO it creates.
txOffer :: MonadBlockChain m => L.Value -> Integer -> m SpendableOut
txOffer lot minBid = do
  seller <- ownPaymentPubKeyHash
  tx <-
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        paysScript A.auctionValidator (A.Offer seller minBid) lot
  outputs <- spOutsFromCardanoTx tx
  -- the transaction created exactly one script output, so the call to head never fail
  return $ head $ filter (isJust . sBelongsToScript) outputs

-- | Start an auction by setting the bidding deadline. This transaction consumes
-- the provided 'Offer' Utxo and returns a 'NoBids' UTxO to the auction
-- validator. It also mints the thread NFT that ensures the authenticity of the
-- auction from that point on.
txSetDeadline :: MonadBlockChain m => SpendableOut -> L.POSIXTime -> m ()
txSetDeadline offerUtxo deadline = do
  let lot = sOutValue offerUtxo
      theNft = A.threadToken $ fst offerUtxo
      Just (A.Offer seller minBid) = spOutGetDatum @A.Auction offerUtxo
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        [ SpendsScript
            A.auctionValidator
            A.SetDeadline
            offerUtxo,
          Mints (Just $ fst offerUtxo) [A.threadTokenPolicy] theNft,
          SignedBy [seller]
        ]
          :=>: [paysScript A.auctionValidator (A.NoBids seller minBid deadline) (lot <> theNft)]

previousBidder :: A.AuctionState -> Maybe (Integer, L.PubKeyHash)
previousBidder (A.Bidding _ _ (A.BidderInfo bid bidder)) = Just (bid, bidder)
previousBidder _ = Nothing

-- | Bid a certain amount of Lovelace on the auction with the given 'Offer'
-- UTxO. If there was a previous bidder, they will receive their money back.
txBid :: MonadBlockChain m => SpendableOut -> Integer -> m ()
txBid offerUtxo bid =
  let theNft = A.threadToken $ fst offerUtxo
   in do
        bidder <- ownPaymentPubKeyHash
        [(utxo, datum)] <-
          scriptUtxosSuchThat
            A.auctionValidator
            (\_ x -> x `Value.geq` theNft)
        -- The call to 'fromJust' can never fail. If there's already a thread token,
        -- we're at least in 'NoBids' state.
        let deadline = fromJust $ A.getBidDeadline datum
            seller = A.getSeller datum
        void $
          validateTxSkel $
            txSkelOpts (def {adjustUnbalTx = True}) $
              [ Before deadline,
                SpendsScript
                  A.auctionValidator
                  (A.Bid (A.BidderInfo bid bidder))
                  utxo
              ]
                :=>: ( paysScript
                         A.auctionValidator
                         (A.Bidding seller deadline (A.BidderInfo bid bidder))
                         (sOutValue utxo <> Ada.lovelaceValueOf bid) :
                       case previousBidder datum of
                         Nothing -> []
                         Just (prevBid, prevBidder) ->
                           [paysPK prevBidder (Ada.lovelaceValueOf prevBid)]
                     )

-- | Close the auction with the given 'Offer' UTxO. If there were any bids, this
-- will pay the lot to the last bidder and the last bid to the
-- seller. Otherwise, the seller will receive the lot back. This transaction
-- also burns the thread token.
txHammer :: MonadBlockChain m => SpendableOut -> m ()
txHammer offerUtxo =
  let theNft = A.threadToken $ fst offerUtxo
   in do
        utxos <-
          scriptUtxosSuchThat
            A.auctionValidator
            (\_ x -> x `Value.geq` theNft)
        void $
          validateTxSkel $
            txSkelOpts (def {adjustUnbalTx = True}) $
              case utxos of
                [] ->
                  -- There's no thread token, so the auction is still in 'Offer'
                  -- state
                  [SpendsScript A.auctionValidator (A.Hammer $ fst offerUtxo) offerUtxo]
                    :=>: [ paysPK
                             (A.getSeller $ fromJust $ spOutGetDatum @A.Auction offerUtxo)
                             (sOutValue offerUtxo)
                         ]
                (utxo, datum) : _ ->
                  -- There is a thread token, so the auction is in 'NoBids' or
                  -- 'Bidding' state, which means that the following pattern
                  -- match can not fail:
                  let Just deadline = A.getBidDeadline datum
                   in [ After deadline,
                        SpendsScript
                          A.auctionValidator
                          (A.Hammer $ fst offerUtxo)
                          utxo,
                        Mints
                          (Just $ fst offerUtxo)
                          [A.threadTokenPolicy]
                          (Pl.negate theNft)
                      ]
                        :=>: case previousBidder datum of
                          Nothing ->
                            let lot = sOutValue utxo <> Pl.negate theNft
                             in [paysPK (A.getSeller datum) lot]
                          Just (lastBid, lastBidder) ->
                            let lot =
                                  sOutValue utxo
                                    <> Pl.negate (Ada.lovelaceValueOf lastBid)
                                    <> Pl.negate theNft
                             in [ paysPK lastBidder lot,
                                  paysPK (A.getSeller datum) (Ada.lovelaceValueOf lastBid)
                                ]
