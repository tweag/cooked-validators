module Auction.Offchain where

import qualified Auction as A
import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
import qualified Ledger as L
import Ledger.Ada as Ada
import Ledger.Typed.Scripts as Pl
import qualified PlutusTx.Numeric as Pl

txOpen ::
  (MonadBlockChain m) =>
  A.Parameters' () ->
  m A.Parameters
txOpen p = do
  seller <- ownPaymentPubKeyHash
  let p' :: A.Parameters
      p' =
        A.Parameters'
          { A.seller = seller,
            A.lot = A.lot p,
            A.minBid = A.minBid p,
            A.bidDeadline = A.bidDeadline p
          }
  _ <-
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) [PaysScript (A.auctionValidator p') A.NoBids (A.lot p)]
  return p'

previousBidder :: A.AuctionState -> Maybe (Integer, L.PubKeyHash)
previousBidder (A.Bidding (A.BidderInfo bid bidder)) = Just (bid, bidder)
previousBidder _ = Nothing

txBid ::
  (MonadBlockChain m) =>
  A.Parameters ->
  Integer ->
  m ()
txBid p bid = do
  bidder <- ownPaymentPubKeyHash
  [utxo] <- scriptUtxosSuchThat (A.auctionValidator p) (\_ _ -> True)
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        [ -- no need to ask for the bidder to sign the transaction, that's automatic
          Before (A.bidDeadline p),
          SpendsScript
            (A.auctionValidator p)
            (A.Bid (A.BidderInfo bid bidder))
            utxo
        ]
          :=>: ( [ PaysScript (A.auctionValidator p) (A.Bidding (A.BidderInfo bid bidder)) $
                     A.lot p <> Ada.lovelaceValueOf bid
                 ]
                   <> case previousBidder (snd utxo) of
                     Nothing -> []
                     Just (prevBid, prevBidder) ->
                       [paysPK prevBidder (Ada.lovelaceValueOf prevBid)]
               )

txHammer ::
  (MonadBlockChain m) =>
  A.Parameters ->
  m ()
txHammer p = do
  [utxo] <- scriptUtxosSuchThat (A.auctionValidator p) (\_ _ -> True)
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        [ After (A.bidDeadline p),
          SpendsScript
            (A.auctionValidator p)
            A.Hammer
            utxo
        ]
          :=>: case previousBidder (snd utxo) of
            Nothing ->
              [paysPK (A.seller p) (A.lot p)]
            Just (lastBid, lastBidder) ->
              [ paysPK lastBidder (A.lot p),
                paysPK (A.seller p) (Ada.lovelaceValueOf lastBid)
              ]

testParams :: L.POSIXTime -> A.Parameters' ()
testParams t =
  A.Parameters'
    { A.seller = (),
      A.lot = Ada.lovelaceValueOf 1235,
      A.minBid = 3,
      A.bidDeadline = t + 6000000
    }

test = runMockChain $ do
  t0 <- currentTime
  p <- txOpen (testParams t0)
  txBid p 3000000 `as` (wallet 3)
  return ()
