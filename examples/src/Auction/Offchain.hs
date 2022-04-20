module Auction.Offchain where

import qualified Auction as A
import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Data.Default
import qualified Ledger as L
import Ledger.Ada as Ada
import qualified Ledger.Value as Value
-- import PlutusTx.Numeric as Pl

txOpen ::
  (MonadBlockChain m) =>
  A.Parameters' () () ->
  m A.Parameters
txOpen p = do
  seller <- ownPaymentPubKeyHash
  utxo : _ <- pkUtxosSuchThatValue seller (\v -> v `Value.geq` A.lot p)
  let threadToken = A.threadTokenAssetClass (fst utxo) (A.lot p)
      p' :: A.Parameters
      p' =
        A.Parameters'
          { A.lot = A.lot p,
            A.minBid = A.minBid p,
            A.bidDeadline = A.bidDeadline p,
            A.seller = seller,
            A.threadToken = threadToken
          }

      token :: L.Value
      token = Value.assetClassValue threadToken 1

  _ <-
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        [ Mints
            (Just (Scripts.validatorAddress (A.auctionValidator p')))
            [A.threadTokenPolicy A.threadTokenName (fst utxo) (A.lot p')]
            token,
          SpendsPK utxo
        ]
          :=>: [ PaysScript (A.auctionValidator p') A.NoBids (A.lot p <> token)
                 -- paysPK seller (sOutValue utxo <> Pl.negate (A.lot p))
               ]
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
