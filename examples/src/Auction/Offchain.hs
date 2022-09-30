module Auction.Offchain where

import qualified Auction as A
import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
import qualified Ledger as L
import Ledger.Ada as Ada
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value

-- | Make an offer. There are no checks with this transaction. Anyone is allowed
-- to pay the 'auctionValidator' with something they want to sell, using the
-- 'Offer' datum to specify the seller of the auction.
--
-- This transaction returns the 'SpendableOut' of the 'Offer' UTxO it creates.
txOffer :: MonadBlockChain m => L.Value -> m SpendableOut
txOffer lot = do
  seller <- ownPaymentPubKeyHash
  tx <-
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        PaysScript A.auctionValidator (A.Offer seller) lot
  let [(lotOut, lotOref)] = getCardanoTxOutRefs tx
  return (lotOref, fromJust $ fromTxOut lotOut)

-- | Start an auction by setting the bidding deadline. This transaction consumes
-- the provided 'Offer' Utxo and returns a 'NoBids' UTxO to the auction
-- validator. It also mints the thread NFT that ensures the authenticity of the
-- auction from that point on.
txSetDeadline :: MonadBlockChain m => SpendableOut -> Pl.POSIXTime -> m ()
txSetDeadline offerUtxo deadline = do
  seller <- ownPaymentPukKeyHash
  let lot = sOutValue offerUtxo
      theNft = threadToken $ fst offerUtxo
  _ <-
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        [ SpendsScript
            (A.auctionValidator vp)
            A.SetDeadline
            utxo,
          Mints (Just $ fst oref) [A.ThreadTokenPolicy] theNft
        ]
          :=>: [PaysScript A.auctionValidator (A.NoBids deadline) (lot <> theNft)]

previousBidder :: A.AuctionState -> Maybe (Integer, L.PubKeyHash)
previousBidder (A.Bidding _ _ (A.BidderInfo bid bidder)) = Just (bid, bidder)
previousBidder _ = Nothing

-- | Bid a certain amount of Lovelace on the auction with the given 'Offer'
-- UTxO. If there was a previous bidder, they will receive their money back.
txBid :: MonadBlockChain m => SpendableOut -> Integer -> m ()
txBid offerUtxo bid = do
  bidder <- ownPaymentPubKeyHash
  [utxo] <-
    scriptUtxosSuchThat
      A.auctionValidator
      (\x _ -> x `Value.geq` threadToken $ fst offerUtxo)
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        [ Before (A.bidDeadline p),
          SpendsScript
            (A.auctionValidator p)
            (A.Bid (A.BidderInfo bid bidder))
            utxo
        ]
          :=>: ( PaysScript
                   (A.auctionValidator p)
                   (A.Bidding (A.BidderInfo bid bidder))
                   (sOutValue utxo <> Ada.lovelaceValueOf bid) :
                 case previousBidder (snd utxo) of
                   Nothing -> []
                   Just (prevBid, prevBidder) ->
                     [paysPK prevBidder (Ada.lovelaceValueOf prevBid)]
               )

-- | Close the auction with the given 'Offer' UTxO. If there were any bids, this
-- will pay the lot to the last bidder and the last bid to the
-- seller. Otherwise, the seller will receive the lot back. This transaction
-- also burns the thread token.
txHammer :: MonadBlockChain m => SpendableOut -> m ()
txHammer offerUtxo = do
  [utxo] <-
    scriptUtxosSuchThat
      A.auctionValidator
      (\x _ -> x `Value.geq` threadToken $ fst offerUtxo)
  let Just deadline = A.bidDeadline $ snd utxo
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        [ After deadline,
          SpendsScript
            (A.auctionValidator p)
            A.Hammer
            utxo,
          Mints
            (Just $ fst offerUtxo)
            [A.threadTokenPolicy]
            (Pl.negate $ threadToken offerUtxo)
        ]
          :=>: case previousBidder (snd utxo) of
            Nothing ->
              let lot = sOutValue utxo <> Pl.negate (threadToken offerUtxo)
               in [paysPK (A.seller $ snd utxo) lot]
            Just (lastBid, lastBidder) ->
              let lot =
                    sOutValue utxo
                      <> Pl.negate (Ada.lovelaceValueOf lastBid)
                      <> Pl.negate (threadToken offerUtxo)
               in [ paysPK lastBidder lot,
                    paysPK (A.seller $ snd utxo) (Ada.lovelaceValueOf lastBid)
                  ]
