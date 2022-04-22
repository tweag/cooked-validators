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

txOpen ::
  (MonadBlockChain m) =>
  A.StaticValParams ->
  m (A.ValParams, A.PolicyParams)
txOpen p = do
  seller <- ownPaymentPubKeyHash
  utxo : _ <- pkUtxosSuchThatValue seller (\v -> v `Value.geq` A.lot' p)
  let p' :: A.ValParams
      p' =
        A.ValParams
          { A.staticValParams = p,
            A.seller = seller,
            A.threadTokenAssetClass = A.threadTokenAssetClassFromOrefAndLot (fst utxo) (A.lot' p)
          }

      q :: A.PolicyParams
      q =
        A.PolicyParams
          { A.pThreadTokenName = A.threadTokenName,
            A.pLotOutRef = fst utxo,
            A.pLot = A.lot' p
          }

      token :: L.Value
      token = Value.assetClassValue (A.threadTokenAssetClass p') 1

  _ <-
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        [ Mints
            (Just (Scripts.validatorAddress (A.auctionValidator p')))
            [A.threadTokenPolicy q]
            token,
          SpendsPK utxo
        ]
          :=>: [ PaysScript (A.auctionValidator p') A.NoBids (A.lot p' <> token)
               ]
  return (p', q)

previousBidder :: A.AuctionState -> Maybe (Integer, L.PubKeyHash)
previousBidder (A.Bidding (A.BidderInfo bid bidder)) = Just (bid, bidder)
previousBidder _ = Nothing

txBid ::
  (MonadBlockChain m) =>
  A.ValParams ->
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
                     A.lot p <> Ada.lovelaceValueOf bid <> Value.assetClassValue (A.threadTokenAssetClass p) 1
                 ]
                   <> case previousBidder (snd utxo) of
                     Nothing -> []
                     Just (prevBid, prevBidder) ->
                       [paysPK prevBidder (Ada.lovelaceValueOf prevBid)]
               )

txHammer ::
  (MonadBlockChain m) =>
  A.ValParams ->
  A.PolicyParams ->
  m ()
txHammer p q = do
  [utxo] <- scriptUtxosSuchThat (A.auctionValidator p) (\_ _ -> True)
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        [ After (A.bidDeadline p),
          SpendsScript
            (A.auctionValidator p)
            A.Hammer
            utxo,
          Mints
            (Just (Scripts.validatorAddress (A.auctionValidator p)))
            [A.threadTokenPolicy q]
            (Value.assetClassValue (A.threadTokenAssetClass p) (-1))
        ]
          :=>: case previousBidder (snd utxo) of
            Nothing ->
              [paysPK (A.seller p) (A.lot p)]
            Just (lastBid, lastBidder) ->
              [ paysPK lastBidder (A.lot p),
                paysPK (A.seller p) (Ada.lovelaceValueOf lastBid)
              ]
