module Auction.Offchain where

import qualified Auction as A
import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
import qualified Ledger as L
import Ledger.Ada as Ada
import Ledger.Typed.Scripts as Scripts

-- * Basic transactions (intended to be valid if executed in the correct order)

txOpen ::
  (MonadBlockChain m) =>
  Scripts.TypedValidator A.Auction ->
  Wallet ->
  L.Value ->
  A.AuctionState ->
  m ()
txOpen script seller lot datum =
  void $
    validateTxConstrLbl
      "Opening the auction"
      ( [SignedBy [walletPKHash seller]]
          :=>: [PaysScript script datum lot]
      )

txFirstBid ::
  (MonadBlockChain m) =>
  Scripts.TypedValidator A.Auction ->
  Wallet ->
  Integer ->
  A.AuctionState ->
  m ()
txFirstBid script bidder bid datum =
  void $
    validateTxSkel $
      TxSkel
        { txLabel = Just $ "First bid of " ++ show bid ++ " Ada from " ++ show bidder,
          txOpts = def,
          txConstraints =
            [SignedBy [walletPKHash bidder]]
              :=>: [PaysScript script datum (Ada.lovelaceValueOf bid)]
        }

txFurtherBid ::
  (MonadBlockChain m) =>
  Scripts.TypedValidator A.Auction ->
  Wallet ->
  Integer ->
  Wallet ->
  Integer ->
  A.AuctionState ->
  m ()
txFurtherBid script bidder bid lastBidder lastBid datum = do
  [(lastBidderOutput, lastBidderDatum)] <-
    scriptUtxosSuchThat
      script
      ( \d _ -> case d of
          A.Bidding _ lastBidder' -> lastBidder' == walletPKHash lastBidder
          _ -> False
      )
  void $
    validateTxConstrLbl
      ("Further bid of " ++ show bid ++ " Ada from " ++ show bidder ++ " returning " ++ show lastBid ++ " Ada to " ++ show lastBidder)
      ( [ SignedBy [walletPKHash bidder],
          SpendsScript script (A.Bid bid (walletPKHash bidder)) (lastBidderOutput, lastBidderDatum)
        ]
          :=>: [ PaysScript script datum (Ada.lovelaceValueOf bid),
                 paysPK (walletPKHash lastBidder) (Ada.lovelaceValueOf lastBid)
               ]
      )

txHammerAfterBids ::
  (MonadBlockChain m) =>
  Scripts.TypedValidator A.Auction ->
  Wallet ->
  Wallet ->
  L.Value ->
  Integer ->
  m ()
txHammerAfterBids script buyer seller lot lastBid = do
  [(buyerOutput, buyerDatum)] <-
    scriptUtxosSuchThat
      script
      ( \d _ -> case d of
          A.Bidding _ lastBidder -> lastBidder == walletPKHash buyer
          _ -> False
      )
  os <- scriptUtxosSuchThat script (\_ _ -> True)
  let (sellerOutput, sellerDatum) =
        case filter (\o -> sOutValue (fst o) == Ada.lovelaceValueOf lastBid) os of
          [o] -> o
  void $
    validateTxConstrLbl
      ("Hammer giving " ++ show lot ++ " to the last bidder " ++ show buyer ++ " and paying " ++ show lastBid ++ " Ada to the seller " ++ show seller)
      ( [ SpendsScript script A.Hammer (buyerOutput, buyerDatum),
          SpendsScript script A.Hammer (sellerOutput, sellerDatum)
        ]
          :=>: [ paysPK (walletPKHash buyer) lot,
                 paysPK (walletPKHash seller) (Ada.lovelaceValueOf lastBid)
               ]
      )

txHammerNoBids ::
  (MonadBlockChain m) =>
  Scripts.TypedValidator A.Auction ->
  Wallet ->
  L.Value ->
  m ()
txHammerNoBids script seller lot = do
  os <- scriptUtxosSuchThat script (\_ _ -> True)
  let (sellerOutput, sellerDatum) =
        case filter (\o -> sOutValue (fst o) == lot) os of
          [o] -> o
  void $
    validateTxConstrLbl
      ("Hammer after no bids returning " ++ show lot ++ " to the seller " ++ show seller)
      ( [SpendsScript script A.Hammer (sellerOutput, sellerDatum)]
          :=>: [paysPK (walletPKHash seller) lot]
      )
