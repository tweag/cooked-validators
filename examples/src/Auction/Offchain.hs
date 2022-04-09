module AuctionSpec where

import qualified Auction as A
import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
import qualified Ledger as L
import Ledger.Ada as Ada
import Ledger.Typed.Scripts as Scripts

-- * Basic transactions (intended to be valid if executed in the correct order)

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
          A.Bidding lastBid' lastBidder' -> lastBidder' == walletPKHash lastBidder
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

txHammer ::
  (MonadBlockChain m) =>
  Scripts.TypedValidator A.Auction ->
  Wallet ->
  Wallet ->
  L.Value ->
  Integer ->
  A.AuctionState ->
  m ()
txHammer script buyer seller lot lastBid datum = do
  [(buyerOutput, buyerDatum)] <-
    scriptUtxosSuchThat
      script
      ( \d _ -> case d of
          A.Bidding _ lastBidder -> lastBidder == walletPKHash buyer
          _ -> False
      )
  void $
    validateTxConstrLbl
      ("Hammer giving " ++ show lot ++ " to the last bidder " ++ show buyer ++ " and paying " ++ show lastBid ++ " Ada to the seller " ++ show seller)
      ( [ SignedBy [walletPKHash seller],
          SpendsScript script A.Hammer (buyerOutput, buyerDatum)
        ]
          :=>: [ paysPK (walletPKHash buyer) lot,
                 paysPK (walletPKHash seller) (Ada.lovelaceValueOf lastBid)
               ]
      )

txMoneyBack ::
  (MonadBlockChain m) =>
  Scripts.TypedValidator A.Auction ->
  Wallet ->
  Integer ->
  A.AuctionState ->
  m ()
txMoneyBack script bidder bid datum = do
  [(bidderOutput, bidderDatum)] <-
    scriptUtxosSuchThat
      script
      ( \d _ -> case d of
          A.Bidding _ lastBidder -> lastBidder == walletPKHash bidder
          _ -> False
      )
  void $
    validateTxConstrLbl
      "Money back"
      ( [SpendsScript script A.MoneyBack (bidderOutput, bidderDatum)]
          :=>: [paysPK (walletPKHash bidder) (Ada.lovelaceValueOf bid)]
      )
