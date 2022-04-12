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

-- | Helper function to perform a case analysis on the state of the
-- script:
-- * If it owns no UTxO, the auction has not yet been opened. Then,
--   perform the transaction described by @f0@
-- * If it owns exactly one UTxO and that has a datum of 'A.NoBids',
--   the auction has already been opened, but there have not yet been
--   any bids. In that case, perform the transaction described by
--   @f1@, which has access to
--     * the 'SpendableOut' that describes the locked lot, together
--       with the lot and address of the seller
-- * If it owns the UTxO from the last case and an UTxO that has a
--   datum of 'A.Bidding _ _', the auction has already had at least
--   one bid. In that case, perform the transaction described by @f2@,
--   which has access to
--     * the 'SpendableOut' that describes the locked lot, together
--       with the address of the seller
--     * the 'SpenableOut' describing the last bid, together with the
--       last bid in Ada, and the address of the last bidder
-- * In every other case, the script is in an invalid state, so it's
--   best to do nothing.
txCaseAnalyse ::
  (MonadBlockChain m) =>
  Scripts.TypedValidator A.Auction ->
  m a ->
  ( (SpendableOut, L.Value, L.PubKeyHash) ->
    m b
  ) ->
  ( (SpendableOut, L.PubKeyHash) ->
    (SpendableOut, Integer, L.PubKeyHash) ->
    m c
  ) ->
  m ()
txCaseAnalyse script f0 f1 f2 = do
  los <-
    scriptUtxosSuchThat
      script
      ( \d _ -> case d of
          A.NoBids _ -> True
          _ -> False
      )
  pos <-
    scriptUtxosSuchThat
      script
      ( \d _ -> case d of
          A.Bidding _ _ -> True
          _ -> False
      )
  case pos of
    [(po, A.Bidding (A.BidderInfo bid bidder) (A.SellerInfo seller lot))] ->
      void $ f2 (lo, lot, seller) (po, bid, bidder)
    [] ->
      case los of
        [(lo, A.NoBids (SellerInfo seller lot))] ->
          void $ f1 (lo, lot, seller)
        [] -> void f0
        _ -> return ()
    _ -> return ()

txOpen ::
  (MonadBlockChain m) =>
  Scripts.TypedValidator A.Auction ->
  Wallet ->
  L.Value ->
  m ()
txOpen script seller lot =
  txCaseAnalyse
    script
    ( validateTxConstrLbl
        "Opening the auction"
        ( [SignedBy [walletPKHash seller]]
            :=>: [PaysScript script (A.NoBids $ SellerInfo (walletPKHash seller) lot) lot]
        )
    )
    (\_ -> fail "You can not open an auction that is already open")
    (\_ _ -> fail "You can not open an auction that has already had bids")

txBid ::
  (MonadBlockChain m) =>
  Scripts.TypedValidator A.Auction ->
  Integer ->
  Wallet ->
  m ()
txBid script bid bidder =
  txCaseAnalyse
    script
    (fail "You can not bid on an auction that is not yet opened")
    ( \(lotOutput, lot, seller) ->
        validateTxConstrLbl
          ("First bid of " ++ show bid ++ " Ada from " ++ show bidder)
          ( [SignedBy [walletPKHash bidder]]
              :=>: [ PaysScript
                       script
                       ( A.Bidding
                           (BidderInfo bid (walletPKHash bidder))
                           (SellerInfo seller lot)
                       )
                       (Ada.lovelaceValueOf bid)
                   ]
          )
    )
    ( \_ (prevOutput, prevBid, prevBidder) ->
        validateTxConstrLbl
          ("Further bid of " ++ show bid ++ " Ada from " ++ show (walletPKHash bidder) ++ " returning " ++ show prevBid ++ " Ada to " ++ show prevBidder)
          ( [ SignedBy [walletPKHash bidder],
              SpendsScript
                script
                (A.Bid bid (walletPKHash bidder))
                (prevOutput, A.Bidding prevBid prevBidder)
            ]
              :=>: [ PaysScript
                       script
                       (A.Bidding bid (walletPKHash bidder))
                       (Ada.lovelaceValueOf bid),
                     paysPK prevBidder (Ada.lovelaceValueOf prevBid)
                   ]
          )
    )

txHammer ::
  (MonadBlockChain m) =>
  Scripts.TypedValidator A.Auction ->
  m ()
txHammer script =
  txCaseAnalyse
    script
    (return ())
    ( \(lotOutput, lot, seller) ->
        validateTxConstrLbl
          ("Hammer after no bids returning " ++ show lot ++ " to the seller " ++ show seller)
          ( [SpendsScript script A.Hammer (lotOutput, A.NoBids)]
              :=>: [paysPK seller lot]
          )
    )
    ( \(lotOutput, lot, seller) (buyerOutput, lastBid, buyer) ->
        validateTxConstrLbl
          ("Hammer giving " ++ show lot ++ " to the last bidder " ++ show buyer ++ " and paying " ++ show lastBid ++ " Ada to the seller " ++ show seller)
          ( [ SpendsScript script A.Hammer (buyerOutput, A.Bidding (BuyerInfo lastBid buyer) (SellerInfo lot seller)),
              SpendsScript script A.Hammer (lotOutput, A.NoBids (SellerInfo lot seller))
            ]
              :=>: [ paysPK buyer lot,
                     paysPK seller (Ada.lovelaceValueOf lastBid)
                   ]
          )
    )
