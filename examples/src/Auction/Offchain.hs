{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Auction.Offchain where

import qualified Auction as A
import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints.Type
import Data.Default
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Ledger as L
import qualified Ledger.Ada as Ada
import qualified Ledger.Interval as Interval
import qualified Ledger.Tx as Pl
import qualified Ledger.Value as Value
import Optics.Core
import qualified PlutusTx.Numeric as Pl
import Test.QuickCheck.Modifiers (NonZero (..))

-- | Make an offer. There are no checks with this transaction. Anyone is allowed
-- to pay the 'auctionValidator' with something they want to sell, using the
-- 'Offer' datum to specify the seller of the auction.
--
-- This transaction returns the 'SpendableOut' of the 'Offer' UTxO it creates.
txOffer :: MonadBlockChain m => L.Value -> Integer -> m SpendableOut
txOffer lot minBid = do
  -- oldUtxos <- scriptUtxosSuchThat A.auctionValidator (\_ _ -> True)
  seller <- ownPaymentPubKeyHash
  tx <-
    validateTxSkel' $
      mempty
        { txSkelOpts = def {adjustUnbalTx = True},
          txSkelOuts = [paysScript A.auctionValidator (Right $ A.Offer seller minBid) lot]
        }
  outputs <- spOutsFromCardanoTx tx
  -- the transaction created exactly one script output, so the call to head never fail
  -- newUtxo : _ <- scriptUtxosSuchThat A.auctionValidator (\d x -> d Pl.== A.Offer seller minBid && x `Value.geq` lot)
  -- return $ -- Debug.trace (show tx ++ "\n\n" ++ show (Pl.getCardanoTxOutRefs tx) ++ "\n\n" ++ show (Pl.insert tx mempty)) $
  --   fst newUtxo

  -- uncomment below for something that I would expect to be equivalent, but which isn't:
  return $
    -- Debug.trace (show tx) $
    head $ filter (isJust . sBelongsToScript) outputs

-- | Start an auction by setting the bidding deadline. This transaction consumes
-- the provided 'Offer' Utxo and returns a 'NoBids' UTxO to the auction
-- validator. It also mints the thread NFT that ensures the authenticity of the
-- auction from that point on.
txSetDeadline :: MonadBlockChain m => SpendableOut -> L.POSIXTime -> m Pl.CardanoTx
txSetDeadline offerUtxo deadline = do
  let lot = sOutValue offerUtxo
      offerOref = sOutTxOutRef offerUtxo
      theNft = A.threadToken offerOref
  (A.Offer seller minBid) <- spOutGetDatum @A.Auction offerUtxo
  validateTxSkel' $
    mempty
      { txSkelOpts =
          def
            { adjustUnbalTx = True -- ,
            -- unsafeModTx =
            --   [ RawModTxBeforeBalancing (\tx -> Debug.trace (show tx) tx),
            --     RawModTxAfterBalancing (\tx -> Debug.trace (show tx) tx)
            --   ]
            },
        txSkelMints =
          txSkelMintsFromList
            [ ( Pl.Versioned A.threadTokenPolicy Pl.PlutusV2,
                SomeMintsRedeemer offerOref,
                A.tokenNameFromTxOutRef offerOref,
                NonZero 1
              )
            ],
        txSkelIns = Map.singleton offerUtxo $ SpendsScript A.auctionValidator A.SetDeadline,
        txSkelRequiredSigners = Set.singleton seller,
        txSkelOuts =
          [ paysScript
              A.auctionValidator
              (Right $ A.NoBids seller minBid deadline)
              (lot <> theNft)
          ]
      }

previousBidder :: A.AuctionState -> Maybe (Integer, L.PubKeyHash)
previousBidder (A.Bidding _ _ (A.BidderInfo bid bidder)) = Just (bid, bidder)
previousBidder _ = Nothing

-- | Bid a certain amount of Lovelace on the auction with the given 'Offer'
-- UTxO. If there was a previous bidder, they will receive their money back.
txBid :: MonadBlockChain m => SpendableOut -> Integer -> m L.CardanoTx
txBid offerUtxo bid =
  let theNft = A.threadToken $ sOutTxOutRef offerUtxo
   in do
        bidder <- ownPaymentPubKeyHash
        [(utxo, Right datum)] <-
          scriptUtxosSuchThat
            A.auctionValidator
            (\_ x -> x `Value.geq` theNft)
        -- The call to 'fromJust' can never fail. If there's already a thread token,
        -- we're at least in 'NoBids' state.
        let deadline = fromJust $ A.getBidDeadline datum
            seller = A.getSeller datum
        validateTxSkel' $
          mempty
            { txSkelOpts = def {adjustUnbalTx = True},
              txSkelIns =
                Map.singleton utxo $
                  SpendsScript
                    A.auctionValidator
                    (A.Bid (A.BidderInfo bid bidder)),
              txSkelOuts =
                paysScript
                  A.auctionValidator
                  (Right $ A.Bidding seller deadline (A.BidderInfo bid bidder))
                  (sOutValue utxo <> Ada.lovelaceValueOf bid) :
                case previousBidder datum of
                  Nothing -> []
                  Just (prevBid, prevBidder) ->
                    [paysPK prevBidder (Ada.lovelaceValueOf prevBid)],
              txSkelValidityRange = Interval.to (deadline - 1),
              txSkelRequiredSigners = Set.singleton bidder
            }

-- | Close the auction with the given 'Offer' UTxO. If there were any bids, this
-- will pay the lot to the last bidder and the last bid to the
-- seller. Otherwise, the seller will receive the lot back. This transaction
-- also burns the thread token.
txHammer :: MonadBlockChain m => SpendableOut -> m ()
txHammer offerUtxo =
  let offerOref = sOutTxOutRef offerUtxo
      theNft = A.threadToken offerOref
   in do
        utxos <-
          scriptUtxosSuchThat
            A.auctionValidator
            (\_ x -> x `Value.geq` theNft)
        (A.Offer seller _minBid) <- spOutGetDatum @A.Auction offerUtxo
        void $
          validateTxSkel' $
            mempty
              { txSkelOpts = def {adjustUnbalTx = True}
              }
              <> case utxos of
                [] ->
                  -- There's no thread token, so the auction is still in 'Offer'
                  -- state
                  mempty
                    { txSkelIns =
                        Map.singleton offerUtxo $
                          SpendsScript
                            A.auctionValidator
                            (A.Hammer offerOref),
                      txSkelOuts =
                        [ paysPK
                            seller
                            (sOutValue offerUtxo)
                        ]
                    }
                (utxo, Right datum) : _ ->
                  -- There is a thread token, so the auction is in 'NoBids' or
                  -- 'Bidding' state, which means that the following pattern
                  -- match can not fail:
                  let Just deadline = A.getBidDeadline datum
                   in mempty
                        { txSkelValidityRange = Interval.from deadline,
                          txSkelIns =
                            Map.singleton utxo $
                              SpendsScript
                                A.auctionValidator
                                (A.Hammer offerOref),
                          txSkelMints =
                            review
                              mintsListIso
                              [ ( Pl.Versioned A.threadTokenPolicy Pl.PlutusV2,
                                  SomeMintsRedeemer $ sOutTxOutRef offerUtxo,
                                  A.tokenNameFromTxOutRef offerOref,
                                  NonZero (-1)
                                )
                              ],
                          txSkelOuts =
                            case previousBidder datum of
                              Nothing ->
                                let lot = sOutValue utxo <> Pl.negate theNft
                                 in [paysPK seller lot]
                              Just (lastBid, lastBidder) ->
                                let lot =
                                      sOutValue utxo
                                        <> Pl.negate (Ada.lovelaceValueOf lastBid)
                                        <> Pl.negate theNft
                                 in [ paysPK lastBidder lot,
                                      paysPK seller (Ada.lovelaceValueOf lastBid)
                                    ]
                        }
                _ -> error "expected one output with a datum"
