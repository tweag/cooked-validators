{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Auction.Offchain where

import qualified Auction as A
import Control.Monad
import Cooked
import Data.Default
import qualified Data.Map as Map
import qualified Ledger.Tx as Ledger
import Optics.Core
import qualified Plutus.Script.Utils.Ada as Ada
import qualified Plutus.Script.Utils.Typed as Pl
import qualified Plutus.Script.Utils.Value as Value
import qualified Plutus.V2.Ledger.Api as Pl
import qualified PlutusTx.Numeric as Pl

-- | Make an offer. There are no checks with this transaction. Anyone is allowed
-- to pay the 'auctionValidator' with something they want to sell, using the
-- 'Offer' datum to specify the seller of the auction.
--
-- This transaction returns the 'Pl.TxOutRef' of the 'Offer' UTxO it creates.
txOffer :: MonadBlockChain m => Wallet -> Pl.Value -> Integer -> m Pl.TxOutRef
txOffer seller lot minBid = do
  tx <-
    validateTxSkel $
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelOuts = [paysScript A.auctionValidator (A.Offer (walletPKHash seller) minBid) lot],
          txSkelSigners = [seller]
        }
  return . fst . (!! 0) . utxosFromCardanoTx $ tx

-- | Start an auction by setting the bidding deadline. This transaction consumes
-- the provided 'Offer' Utxo and returns a 'NoBids' UTxO to the auction
-- validator. It also mints the thread NFT that ensures the authenticity of the
-- auction from that point on.
txSetDeadline :: MonadBlockChain m => Wallet -> Pl.TxOutRef -> Pl.POSIXTime -> m Ledger.CardanoTx
txSetDeadline submitter offerOref deadline = do
  let theNft = A.threadToken offerOref
  Just (A.Offer seller minBid) <- typedDatumFromTxOutRef @(Pl.DatumType A.Auction) offerOref
  Just lot <- valueFromTxOutRef offerOref
  validateTxSkel $
    txSkelTemplate
      { txSkelOpts = def {txOptEnsureMinAda = True},
        txSkelSigners = [submitter],
        txSkelMints =
          txSkelMintsFromList
            [ ( Pl.Versioned A.threadTokenPolicy Pl.PlutusV2,
                SomeMintsRedeemer offerOref,
                A.tokenNameFromTxOutRef offerOref,
                1
              )
            ],
        txSkelIns = Map.singleton offerOref $ TxSkelRedeemerForScript A.SetDeadline,
        txSkelOuts =
          [ paysScript
              A.auctionValidator
              (A.NoBids seller minBid deadline)
              (lot <> theNft)
          ]
      }

previousBidder :: A.AuctionState -> Maybe (Integer, Pl.PubKeyHash)
previousBidder (A.Bidding _ _ (A.BidderInfo bid bidder)) = Just (bid, bidder)
previousBidder _ = Nothing

-- | Bid a certain amount of Lovelace on the auction with the given 'Offer'
-- UTxO. If there was a previous bidder, they will receive their money back.
txBid :: MonadBlockChain m => Wallet -> Pl.TxOutRef -> Integer -> m Ledger.CardanoTx
txBid submitter offerOref bid = do
  let theNft = A.threadToken offerOref
  [(oref, output)] <-
    runUtxoSearch $
      utxosAtSearch (Pl.validatorAddress A.auctionValidator)
        `filterWithPred` ((`Value.geq` theNft) . outputValue)
        `filterWith` resolveDatum
        `filterWithPure` isOutputWithInlineDatumOfType @A.AuctionState
  let datum = output ^. outputDatumL
      Just deadline = A.getBidDeadline datum
      seller = A.getSeller datum
      lotPlusPreviousBidPlusNft = outputValue output
  -- In an ideal world, we would not have to subtract 1 millisecond from the
  -- deadline here. As it stands at the moment, we have to do it in order to
  -- account for a few subtle bugs/features of the implementation of plutus and
  -- the ledger. For more explanation see here:
  --
  -- https://github.com/tweag/cooked-validators/issues/309
  validityInterval <- slotRangeBefore (deadline - 1)
  validateTxSkel $
    txSkelTemplate
      { txSkelOpts = def {txOptEnsureMinAda = True},
        txSkelSigners = [submitter],
        txSkelIns =
          Map.singleton oref $
            TxSkelRedeemerForScript
              (A.Bid (A.BidderInfo bid (walletPKHash submitter))),
        txSkelOuts =
          case previousBidder datum of
            Nothing ->
              [ paysScript
                  A.auctionValidator
                  (A.Bidding seller deadline (A.BidderInfo bid (walletPKHash submitter)))
                  (lotPlusPreviousBidPlusNft <> Ada.lovelaceValueOf bid)
              ]
            Just (prevBid, prevBidder) ->
              [ paysPK prevBidder (Ada.lovelaceValueOf prevBid),
                paysScript
                  A.auctionValidator
                  (A.Bidding seller deadline (A.BidderInfo bid (walletPKHash submitter)))
                  (lotPlusPreviousBidPlusNft <> Ada.lovelaceValueOf (bid - prevBid))
              ],
        txSkelValidityRange = validityInterval
      }

-- | Close the auction with the given 'Offer' UTxO. If there were any bids, this
-- will pay the lot to the last bidder and the last bid to the
-- seller. Otherwise, the seller will receive the lot back. This transaction
-- also burns the thread token.
txHammer :: MonadBlockChain m => Wallet -> Pl.TxOutRef -> m ()
txHammer submitter offerOref = do
  let theNft = A.threadToken offerOref
  utxos <-
    runUtxoSearch $
      utxosAtSearch (Pl.validatorAddress A.auctionValidator)
        `filterWithPred` ((`Value.geq` theNft) . outputValue)
        `filterWith` resolveDatum
        `filterWithPure` isOutputWithInlineDatumOfType @A.AuctionState
  case utxos of
    [] ->
      -- There's no thread token, so the auction is still in 'Offer' state, and
      -- the 'offerOref' still points to an UTxO at the auction validator.
      do
        Just (A.Offer seller _minBid) <- typedDatumFromTxOutRef @A.AuctionState offerOref
        Just lot <- valueFromTxOutRef offerOref
        void $
          validateTxSkel $
            txSkelTemplate
              { txSkelOpts = def {txOptEnsureMinAda = True},
                txSkelSigners = [submitter],
                txSkelIns =
                  Map.singleton offerOref $
                    TxSkelRedeemerForScript (A.Hammer offerOref),
                txSkelOuts = [paysPK seller lot]
              }
    (oref, output) : _ -> do
      -- There is a thread token, so the auction is in 'NoBids' or 'Bidding'
      -- state, which means that the following pattern matches cannot fail:
      let datum = output ^. outputDatumL
          Just deadline = A.getBidDeadline datum
          seller = A.getSeller datum
      validityInterval <- slotRangeAfter deadline
      void $
        validateTxSkel $
          txSkelTemplate
            { txSkelOpts = def {txOptEnsureMinAda = True},
              txSkelSigners = [submitter],
              txSkelIns =
                Map.singleton oref $
                  TxSkelRedeemerForScript (A.Hammer offerOref),
              txSkelMints =
                txSkelMintsFromList
                  [ ( Pl.Versioned A.threadTokenPolicy Pl.PlutusV2,
                      SomeMintsRedeemer offerOref,
                      A.tokenNameFromTxOutRef offerOref,
                      -1
                    )
                  ],
              txSkelOuts =
                case previousBidder datum of
                  Nothing ->
                    let lot = outputValue output <> Pl.negate theNft
                     in [paysPK seller lot]
                  Just (lastBid, lastBidder) ->
                    let lot = outputValue output <> Pl.negate theNft <> Pl.negate (Ada.lovelaceValueOf lastBid)
                     in [ paysPK lastBidder lot,
                          paysPK seller (Ada.lovelaceValueOf lastBid)
                        ],
              txSkelValidityRange = validityInterval
            }
