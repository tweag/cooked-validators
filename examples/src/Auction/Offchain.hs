{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Auction.Offchain where

import qualified Auction as A
import Control.Monad
import Cooked
import Data.Default
import qualified Data.Map as Map
import Data.Maybe
import qualified Ledger as L
import qualified Ledger.Ada as Ada
import qualified Ledger.Interval as Interval
import qualified Ledger.Tx as Pl
import qualified Ledger.Value as Value
import Optics.Core
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Pl
import qualified PlutusTx.Numeric as Pl
import Test.QuickCheck.Modifiers (NonZero (..))

-- | Make an offer. There are no checks with this transaction. Anyone is allowed
-- to pay the 'auctionValidator' with something they want to sell, using the
-- 'Offer' datum to specify the seller of the auction.
--
-- This transaction returns the 'Pl.TxOutRef' of the 'Offer' UTxO it creates.
txOffer :: MonadBlockChain m => Wallet -> L.Value -> Integer -> m Pl.TxOutRef
txOffer seller lot minBid = do
  tx <-
    validateTxSkel $
      def
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelOuts = [paysScript A.auctionValidator (A.Offer (walletPKHash seller) minBid) lot],
          txSkelSigners = [seller]
        }
  return . head
    . mapMaybe
      ( \(oref, out) -> case isScriptOutputFrom A.auctionValidator out of
          Nothing -> Nothing
          Just _ -> Just oref
      )
    . utxosFromCardanoTx
    $ tx

-- | Start an auction by setting the bidding deadline. This transaction consumes
-- the provided 'Offer' Utxo and returns a 'NoBids' UTxO to the auction
-- validator. It also mints the thread NFT that ensures the authenticity of the
-- auction from that point on.
txSetDeadline :: MonadBlockChain m => Wallet -> Pl.TxOutRef -> L.POSIXTime -> m Pl.CardanoTx
txSetDeadline submitter offerOref deadline = do
  let theNft = A.threadToken offerOref
  Just (A.Offer seller minBid) <- typedDatumFromTxOutRef @(Pl.DatumType A.Auction) offerOref
  Just lot <- valueFromTxOutRef offerOref
  validateTxSkel $
    def
      { txSkelOpts = def {txOptEnsureMinAda = True},
        txSkelSigners = [submitter],
        txSkelMints =
          txSkelMintsFromList
            [ ( Pl.Versioned A.threadTokenPolicy Pl.PlutusV2,
                SomeMintsRedeemer offerOref,
                A.tokenNameFromTxOutRef offerOref,
                NonZero 1
              )
            ],
        txSkelIns = Map.singleton offerOref $ TxSkelRedeemerForScript @A.Auction A.SetDeadline,
        txSkelOuts =
          [ paysScript
              A.auctionValidator
              (A.NoBids seller minBid deadline)
              (lot <> theNft)
          ]
      }

previousBidder :: A.AuctionState -> Maybe (Integer, L.PubKeyHash)
previousBidder (A.Bidding _ _ (A.BidderInfo bid bidder)) = Just (bid, bidder)
previousBidder _ = Nothing

resolveOutputDatum ::
  MonadBlockChainWithoutValidation m =>
  output ->
  m (Maybe (ConcreteOutput (OwnerType output) L.Datum (ValueType output)))
resolveOutputDatum = undefined

-- | Bid a certain amount of Lovelace on the auction with the given 'Offer'
-- UTxO. If there was a previous bidder, they will receive their money back.
txBid :: MonadBlockChain m => Wallet -> Pl.TxOutRef -> Integer -> m L.CardanoTx
txBid submitter offerOref bid = do
  let theNft = A.threadToken offerOref
  [(oref, output)] <-
    filteredUtxosWithDatums $
      isOutputWithValueSuchThat (`Value.geq` theNft)
        <=< isScriptOutputFrom' A.auctionValidator
  let ResolvedOrInlineDatum datum = output ^. outputDatumL
      Just deadline = A.getBidDeadline datum
      seller = A.getSeller datum
      lotPlusPreviousBidPlusNft = outputValue output
  validateTxSkel $
    def
      { txSkelOpts = def {txOptEnsureMinAda = True},
        txSkelSigners = [submitter],
        txSkelIns =
          Map.singleton oref $
            TxSkelRedeemerForScript
              @A.Auction
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
        txSkelValidityRange = Interval.to (deadline - 1)
      }

-- | Close the auction with the given 'Offer' UTxO. If there were any bids, this
-- will pay the lot to the last bidder and the last bid to the
-- seller. Otherwise, the seller will receive the lot back. This transaction
-- also burns the thread token.
txHammer :: MonadBlockChain m => Wallet -> Pl.TxOutRef -> m ()
txHammer submitter offerOref = do
  let theNft = A.threadToken offerOref
  utxos <-
    filteredUtxosWithDatums $
      isScriptOutputFrom' A.auctionValidator
        >=> isOutputWithValueSuchThat (`Value.geq` theNft)
  case utxos of
    [] ->
      -- There's no thread token, so the auction is still in 'Offer' state, and
      -- the 'offerOref' still points to an UTxO at the auction validator.
      do
        Just (A.Offer seller _minBid) <- typedDatumFromTxOutRef @A.AuctionState offerOref
        Just lot <- valueFromTxOutRef offerOref
        void $
          validateTxSkel $
            def
              { txSkelOpts = def {txOptEnsureMinAda = True},
                txSkelSigners = [submitter],
                txSkelIns =
                  Map.singleton offerOref $
                    TxSkelRedeemerForScript @A.Auction (A.Hammer offerOref),
                txSkelOuts = [paysPK seller lot]
              }
    (oref, output) : _ -> do
      -- There is a thread token, so the auction is in 'NoBids' or 'Bidding'
      -- state, which means that the following pattern matches cannot fail:
      let ResolvedOrInlineDatum datum = output ^. outputDatumL
          Just deadline = A.getBidDeadline datum
          seller = A.getSeller datum
      void $
        validateTxSkel $
          def
            { txSkelOpts = def {txOptEnsureMinAda = True},
              txSkelSigners = [submitter],
              txSkelIns =
                Map.singleton oref $
                  TxSkelRedeemerForScript @A.Auction (A.Hammer offerOref),
              txSkelMints =
                txSkelMintsFromList
                  [ ( Pl.Versioned A.threadTokenPolicy Pl.PlutusV2,
                      SomeMintsRedeemer offerOref,
                      A.tokenNameFromTxOutRef offerOref,
                      NonZero (-1)
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
              txSkelValidityRange = Interval.from deadline
            }
