{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module AuctionSpec where

import qualified Auction as A
import qualified Auction.Offchain as A
import Control.Applicative
import Control.Arrow
import Control.Monad
import Cooked.Attack
import Cooked.Currencies
import Cooked.Ltl
import Cooked.MockChain
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Optics
import Data.Default
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import qualified Ledger as L
import qualified Ledger.Ada as Ada
import qualified Ledger.Value as Value
import Test.Tasty
import Test.Tasty.HUnit

-- * Parameters and initial distributions

-- Just so we have something to sell in our auction that's not Ada:
-- Have a banana.

bananaAssetClass :: Value.AssetClass
bananaAssetClass = permanentAssetClass "Banana"

-- | Value representing a number of bananas
banana :: Integer -> Value.Value
banana = Value.assetClassValue bananaAssetClass

-- | How many bananas are in the given value? This is a left inverse of 'banana'.
bananasIn :: Value.Value -> Integer
bananasIn v = Value.assetClassValueOf v bananaAssetClass

-- | initial distribution s.t. the first wallet owns five bananas
testInit :: InitialDistribution
testInit =
  InitialDistribution $
    M.insert
      (wallet 1)
      (Ada.lovelaceValueOf 100_000_000 <> banana 5 : (standard M.! wallet 1))
      standard
  where
    InitialDistribution standard = def

-- * Successful single-trace runs

-- These runs use the transactions from Auction.Offchain as they are meant to be
-- used.

noBids :: MonadMockChain m => m ()
noBids = do
  t0 <- currentTime
  let deadline = t0 + 60_000
  offerUtxo <- A.txOffer (banana 2) 30_000_000 `as` wallet 1
  A.txSetDeadline offerUtxo deadline
  awaitTime (deadline + 1)
  A.txHammer offerUtxo

oneBid :: MonadMockChain m => m ()
oneBid = do
  t0 <- currentTime
  let deadline = t0 + 60_000
  offerUtxo <- A.txOffer (banana 2) 30_000_000 `as` wallet 1
  A.txSetDeadline offerUtxo deadline
  A.txBid offerUtxo 30_000_000 `as` wallet 2
  awaitTime (deadline + 1)
  A.txHammer offerUtxo

twoBids :: MonadMockChain m => m ()
twoBids = do
  t0 <- currentTime
  let deadline = t0 + 60_000
  offerUtxo <- A.txOffer (banana 2) 30_000_000 `as` wallet 1
  A.txSetDeadline offerUtxo deadline
  A.txBid offerUtxo 30_000_000 `as` wallet 2
  A.txBid offerUtxo 40_000_000 `as` wallet 3
  awaitTime (deadline + 1)
  A.txHammer offerUtxo

twoAuctions :: MonadMockChain m => m ()
twoAuctions = do
  t0 <- currentTime
  let deadline1 = t0 + 60_000
      deadline2 = t0 + 90_000
  offerUtxo1 <- A.txOffer (banana 2) 30_000_000 `as` wallet 1
  offerUtxo2 <- A.txOffer (banana 3) 50_000_000 `as` wallet 1
  A.txSetDeadline offerUtxo1 deadline1
  A.txSetDeadline offerUtxo2 deadline2
  A.txBid offerUtxo1 30_000_000 `as` wallet 2
  A.txBid offerUtxo2 50_000_000 `as` wallet 3
  A.txBid offerUtxo2 60_000_000 `as` wallet 4
  awaitTime (deadline1 + 1)
  A.txHammer offerUtxo1
  awaitTime (deadline2 + 1)
  A.txHammer offerUtxo2

-- | helper function to compute what the given wallet owns in the
-- given state
holdingInState :: UtxoState -> Wallet -> L.Value
holdingInState (UtxoState m) w
  | Just vs <- M.lookup (walletAddress w) m = utxoValueSetTotal vs
  | otherwise = mempty

successfulSingle :: TestTree
successfulSingle =
  testGroup
    "Successful single-trace runs"
    [ testCase "zero bids" $ testSucceedsFrom testInit noBids,
      testCase "one bid" $ testSucceedsFrom testInit oneBid,
      testCase "two bids on the same auction" $
        testSucceedsFrom'
          (\_ s -> testBool $ 2 == bananasIn (holdingInState s (wallet 3)))
          testInit
          twoBids,
      testCase
        "two concurrent auctions"
        $ testSucceedsFrom'
          ( \_ s ->
              testBool (2 == bananasIn (holdingInState s (wallet 2)))
                .&&. testBool (3 == bananasIn (holdingInState s (wallet 4)))
          )
          testInit
          twoAuctions
    ]

-- * Failing single-trace runs

failingOffer :: MonadMockChain m => m ()
failingOffer =
  void $
    A.txOffer (banana 1) 20_000_000 `as` wallet 2

failingTwoBids :: MonadMockChain m => m ()
failingTwoBids = do
  t0 <- currentTime
  let deadline = t0 + 60_000
  offerUtxo <- A.txOffer (banana 2) 30_000_000 `as` wallet 1
  A.txSetDeadline offerUtxo deadline
  A.txBid offerUtxo 30_000_000 `as` wallet 2
  A.txBid offerUtxo 30_000_000 `as` wallet 3
  awaitTime (deadline + 1)
  A.txHammer offerUtxo

failingSingle :: TestTree
failingSingle =
  testGroup
    "Single-trace runs that are expected to fail"
    [ testCase "opening banana auction while owning no bananas" $
        testFailsFrom testInit failingOffer,
      testCase "second bid not higher than first" $
        testFailsFrom'
          isCekEvaluationFailure
          testInit
          failingTwoBids
    ]

-- * (hopefully) failing attacks

simpleTraces :: (Alternative m, MonadMockChain m) => m ()
simpleTraces = noBids <|> oneBid <|> twoBids <|> twoAuctions

-- | Token duplication attack: Whenever we see a transaction that mints
-- something, try to mint one more token and pay it to the attacker. This should
-- be ruled out by the minting policy of the thread token.
tryDupTokens :: (Alternative m, MonadModalMockChain m) => m ()
tryDupTokens =
  somewhere
    ( dupTokenAttack
        (\_ n -> Just $ n + 1) -- the modification of the minted value
        (wallet 6) -- the attacker's wallet
    )
    simpleTraces

-- | Datum hijacking attack: Try to steal outputs from a validator.
tryDatumHijack :: (Alternative m, MonadModalMockChain m) => m ()
tryDatumHijack =
  somewhere
    ( datumHijackingAttack @A.Auction
        ( \_ d _ -> case d of -- try to steal all outputs that have the 'Bidding' datum, no matter their validator or value
            A.Bidding {} -> True
            _ -> False
        )
        (0 ==) -- if there is more than one 'Bidding' output, try stealing only the first
    )
    simpleTraces

-- | Double satisfaction attack. This is essentially the scenario of the trace
-- 'twoAuctions', where we try to add 'Hammer' and 'Bid' Inputs.
tryDoubleSat :: MonadModalMockChain m => m ()
tryDoubleSat = do
  t0 <- currentTime
  let deadline1 = t0 + 60_000
      deadline2 = t0 + 90_000
  offerUtxo1 <- A.txOffer (banana 2) 30_000_000 `as` wallet 1
  offerUtxo2 <- A.txOffer (banana 3) 50_000_000 `as` wallet 1
  somewhere
    ( doubleSatAttack
        ( dsAddOneSscToSsc
            A.auctionValidator
            ( \_ _ ->
                A.Hammer (fst offerUtxo1) :
                A.Hammer (fst offerUtxo2) :
                map
                  (A.Bid . uncurry A.BidderInfo)
                  [ (5, walletPKHash $ wallet 1),
                    (5, walletPKHash $ wallet 6),
                    (4, walletPKHash $ wallet 1),
                    (4, walletPKHash $ wallet 6),
                    (3, walletPKHash $ wallet 1),
                    (3, walletPKHash $ wallet 6)
                  ]
            )
            (wallet 6)
        )
    )
    ( do
        A.txSetDeadline offerUtxo1 deadline1
        A.txSetDeadline offerUtxo2 deadline2
        A.txBid offerUtxo1 30_000_000 `as` wallet 2
        A.txBid offerUtxo2 50_000_000 `as` wallet 3
        A.txBid offerUtxo2 60_000_000 `as` wallet 4
        awaitTime (deadline1 + 1)
        A.txHammer offerUtxo1
        awaitTime (deadline2 + 1)
        A.txHammer offerUtxo2
    )

-- | datum tampering attack that tries to change the bidder to wallet 6 on the
-- 'Bidding' datum
tryTamperDatum :: (Alternative m, MonadModalMockChain m) => m ()
tryTamperDatum =
  somewhere
    ( tamperDatumAttack @A.Auction
        ( \case
            A.Bidding seller deadline (A.BidderInfo x _) ->
              Just $ A.Bidding seller deadline (A.BidderInfo x (walletPKHash $ wallet 6))
            _ -> Nothing
        )
    )
    simpleTraces

attacks :: TestTree
attacks =
  testGroup
    "Attacks"
    [ testCase "token duplication" $
        testFailsFrom'
          -- Ensure that the trace fails and gives back an error message satisfying a specific condition
          ( isCekEvaluationFailureWithMsg
              (\msg -> "not minting or burning" `isPrefixOf` msg || "Hammer does not burn" `isPrefixOf` msg)
          )
          testInit
          tryDupTokens,
      testCase "datum hijacking" $
        testFailsFrom'
          isCekEvaluationFailure
          testInit
          tryDatumHijack,
      testCase "double satisfaction" $
        testFailsFrom'
          isCekEvaluationFailure
          testInit
          tryDoubleSat,
      testCase "datum tampering" $
        testFailsFrom'
          isCekEvaluationFailure
          testInit
          tryTamperDatum
    ]

-- * Comparing two outcomes with 'testBinaryRelatedBy'

-- Produce two outcomes, which differ only by who the (only) bidder in the
-- auction was. Then test that the sellers and buyers in both "worlds" have paid
-- the same amounts.

bidderAlternativeTrace :: (Alternative m, MonadMockChain m) => m ()
bidderAlternativeTrace = do
  t0 <- currentTime
  let deadline = t0 + 60_000
  offerUtxo <- A.txOffer (banana 2) 30_000_000 `as` wallet 1
  A.txSetDeadline offerUtxo deadline
  A.txBid offerUtxo 30_000_000 `as` wallet 2 <|> A.txBid offerUtxo 30_000_000 `as` wallet 3
  awaitTime (deadline + 1)
  A.txHammer offerUtxo

bidderAlternative :: TestTree
bidderAlternative =
  testCase "change in possessions independent of bidder" $
    testBinaryRelatedBy
      ( \a b ->
          testBool $
            holdingInState a (wallet 1) == holdingInState b (wallet 1)
              && holdingInState a (wallet 2) == holdingInState b (wallet 3)
      )
      testInit
      bidderAlternativeTrace

-- * Collecting all the tests in this module

miscTests :: TestTree
miscTests =
  testGroup
    "Miscellaneous tests"
    [bidderAlternative]

tests :: TestTree
tests =
  testGroup
    "AuctionSpec"
    [ successfulSingle,
      failingSingle,
      attacks,
      miscTests
    ]
