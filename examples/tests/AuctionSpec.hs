{-# LANGUAGE NumericUnderscores #-}

module AuctionSpec where

import qualified Auction as A
import qualified Auction.Offchain as A
import Cooked.MockChain
import qualified Ledger as L
import qualified Ledger.Ada as Ada
import Test.Tasty
import Test.Tasty.HUnit

testParams :: L.POSIXTime -> A.Parameters' ()
testParams t =
  A.Parameters'
    { A.seller = (),
      A.lot = Ada.lovelaceValueOf 20_000_000,
      A.minBid = 3,
      A.bidDeadline = t + 60_000
    }

-- * Successful runs using the transactions from Auction.Offchain

successfulNoBids :: TestTree
successfulNoBids = testCase "zero bids" $ testSucceeds trace
  where
    trace :: MonadMockChain m => m ()
    trace = do
      t0 <- currentTime
      p <- A.txOpen (testParams t0)
      awaitTime (A.bidDeadline p + 1)
      A.txHammer p

successfulOneBid :: TestTree
successfulOneBid = testCase "one bid" $ testSucceeds trace
  where
    trace :: MonadMockChain m => m ()
    trace = do
      t0 <- currentTime
      p <- A.txOpen (testParams t0) `as` wallet 1
      A.txBid p 3 `as` wallet 2
      awaitTime (A.bidDeadline p + 1)
      A.txHammer p

successfulTwoBids :: TestTree
successfulTwoBids = testCase "two bids" $ testSucceeds trace
  where
    trace :: MonadMockChain m => m ()
    trace = do
      t0 <- currentTime
      p <- A.txOpen (testParams t0) `as` wallet 1
      A.txBid p 3 `as` wallet 2
      A.txBid p 4 `as` wallet 3
      awaitTime (A.bidDeadline p + 1)
      A.txHammer p

successfulRuns :: TestTree
successfulRuns =
  testGroup
    "Successful runs"
    [ successfulNoBids,
      successfulOneBid,
      successfulTwoBids
    ]

-- * Failing runs using the transactions from Auction.Offchain

failingNoBids :: TestTree
failingNoBids = testCase "zero bids, no wait before hammer" $ testFails trace
  where
    trace :: MonadMockChain m => m ()
    trace = do
      t0 <- currentTime
      p <- A.txOpen (testParams t0)
      A.txHammer p

failingOneBid :: TestTree
failingOneBid = testCase "one bid, lower than the minimum bid" $ testFails trace
  where
    trace :: MonadMockChain m => m ()
    trace = do
      t0 <- currentTime
      p <- A.txOpen (testParams t0) `as` wallet 1
      A.txBid p (A.minBid p - 1) `as` wallet 2
      awaitTime (A.bidDeadline p + 1)
      A.txHammer p

failingTwoBids :: TestTree
failingTwoBids = testCase "second bid not higher than first" $ testFails trace
  where
    trace :: MonadMockChain m => m ()
    trace = do
      t0 <- currentTime
      p <- A.txOpen (testParams t0) `as` wallet 1
      A.txBid p (A.minBid p) `as` wallet 2
      A.txBid p (A.minBid p) `as` wallet 3
      awaitTime (A.bidDeadline p + 1)
      A.txHammer p

failingRuns :: TestTree
failingRuns =
  testGroup
    "Failing runs (that are expected to fail)"
    [ failingNoBids,
      failingOneBid,
      failingTwoBids
    ]


-- * Attacks using custom transactions

attacks :: TestTree
attacks = testGroup "Attacks" []

tests :: TestTree
tests = testGroup "AuctionSpec" [successfulRuns, failingRuns, attacks]
