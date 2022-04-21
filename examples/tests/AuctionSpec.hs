{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module AuctionSpec where

import qualified Auction as A
import qualified Auction.Offchain as A
import Control.Applicative
import Control.Arrow
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
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
bananaAssetClass = Value.assetClass (Value.currencySymbol "f00d") (Value.tokenName "Banana")

-- | Value representing a number of bananas
banana :: Integer -> Value.Value
banana = Value.assetClassValue bananaAssetClass

-- | How many bananas are in the given value? This is a left inverse of 'banana'.
bananasIn :: Value.Value -> Integer
bananasIn v = Value.assetClassValueOf v bananaAssetClass

-- | initial distribution s.t. the first wallet owns five bananas
testInit :: InitialDistribution
testInit = InitialDistribution $ M.alter addBananas (wallet 1) standard
  where
    InitialDistribution standard = def
    addBananas (Just v) = Just (v <> banana 5)
    addBananas Nothing = Nothing

-- | Parameters of an auction that sells two bananas at a minimum bid
-- of 2 Lovelace and a bidding deadline in 60 seconds from the given
-- time.
bananaParams :: L.POSIXTime -> A.Parameters'
bananaParams t =
  A.Parameters'
    { A.lot' = banana 2,
      A.minBid' = 3,
      A.bidDeadline' = t + 60_000
    }

-- * Successful single-trace runs

-- These runs use the transactions from Auction.Offchain as they are
-- meant to be used.

noBids :: MonadMockChain m => m ()
noBids = do
  t0 <- currentTime
  p <- A.txOpen (bananaParams t0) `as` wallet 1
  awaitTime (A.bidDeadline p + 1)
  A.txHammer p

oneBid :: MonadMockChain m => m ()
oneBid = do
  t0 <- currentTime
  p <- A.txOpen (bananaParams t0) `as` wallet 1
  A.txBid p 3 `as` wallet 2
  awaitTime (A.bidDeadline p + 1)
  A.txHammer p

twoBids :: MonadMockChain m => m ()
twoBids = do
  t0 <- currentTime
  p <- A.txOpen (bananaParams t0) `as` wallet 1
  A.txBid p 3 `as` wallet 2
  A.txBid p 4 `as` wallet 3
  awaitTime (A.bidDeadline p + 1)
  A.txHammer p

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
          twoBids
    ]

-- * Failing single-trace runs

failingOpen :: MonadMockChain m => m ()
failingOpen = do
  t0 <- currentTime
  _ <- A.txOpen (bananaParams t0) `as` wallet 2
  return ()

failingTwoBids :: MonadMockChain m => m ()
failingTwoBids = do
  t0 <- currentTime
  p <- A.txOpen (bananaParams t0) `as` wallet 1
  A.txBid p (A.minBid p) `as` wallet 2
  A.txBid p (A.minBid p) `as` wallet 3
  awaitTime (A.bidDeadline p + 1)
  A.txHammer p

failingSingle :: TestTree
failingSingle =
  testGroup
    "Single-trace runs that are expected to fail"
    [ testCase "opening banana auction while owning no bananas" $
        testFailsFrom testInit failingOpen,
      testCase "second bid lower than first" $
        testFailsFrom'
          isCekEvaluationFailure
          testInit
          failingTwoBids
    ]

-- * A (hopefully) failing double-minting attack

-- Whenever we see a transaction that mints something, try to mint ont
-- more token and pay it to the attacker. This should be ruled out by
-- the minting policy of the thread token.

-- We use the 'somehwhere' modality to try this attack on all of the
-- successful runs from above.

attack :: TxSkel -> Maybe TxSkel
attack (TxSkel label opts cs) = case toConstraints cs of
  is :=>: os -> (\(xi, xo) -> TxSkel label opts (xi :=>: (xo : os))) <$> modConstrs is
    where
      modConstrs :: [MiscConstraint] -> Maybe ([MiscConstraint], OutConstraint)
      modConstrs [] = Nothing
      modConstrs (Mints red pols v : is) =
        case Value.flattenValue v of
          [(cs, tok, n)] ->
            Just
              ( Mints red pols (Value.singleton cs tok (n + 1)) : is,
                paysPK attackerPKHash (Value.singleton cs tok 1)
              )
          _ -> Nothing
      modConstrs (i : is) = first (i :) <$> modConstrs is

      attackerPKHash :: L.PubKeyHash
      attackerPKHash = walletPKHash (wallet 6)

tryDupTokens :: (Alternative m, MonadModalMockChain m) => m ()
tryDupTokens = somewhere attack (noBids <|> oneBid <|> twoBids)

attacks :: TestTree
attacks =
  testGroup
    "Attacks"
    [ testCase "Double-minting" $
        testFailsFrom'
          isCekEvaluationFailure
          testInit
          tryDupTokens
    ]

-- * Comparing two outcomes with 'testBinaryRelatedBy'

-- Produce two outcomes, which differ only by who the (only) bidder in
-- the auction was. Then test that the sellers and buyers in both
-- "worlds" have paid the same amounts.

bidderAlternativeTrace :: (Alternative m, MonadMockChain m) => m ()
bidderAlternativeTrace = do
  t0 <- currentTime
  p <- A.txOpen (bananaParams t0) `as` wallet 1
  A.txBid p 9 `as` wallet 2 <|> A.txBid p 9 `as` wallet 3
  awaitTime (A.bidDeadline p)
  A.txHammer p

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
    "Miscellaneuos tests"
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
