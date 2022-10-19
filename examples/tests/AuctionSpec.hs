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
import Cooked
import Data.Default
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import qualified Ledger as L
import qualified Ledger.Ada as Ada
import qualified Ledger.Value as Value
import Optics.Core
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

-- | initial distribution s.t. everyone owns five bananas
testInit :: InitialDistribution
testInit = initialDistribution' [(i, [minAda <> banana 5]) | i <- knownWallets]

-- * Successful single-trace runs

-- These runs use the transactions from Auction.Offchain as they are meant to be
-- used.

hammerToWithdraw :: MonadMockChain m => m ()
hammerToWithdraw = do
  offerUtxo <- A.txOffer (banana 2) 30_000_000 `as` wallet 1
  A.txHammer offerUtxo `as` wallet 1

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
      testCase "hammer to withdraw" $ testSucceedsFrom testInit hammerToWithdraw,
      testCase "one bid" $ testSucceedsFrom testInit oneBid,
      testCase "two bids on the same auction" $
        testSucceedsFrom'
          (\_ s -> testBool $ 7 == bananasIn (holdingInState s (wallet 3)))
          testInit
          twoBids,
      testCase
        "two concurrent auctions"
        $ testSucceedsFrom'
          ( \_ s ->
              testBool (7 == bananasIn (holdingInState s (wallet 2)))
                .&&. testBool (8 == bananasIn (holdingInState s (wallet 4)))
          )
          testInit
          twoAuctions
    ]

-- * Failing single-trace runs

failingOffer :: MonadMockChain m => m ()
failingOffer =
  void $
    A.txOffer (banana 100) 20_000_000 `as` wallet 2

forbiddenHammerToWithdraw :: MonadMockChain m => m ()
forbiddenHammerToWithdraw = do
  offerUtxo <- A.txOffer (banana 2) 30_000_000 `as` wallet 1
  A.txHammer offerUtxo `as` wallet 2

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
    [ testCase "opening banana auction while owning too few bananas" $
        testFailsFrom testInit failingOffer,
      testCase "wrong user hammers to withdraw" $
        testFailsFrom'
          isCekEvaluationFailure
          testInit
          forbiddenHammerToWithdraw,
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
        (\_ n -> n + 1) -- the modification of the minted value
        (wallet 6) -- the attacker's wallet
    )
    simpleTraces

-- | Datum hijacking attack: Try to steal outputs from a validator.
tryDatumHijack :: (Alternative m, MonadModalMockChain m) => m ()
tryDatumHijack =
  somewhere
    ( datumHijackingAttack @A.Auction
        ( \_ d _ -> case d of
            -- try to steal all outputs that have the 'Bidding' datum, no matter
            -- their validator or value.
            A.Bidding {} -> True
            -- try to steal during the 'SetDeadline' transaction. This
            -- vulnerability existed before PR #161.
            A.NoBids {} -> True
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
  offerUtxo2 <- A.txOffer (banana 3) 50_000_000 `as` wallet 2
  somewhere
    ( doubleSatAttack
        -- look at all 'SpendsScript' constraints going to a validator of type
        -- 'A.Auction'.
        (spendsScriptConstraintsT % spendsScriptConstraintTypeP @A.Auction)
        -- try consuming an additional UTxO from the same validator with one of
        -- five different reddemers.
        ( \mcst (val, _, _) ->
            concatMap
              ( \utxo ->
                  map
                    toConstraints
                    ( SpendsScript val (A.Hammer (fst offerUtxo1)) (fst utxo) :
                      SpendsScript val (A.Hammer (fst offerUtxo2)) (fst utxo) :
                      map
                        ( \(amount, bidder) ->
                            SpendsScript
                              val
                              (A.Bid $ A.BidderInfo amount bidder)
                              (fst utxo)
                        )
                        [ (50_000_000, walletPKHash $ wallet 1),
                          (50_000_000, walletPKHash $ wallet 2),
                          (50_000_000, walletPKHash $ wallet 6)
                        ]
                    )
              )
              ( scriptUtxosSuchThatMcst
                  mcst
                  val
                  (\_ _ -> True)
              )
        )
        -- pay the surplus to wallet 6
        (wallet 6)
        -- try each extra redeemer on a different modified transaction
        AllSeparate
    )
    ( do
        A.txSetDeadline offerUtxo1 deadline1
        A.txSetDeadline offerUtxo2 deadline2
        A.txBid offerUtxo1 30_000_000 `as` wallet 3
        A.txBid offerUtxo2 50_000_000 `as` wallet 4
        A.txBid offerUtxo2 60_000_000 `as` wallet 5
        awaitTime (deadline2 + 1)
        A.txHammer offerUtxo1
        A.txHammer offerUtxo2
    )

-- | datum tampering attack that tries to change the seller to wallet 6 on every
-- datum but 'Offer' (which is any time we pay to the 'auctionValidator' and
-- there are actual checks happening).
tryTamperDatum :: (Alternative m, MonadModalMockChain m) => m ()
tryTamperDatum =
  somewhere
    ( tamperDatumTweak @A.Auction
        ( \case
            A.NoBids seller minBid deadline ->
              Just $ A.NoBids (walletPKHash $ wallet 6) minBid deadline
            A.Bidding seller deadline bidderInfo ->
              Just $ A.Bidding (walletPKHash $ wallet 6) deadline bidderInfo
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
