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
import Cooked
import Cooked.Attack
import Data.Default
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import qualified Ledger as L
import qualified Ledger.Ada as Ada
import qualified Ledger.Value as Value
import Optics.Core
import Test.Tasty
import Test.Tasty.ExpectedFailure
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

-- | Parameters of an auction that sells two bananas at a minimum bid
-- of 2 Lovelace and a bidding deadline in 60 seconds from the given
-- time.
bananaParams :: L.POSIXTime -> A.StaticValParams
bananaParams t =
  A.StaticValParams
    { A.lot' = banana 2,
      A.minBid' = 3,
      A.bidDeadline' = t + 60_000
    }

otherParams :: L.POSIXTime -> A.StaticValParams
otherParams t =
  A.StaticValParams
    { A.lot' = banana 3,
      A.minBid' = 5,
      A.bidDeadline' = t + 90_000
    }

-- * Successful single-trace runs

-- These runs use the transactions from Auction.Offchain as they are
-- meant to be used.

noBids :: MonadMockChain m => m ()
noBids = do
  t0 <- currentTime
  (p, q) <- A.txOpen (bananaParams t0) `as` wallet 1
  awaitTime (A.bidDeadline p + 1)
  A.txHammer p q

oneBid :: MonadMockChain m => m ()
oneBid = do
  t0 <- currentTime
  (p, q) <- A.txOpen (bananaParams t0) `as` wallet 1
  A.txBid p 3 `as` wallet 2
  awaitTime (A.bidDeadline p + 1)
  A.txHammer p q

twoBids :: MonadMockChain m => m ()
twoBids = do
  t0 <- currentTime
  (p, q) <- A.txOpen (bananaParams t0) `as` wallet 1
  A.txBid p 3 `as` wallet 2
  A.txBid p 4 `as` wallet 3
  awaitTime (A.bidDeadline p + 1)
  A.txHammer p q

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
  (p, q) <- A.txOpen (bananaParams t0) `as` wallet 1
  A.txBid p (A.minBid p) `as` wallet 2
  A.txBid p (A.minBid p) `as` wallet 3
  awaitTime (A.bidDeadline p + 1)
  A.txHammer p q

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

-- * (hopefully) failing attacks

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
    (noBids <|> oneBid <|> twoBids)

-- | Datum hijacking attack: Try to steal outputs from a validator.
tryDatumHijack :: (Alternative m, MonadModalMockChain m) => m ()
tryDatumHijack =
  somewhere
    ( datumHijackingAttack @A.Auction
        ( \_ d _ -> case d of -- try to steal all outputs that have the 'Bidding' datum, no matter their validator or value
            A.Bidding _ -> True
            _ -> False
        )
        (0 ==) -- if there is more than one 'Bidding' output, try stealing only the first
    )
    (noBids <|> oneBid <|> twoBids)

-- | Double satisfaction attack
tryDoubleSat :: MonadModalMockChain m => m ()
tryDoubleSat = do
  t0 <- currentTime
  (p, q) <- A.txOpen (bananaParams t0) `as` wallet 1
  somewhere
    ( doubleSatAttack
        -- look at all 'SpendsScript' constraints going to a validator of type
        -- 'A.Auction'.
        (spendsScriptConstraintsT % spendsScriptConstraintTypeP @A.Auction)
        -- If the constraint uses the 'A.Hammer' redeemer, try consuming an
        -- additional UTxO from the same validator with one of six different
        -- 'A.Bid' redeemers.
        ( \mcst (val, r, _) ->
            let utxos =
                  scriptUtxosSuchThatMcst
                    mcst
                    val
                    (\_ _ -> True)
             in case r of
                  A.Hammer ->
                    concatMap
                      ( \utxo ->
                          map
                            ( \(amount, bidder) ->
                                toConstraints $
                                  SpendsScript
                                    val
                                    (A.Bid $ A.BidderInfo amount bidder)
                                    (fst utxo)
                            )
                            [ (5, walletPKHash $ wallet 1),
                              (5, walletPKHash $ wallet 6),
                              (4, walletPKHash $ wallet 1),
                              (4, walletPKHash $ wallet 6),
                              (3, walletPKHash $ wallet 1),
                              (3, walletPKHash $ wallet 6)
                            ]
                      )
                      utxos
                  _ -> []
        )
        -- pay the surplus to wallet 6
        (wallet 6)
        -- try each extra redeemer on a different modified transaction
        AllSeparate
    )
    ( do
        A.txBid p 3 `as` wallet 2
        A.txBid p 4 `as` wallet 3
        awaitTime (A.bidDeadline p + 1)
        A.txHammer p q
    )

-- | datum tampering attack that tries to change the bidder to wallet 6 on the
-- 'Bidding' datum
tryTamperDatum :: (Alternative m, MonadModalMockChain m) => m ()
tryTamperDatum =
  somewhere
    ( tamperDatumTweak @A.Auction
        ( \case
            A.Bidding (A.BidderInfo x _) ->
              Just $ A.Bidding (A.BidderInfo x (walletPKHash $ wallet 6))
            _ -> Nothing
        )
    )
    (noBids <|> oneBid <|> twoBids)

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

-- * known vulnerabilities

stealBidTwoAuctions :: MonadModalMockChain m => m ()
stealBidTwoAuctions = do
  t0 <- currentTime
  -- Alice opens two auctions (it does not matter that they both belong to her,
  -- this vulnerability applies to any two auctions)
  (p, q) <- A.txOpen (bananaParams t0) `as` alice
  (pOther, qOther) <- A.txOpen (otherParams t0) `as` alice
  -- People now bid on the auctions until Bob is the highest bidder on both
  -- auctions.
  A.txBid p 40_000_000 `as` bob
  A.txBid pOther 60_000_000 `as` bob
  -- The UTxO at the first auction that represents the current state:
  [(theLastBidUtxo, _)] <- scriptUtxosSuchThat (A.auctionValidator p) (\_ _ -> True)
  -- Eve now bids on the second auction. Among other things this ensures that
  -- there's an output containing 60_000_000 Lovelace to Bob on the
  -- transaction. This means that, if she simultaneously bids on the first
  -- auction, she can fool the validator of the first auction, which expects an
  -- output of at least 40_000_000 Lovelace to go to Bob. She can keep this
  -- money to herself, effectively stealing Bob's bid on the first auction.
  A.txBid pOther 70_000_000 `as` eve
    `withTweak` addConstraintsTweak
      ( [ Before (A.bidDeadline p),
          SpendsScript
            (A.auctionValidator p)
            (A.Bid (A.BidderInfo 50_000_000 (walletPKHash eve)))
            theLastBidUtxo
        ]
          :=>: [ paysScript
                   (A.auctionValidator p)
                   (A.Bidding (A.BidderInfo 50_000_000 (walletPKHash eve)))
                   ( A.lot p
                       <> Value.assetClassValue (A.threadTokenAssetClass p) 1
                       <> Ada.lovelaceValueOf 50_000_000
                   ),
                 paysPK
                   (walletPKHash eve)
                   (Ada.lovelaceValueOf 40_000_000)
               ]
      )
  -- Both auctions are closed normally. Eve is the highest bidder on both of
  -- them.
  awaitTime (A.bidDeadline p + 1)
  A.txHammer p q
  awaitTime (A.bidDeadline pOther + 1)
  A.txHammer pOther qOther
  where
    alice = wallet 1
    bob = wallet 3
    eve = wallet 6

vulns :: TestTree
vulns =
  testGroup "known vulnerabilities and exploits" $
    map
      expectFail
      [ testCase "stealing a bind from another auction" $
          testFailsFrom'
            isCekEvaluationFailure
            testInit
            stealBidTwoAuctions
      ]

-- * Comparing two outcomes with 'testBinaryRelatedBy'

-- Produce two outcomes, which differ only by who the (only) bidder in
-- the auction was. Then test that the sellers and buyers in both
-- "worlds" have paid the same amounts.

bidderAlternativeTrace :: (Alternative m, MonadMockChain m) => m ()
bidderAlternativeTrace = do
  t0 <- currentTime
  (p, q) <- A.txOpen (bananaParams t0) `as` wallet 1
  A.txBid p 9 `as` wallet 2 <|> A.txBid p 9 `as` wallet 3
  awaitTime (A.bidDeadline p)
  A.txHammer p q

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
      vulns,
      miscTests
    ]
