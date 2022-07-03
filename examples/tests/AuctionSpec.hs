{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module AuctionSpec where

import qualified Auction as A
import qualified Auction.Offchain as A
import Control.Applicative
import Control.Arrow
import Cooked.Attack
import Cooked.Currencies
import Cooked.Ltl
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import Language.Pirouette.PlutusIR
import qualified Ledger as L
import qualified Ledger.Ada as Ada
import qualified Ledger.Value as Value
import Pirouette
import Pirouette.Monad
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
        (\_ n -> Just $ n + 1) -- the modification of the minted value
        (wallet 6) -- the attacker's wallet
    )
    (noBids <|> oneBid <|> twoBids)

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
          tryDatumHijack
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

-- * Pirouette tests

pirouetteTests :: TestTree
pirouetteTests =
  testGroup
    "Pirouette"
    [ testCase "bid-keeps-token" $
        pirouetteCompiledCode'
          (\st -> False)
          A.compiledValidate
          [pirDecls|
            fun isBid : Action -> Bool
                = \(a : Action) . Action_match a @Bool
                    (\(b : BidderInfo) . True)
                    False

            fun receivesToken : ValParams -> ScriptContext -> Bool
                = \(p : ValParams) (ctx : ScriptContext) . False
          |]
          ( [pir| \(res:Bool) (p:ValParams) (s:AuctionState) (a:Action) (ctx:ScriptContext) . if @Bool res then isBid a else False |]
              :==>: [pir| \(res:Bool) (p:ValParams) (s:AuctionState) (a:Action) (ctx:ScriptContext) .
                        receivesToken p ctx |]
          )
    ]

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
      miscTests,
      pirouetteTests
    ]
