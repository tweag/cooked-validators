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
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as Set
import qualified Ledger as L
import qualified Ledger.Tx as Pl
import Optics.Core
import qualified Plutus.Contract.Constraints as Pl
import qualified Plutus.Script.Utils.Ada as Ada
import qualified Plutus.Script.Utils.V1.Scripts as Pl
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Pl
import qualified Plutus.Script.Utils.Value as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import qualified PlutusTx.Numeric as Pl
import Test.QuickCheck.Modifiers (NonZero (..))
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

-- * Parameters and initial distributions

-- Just so we have something to sell in our auction that's not Ada:
-- Have a banana.

bananaAssetClass :: Pl.AssetClass
bananaAssetClass = permanentAssetClass "Banana"

-- | Value representing a number of bananas
banana :: Integer -> Pl.Value
banana = Pl.assetClassValue bananaAssetClass

-- | How many bananas are in the given value? This is a left inverse of 'banana'.
bananasIn :: Pl.Value -> Integer
bananasIn v = Pl.assetClassValueOf v bananaAssetClass

-- | initial distribution s.t. everyone owns five bananas
testInit :: InitialDistribution
testInit = initialDistribution' [(i, [Ada.lovelaceValueOf 20_000_000, banana 5]) | i <- knownWallets]

-- * Successful single-trace runs

-- These runs use the transactions from Auction.Offchain as they are meant to be
-- used.

hammerToWithdraw :: MonadBlockChain m => m ()
hammerToWithdraw = do
  offerOref <- A.txOffer (wallet 1) (banana 2) 30_000_000
  A.txHammer (wallet 1) offerOref

noBids :: MonadBlockChain m => m ()
noBids = do
  t0 <- currentTime
  let deadline = t0 + 60_000
  offerOref <- A.txOffer (wallet 1) (banana 2) 30_000_000
  A.txSetDeadline (wallet 1) offerOref deadline
  awaitTime (deadline + 1)
  A.txHammer (wallet 1) offerOref
  return ()

oneBid :: MonadBlockChain m => m ()
oneBid = do
  t0 <- currentTime
  let deadline = t0 + 60_000
  offerOref <- A.txOffer (wallet 1) (banana 2) 30_000_000
  A.txSetDeadline (wallet 1) offerOref deadline
  A.txBid (wallet 2) offerOref 30_000_000
  awaitTime (deadline + 1)
  A.txHammer (wallet 3) offerOref -- It doesn't matter which wallet hammers here

twoBids :: MonadBlockChain m => m ()
twoBids = do
  t0 <- currentTime
  let deadline = t0 + 60_000
  offerOref <- A.txOffer (wallet 1) (banana 2) 30_000_000
  A.txSetDeadline (wallet 1) offerOref deadline
  A.txBid (wallet 2) offerOref 30_000_000
  A.txBid (wallet 3) offerOref 40_000_000
  awaitTime (deadline + 1)
  A.txHammer (wallet 1) offerOref

twoAuctions :: MonadBlockChain m => m ()
twoAuctions = do
  t0 <- currentTime
  let deadline1 = t0 + 60_000
      deadline2 = t0 + 90_000
  offerOref1 <- A.txOffer (wallet 1) (banana 2) 30_000_000
  offerOref2 <- A.txOffer (wallet 1) (banana 3) 50_000_000
  A.txSetDeadline (wallet 1) offerOref1 deadline1
  A.txSetDeadline (wallet 1) offerOref2 deadline2
  A.txBid (wallet 2) offerOref1 30_000_000
  A.txBid (wallet 3) offerOref2 50_000_000
  A.txBid (wallet 4) offerOref2 60_000_000
  awaitTime (deadline1 + 1)
  A.txHammer (wallet 1) offerOref1
  awaitTime (deadline2 + 1)
  A.txHammer (wallet 1) offerOref2

-- | helper function to compute what the given wallet owns in the
-- given state
holdingInState :: UtxoState -> Wallet -> Pl.Value
holdingInState (UtxoState m) w
  | Just vs <- M.lookup (walletAddress w) m = utxoValueSetTotal vs
  | otherwise = mempty

successfulSingle :: TestTree
successfulSingle =
  testGroup
    "Successful single-trace runs"
    [ testCase "hammer to withdraw" $ testSucceedsFrom testInit hammerToWithdraw,
      testCase "zero bids" $ testSucceedsFrom testInit noBids,
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

failingOffer :: MonadBlockChain m => m ()
failingOffer =
  void $
    A.txOffer (wallet 2) (banana 100) 20_000_000

forbiddenHammerToWithdraw :: MonadBlockChain m => m ()
forbiddenHammerToWithdraw = do
  offerOref <- A.txOffer (wallet 1) (banana 2) 30_000_000
  A.txHammer (wallet 2) offerOref

failingTwoBids :: MonadBlockChain m => m ()
failingTwoBids = do
  t0 <- currentTime
  let deadline = t0 + 60_000
  offerOref <- A.txOffer (wallet 1) (banana 2) 30_000_000
  A.txSetDeadline (wallet 1) offerOref deadline
  A.txBid (wallet 2) offerOref 30_000_000
  A.txBid (wallet 3) offerOref 30_000_000
  awaitTime (deadline + 1)
  A.txHammer (wallet 1) offerOref

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

-- * failing attacks

simpleTraces :: (Alternative m, MonadBlockChain m) => m ()
simpleTraces = noBids <|> oneBid <|> twoBids <|> twoAuctions

-- | Token duplication attack: Whenever we see a transaction that mints
-- something, try to mint one more token and pay it to the attacker. This should
-- be ruled out by the minting policy of the thread token.
tryDupTokens :: (Alternative m, MonadModalBlockChain m) => m ()
tryDupTokens =
  somewhere
    ( dupTokenAttack
        (\_ n -> n + 1) -- the modification of the minted value
        (wallet 6) -- the attacker's wallet
    )
    simpleTraces

-- | Datum hijacking attack: Try to steal outputs from a validator.
tryDatumHijack :: (Alternative m, MonadModalBlockChain m) => m ()
tryDatumHijack =
  somewhere
    ( datumHijackingAttack @A.Auction
        ( \(ConcreteOutput _ _ _ txSkelOutDatum _) -> case txSkelOutTypedDatum txSkelOutDatum of
            -- try to steal all outputs that have the 'Bidding' datum, no matter
            -- their validator or value.
            Just A.Bidding {} -> True
            -- try to steal during the 'SetDeadline' transaction. This
            -- vulnerability existed before PR #161.
            Just A.NoBids {} -> True
            _ -> False
        )
        (0 ==) -- if there is more than one 'Bidding' output, try stealing only the first
    )
    simpleTraces

-- | Double satisfaction attack. This attack tries to add extra 'Bid' inputs to
-- transactions that already 'Bid'.
tryDoubleSat :: (Alternative m, MonadModalBlockChain m) => m ()
tryDoubleSat =
  somewhere
    ( doubleSatAttack
        (txSkelInsL % folded)
        ( \redeemer ->
            case txSkelTypedRedeemer @A.Auction redeemer of
              Just (A.Bid (A.BidderInfo bid bidder)) -> do
                extraUtxos <- filteredUtxosWithDatums $ isScriptOutputFrom' A.auctionValidator
                return $
                  mapMaybe
                    ( \(oref, output) -> case output ^. outputDatumL of
                        ResolvedOrInlineDatum (A.NoBids seller minBid _deadline) ->
                          Just
                            ( Map.singleton oref $
                                TxSkelRedeemerForScript
                                  (A.Bid (A.BidderInfo minBid bidder)),
                              [],
                              mempty
                            )
                        ResolvedOrInlineDatum (A.Bidding seller _deadline (A.BidderInfo prevBid prevBidder)) ->
                          Just
                            ( Map.singleton oref $
                                TxSkelRedeemerForScript
                                  (A.Bid (A.BidderInfo (prevBid + 10_000_000) bidder)),
                              [],
                              mempty
                            )
                        _ -> Nothing
                    )
                    extraUtxos
              _ -> return []
        )
        -- pay the surplus to wallet 6
        (wallet 6)
        -- try each extra redeemer on a different modified transaction
        AllSeparate
    )
    simpleTraces

-- | datum tampering attack that tries to change the seller to wallet 6 on every
-- datum but 'Offer' (which is any time we pay to the 'auctionValidator' and
-- there are actual checks happening).
tryTamperDatum :: (Alternative m, MonadModalBlockChain m) => m ()
tryTamperDatum =
  somewhere
    ( tamperDatumTweak @A.AuctionState
        ( \case
            A.NoBids seller minBid deadline ->
              Just $ A.NoBids (walletPKHash $ wallet 6) minBid deadline
            A.Bidding seller deadline bidderInfo ->
              Just $ A.Bidding (walletPKHash $ wallet 6) deadline bidderInfo
            _ -> Nothing
        )
    )
    simpleTraces

failingAttacks :: TestTree
failingAttacks =
  testGroup
    "failing attacks"
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
      testCase "datum tampering" $
        testFailsFrom'
          isCekEvaluationFailure
          testInit
          tryTamperDatum,
      testCase "double satisfaction" $
        testFailsFrom'
          isCekEvaluationFailure
          testInit
          tryDoubleSat
    ]

-- * Known successful attacks and exploits

-- | Try to mint an additional token of the token name "exampleTokenName"
-- whenever anything is minted.
tryAddToken :: (Alternative m, MonadModalBlockChain m) => m ()
tryAddToken =
  somewhere
    ( addTokenAttack
        (const [(Pl.TokenName "exampleTokenName", NonZero 1)])
        (wallet 6)
    )
    simpleTraces

-- | This trace exploits the fact, discovered with the 'addTokenAttack' above,
-- that one can mint extra tokens on the 'SetDeadline' transaction, in order to
-- steal a bid from one auction with a separate auction.
exploitAddToken :: MonadModalBlockChain m => m ()
exploitAddToken = do
  -- Alice makes an offer (for a big amount of bananas).
  aliceOfferOref <- A.txOffer alice (banana 5) 50_000_000
  let aliceNftTokenName = A.tokenNameFromTxOutRef aliceOfferOref
      aliceNft = A.threadToken aliceOfferOref
  -- Eve sees Alice's offer and quickly makes an offer, for which she immediately
  -- sets the deadline. (As you can see, Eve can be very cheap, her offer
  -- contains nothing at all, and the minimum bid is 1 Lovelace!) On the
  -- 'setDeadline' transaction, she uses the fact that one can mint extra tokens
  -- in order to mint an extra token of Alice's auction's thread token asset
  -- class.
  eveOfferOref <- A.txOffer eve mempty 1
  t0 <- currentTime
  let eveDeadline = t0 + 60_000
  A.txSetDeadline eve eveOfferOref eveDeadline
    `withTweak` ( do
                    addMintTweak
                      ( Pl.Versioned A.threadTokenPolicy Pl.PlutusV2,
                        SomeMintsRedeemer eveOfferOref, -- Use the redeemer that is already on the transaction!
                        aliceNftTokenName,
                        NonZero 1
                      )
                    addOutputTweak $ paysPK (walletPKHash eve) aliceNft
                )

  -- Eve bids on her own offer, and also pays her forged NFT for Alice's auction
  -- on the same UTxO. This means that now there is a UTxO at the
  -- 'auctionValidator' which is identified by two NFTs: one for Alice's
  -- auction, and one for Eve's.
  A.txBid eve eveOfferOref 1
    `withTweak` overTweak
      ( singular $
          txSkelOutsL
            % traversed
            % txSkelOutOwnerTypeP @(Pl.TypedValidator A.Auction)
            % outputValueL
      )
      (<> aliceNft)

  -- Bob thinks he's bidding for Alice's auction (which is not even opened
  -- yet!). In fact, his money is put into Eve's auction.
  A.txBid bob aliceOfferOref 40_000_000

  -- After the deadline of Eve's auction, anyone can hammer it, and this will
  -- pay Bob's money to Eve, while Bob will only get the forged NFT in exchange.
  awaitTime eveDeadline
  A.txHammer eve eveOfferOref
  where
    alice = wallet 1
    bob = wallet 2
    eve = wallet 6

-- | This trace exploits the double satifsaction vulnerability in 'validBid' to
-- steal a bid. The idea is to bid on two auctions that currently have the same
-- highest bidder Bob, but only return one of the two bids to Bob in doing so.
--
-- The trace also illustrates that 'doubleSatAttack' is only as good as its
-- user, and also strongly depends on the variety of the scenarios described by
-- the 'simpleTraces'. We have here a double satisfaction scenario that simply
-- didn't come up as one of the cases tried by the 'doubleSatAttack' above.
exploitDoubleSat :: MonadModalBlockChain m => m ()
exploitDoubleSat = do
  -- Alice opens two auctions and sets the deadlines (it does not matter that
  -- they both belong to her, this vulnerability applies to any two auctions)
  offer1 <- A.txOffer alice (banana 2) 40_000_000
  offer2 <- A.txOffer alice (banana 3) 60_000_000
  t0 <- currentTime
  let t1 = t0 + 60_000
      t2 = t0 + 90_000
  A.txSetDeadline alice offer1 t1
  A.txSetDeadline alice offer2 t2
  -- People now bid on the auctions until Bob is the highest bidder on both
  -- auctions.
  A.txBid bob offer1 40_000_000
  A.txBid bob offer2 60_000_000
  -- The UTxO at the first auction that represents the current state:
  [(theLastBidOref, theLastBidOutput)] <-
    filteredUtxos $
      isScriptOutputFrom A.auctionValidator
        >=> isOutputWithValueSuchThat (`Pl.geq` A.threadToken offer1)
  -- Eve now bids on the second auction. Among other things this ensures that
  -- there's an output containing 40_000_000 Lovelace to Bob on the
  -- transaction. This means that, if she simultaneously bids on the first
  -- auction, she can fool the validator of the first auction, which expects an
  -- output of at least 20_000_000 Lovelace to go to Bob. She can keep this
  -- money to herself, effectively stealing Bob's bid on the first auction.
  A.txBid eve offer2 70_000_000
    `withTweak` ( do
                    overTweak txSkelValidityRangeL (`L.intersection` Pl.to (t1 - 1))
                    addInputTweak theLastBidOref $
                      TxSkelRedeemerForScript
                        (A.Bid $ A.BidderInfo 50_000_000 (walletPKHash eve))
                    addOutputTweak $
                      paysScript
                        A.auctionValidator
                        (A.Bidding (walletPKHash alice) t1 (A.BidderInfo 50_000_000 (walletPKHash eve)))
                        ( outputValue theLastBidOutput
                            <> Pl.negate (Ada.lovelaceValueOf 40_000_000) -- subtract Bob's bid
                            <> Ada.lovelaceValueOf 50_000_000 -- add Eve's bid
                        )
                    addOutputTweak $
                      paysPK
                        (walletPKHash eve)
                        (Ada.lovelaceValueOf 50_000_000)
                )
  -- Both auctions are closed normally. Eve is the highest bidder on both of
  -- them.
  awaitTime (t1 + 1)
  A.txHammer eve offer1
  awaitTime (t2 + 1)
  A.txHammer eve offer2
  where
    alice = wallet 1
    bob = wallet 2
    eve = wallet 6

successfulAttacks :: TestTree
successfulAttacks =
  testGroup "successful attacks and exploits" $
    map
      expectFail
      [ testCase "adding extra tokens" $
          testFailsFrom'
            isCekEvaluationFailure
            testInit
            tryAddToken,
        testCase "exploit extra tokens to steal a bid" $
          testFailsFrom'
            isCekEvaluationFailure
            testInit
            exploitAddToken,
        testCase "exploit double satisfaction to steal a bid" $
          testFailsFrom'
            isCekEvaluationFailure
            testInit
            exploitDoubleSat
      ]

-- * Comparing two outcomes with 'testBinaryRelatedBy'

-- Produce two outcomes, which differ only by who the (only) bidder in the
-- auction was. Then test that the sellers and buyers in both "worlds" have paid
-- the same amounts.

bidderAlternativeTrace :: (Alternative m, MonadBlockChain m) => m ()
bidderAlternativeTrace = do
  t0 <- currentTime
  let deadline = t0 + 60_000
  offerOref <- A.txOffer (wallet 1) (banana 2) 30_000_000
  A.txSetDeadline (wallet 1) offerOref deadline
  A.txBid (wallet 2) offerOref 30_000_000 <|> A.txBid (wallet 3) offerOref 30_000_000
  awaitTime (deadline + 1)
  A.txHammer (wallet 1) offerOref

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
      failingAttacks,
      miscTests,
      successfulAttacks
    ]
