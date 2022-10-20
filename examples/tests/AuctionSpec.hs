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
import Data.Maybe
import qualified Ledger as L
import qualified Ledger.Ada as Ada
import qualified Ledger.Value as Pl
import qualified Ledger.Value as Value
import Optics.Core
import qualified Plutus.Script.Utils.V1.Scripts as Pl
import qualified PlutusTx.Numeric as Pl
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

-- * failing attacks

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

-- * Known successful attacks and exploits

-- | This attack adds extra tokens, depending on the minting policy. It goes
-- through all 'Mints' constraints on the transaction, looking at all the
-- policies involved.
--
-- This attack adds an 'AddTokenLbl' with the token name of the additional
-- minted token(s). It returns additional value minted.
addTokenAttack ::
  -- | For each policy that occurs in some 'Mints' constraint, return a list of
  -- token names together with how many tokens with that name be minted, using
  -- the same redeemer as on the original transaction. For each of the elements
  -- of the returned list, one modified transaction will be tried.
  (Pl.MintingPolicy -> [(Pl.TokenName, Integer)]) ->
  -- | The wallet of the attacker. Any extra tokens will be paid to this wallet.
  Wallet ->
  Tweak Pl.Value
addTokenAttack extraTokens attacker = do
  mints <- viewTweak $ partsOf mintsConstraintsT
  msum $
    map
      ( \(MintsConstraint redeemer pols _value) ->
          msum $
            map
              ( \pol ->
                  msum $
                    map
                      ( \(extraTn, amount) ->
                          let extraValue = Pl.assetClassValue (Pl.assetClass (Pl.scriptCurrencySymbol pol) extraTn) amount
                           in do
                                addMiscConstraintTweak $ Mints redeemer [pol] extraValue
                                addLabelTweak $ AddTokenLbl extraTn
                                addOutConstraintTweak $ paysPK (walletPKHash attacker) extraValue
                                return extraValue
                      )
                      $ extraTokens pol
              )
              pols
      )
      mints

newtype AddTokenLbl = AddTokenLbl Pl.TokenName deriving (Show, Eq)

-- | Apply the given tweak to the first tranaction in de given trace.
withTweak :: MonadModalMockChain m => m x -> Tweak a -> m x
withTweak trace tweak = modifyLtl (LtlAtom $ UntypedTweak tweak) trace

-- | Try to mint an additional token of the token name "exampleTokenName"
-- whenever anything is minted.
tryAddToken :: (Alternative m, MonadModalMockChain m) => m ()
tryAddToken =
  somewhere
    ( addTokenAttack
        (const [(Pl.TokenName "exampleTokenName", 1)])
        (wallet 6)
    )
    simpleTraces

-- | This trace exploits the fact, discovered with the 'addTokenAttack' above,
-- that one can mint extra tokens on the 'SetDeadline' transaction, in order to
-- steal a bid from one auction with a separate auction.
exploitAddToken :: MonadModalMockChain m => m ()
exploitAddToken = do
  -- Alice makes an offer (for a big amount of bananas).
  aliceOffer <- A.txOffer (banana 5) 50_000_000 `as` alice
  let aliceNft = A.threadToken $ fst aliceOffer
  -- Eve sees alices offer and quickly makes an offer, for which she immediately
  -- sets the deadline. (As you can see, eve can be very cheap, her offer
  -- contains nothing at all, and the minimum bid is 1 Lovelace!) On the
  -- 'setDeadline' transaction, she uses the fact that one can mint extra tokens
  -- in order to mint an extra token of alice's auction's thread token asset
  -- class.
  eveOffer <- A.txOffer mempty 1 `as` eve
  t0 <- currentTime
  let eveDeadline = t0 + 60_000
  A.txSetDeadline eveOffer eveDeadline `as` eve
    `withTweak` addConstraintsTweak
      ( [Mints (Just $ fst aliceOffer) [A.threadTokenPolicy] aliceNft]
          :=>: [paysPK (walletPKHash eve) aliceNft]
      )

  -- Eve bids on her own offer, and also pays her forged NFT for Alice's auction
  -- on the same UTxO. This means that now there is a UTxO at the
  -- 'auctionValidator' which is identified by two NFTs: one for Alice's
  -- auction, and one for Eve's.
  A.txBid eveOffer 1 `as` eve
    `withTweak` overTweak
      (singular paysScriptConstraintsT)
      ( \(PaysScriptConstraint validator staking datum value) ->
          PaysScriptConstraint validator staking datum (value <> aliceNft)
      )

  -- Bob thinks he's bidding for Alice's auction (which is not even opened
  -- yet!). In fact, his money is put into Eve's auction.
  A.txBid aliceOffer 40_000_000 `as` bob

  -- After the deadline of Eve's auction, anyone can hammer it, and this will
  -- pay Bob's money to Eve, while Bob will only get the forged NFT in exchange.
  awaitTime eveDeadline
  A.txHammer eveOffer
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
        testCase "exploit extra tokens" $
          testFailsFrom'
            isCekEvaluationFailure
            testInit
            exploitAddToken
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
      failingAttacks,
      miscTests,
      successfulAttacks
    ]
