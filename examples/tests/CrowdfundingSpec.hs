{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module CrowdfundingSpec where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Cooked.Attack
import Cooked.Currencies
import Cooked.Ltl
import Cooked.MockChain
import Cooked.Tx.Constraints
import qualified Crowdfunding as Cf
import qualified Crowdfunding.Offchain as Cf
import Data.Default
import Data.List (isPrefixOf, (\\))
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Ledger as L
import qualified Ledger.Value as Value
import qualified Plutus.V1.Ledger.Ada as Ada
import Test.Tasty
import Test.Tasty.HUnit

-- Just so we have something to fund that's not Ada:
-- Have a banana and an apple

bananaAssetClass :: Value.AssetClass
bananaAssetClass = permanentAssetClass "Banana"

-- | Value representing a number of bananas
banana :: Integer -> Value.Value
banana = Value.assetClassValue bananaAssetClass

-- | Parameters of a crowdfund that is attempting to fund 5 bananas to wallet 2,
-- with a deadline in 60 seconds from the given time, and minimum contribution of
-- 2 bananas.
bananaParams :: L.POSIXTime -> Cf.ValParams
bananaParams t =
  Cf.ValParams
    { Cf.projectDeadline = t + 60_000,
      Cf.threshold = banana 5,
      Cf.minContribution = banana 2,
      Cf.fundingTarget = walletPKHash (wallet 2)
    }

-- appleAssetClass :: Value.AssetClass
-- appleAssetClass = permanentAssetClass "Apple"

-- -- | Value representing a number of apples
-- apple :: Integer -> Value.Value
-- apple = Value.assetClassValue appleAssetClass

-- -- | Parameters of a crowdfund that is attempting to fund 5 apples to wallet 2,
-- -- with a deadline in 60 seconds from the given time, and minimum contribution of
-- -- 2 apples.
-- appleParams :: L.POSIXTime -> Cf.ValParams
-- appleParams t =
--   Cf.ValParams
--     { Cf.projectDeadline = t + 120_000,
--       Cf.threshold = apple 4,
--       Cf.minContribution = apple 2,
--       Cf.fundingTarget = walletPKHash (wallet 2)
--     }

-- | initial distribution s.t. all wallets own 5 bananas and 5 apples
testInit :: InitialDistribution
testInit = initialDistribution' [(wallet i, [minAda <> banana 5]) | i <- [1 .. 10]]

nothing :: MonadMockChain m => m ()
nothing = return ()

-- * Successful single-trace runs

-- These runs use the transactions from Crowdfunding.Offchain as they are
-- meant to be used.

-- | one contribution
oneContribution :: MonadMockChain m => m ()
oneContribution = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 3

-- | one contribution, refunded
oneContributionRefund :: MonadMockChain m => m ()
oneContributionRefund = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 3
  Cf.txRefund `as` wallet 3

-- | one contribution, project funded
oneContributionFund :: MonadMockChain m => m ()
oneContributionFund = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  Cf.txProjectFund (bananaParams t0) `as` wallet 2

-- | one contributer, multiple contributions, refunded
oneContributorRefund :: MonadMockChain m => m ()
oneContributorRefund = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 3
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 3
  Cf.txRefund `as` wallet 3

-- | one contributor, multiple contributions, project is funded
oneContributorFund :: MonadMockChain m => m ()
oneContributorFund = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 1
  Cf.txProjectFund (bananaParams t0) `as` wallet 2

-- | owner contributes, project is funded
ownerContributes :: MonadMockChain m => m ()
ownerContributes = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 5) `as` wallet 2
  Cf.txProjectFund (bananaParams t0) `as` wallet 2

-- | owner refunds all contributors
ownerRefunds :: MonadMockChain m => m ()
ownerRefunds = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 4
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  Cf.txRefundAll (bananaParams t0) `as` wallet 2

-- | owner refunds all contributors
ownerRefundsSameContributor :: MonadMockChain m => m ()
ownerRefundsSameContributor = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  Cf.txRefundAll (bananaParams t0) `as` wallet 2

-- | two contributions, refunded
twoContributionsRefund :: MonadMockChain m => m ()
twoContributionsRefund = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  Cf.txRefund `as` wallet 1
  Cf.txRefund `as` wallet 3

-- | two contributions, funded
twoContributionsFund :: MonadMockChain m => m ()
twoContributionsFund = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  Cf.txProjectFund (bananaParams t0) `as` wallet 2

-- | multiple contributions, one refunded before project is funded
multipleContributionsOneRefunded :: MonadMockChain m => m ()
multipleContributionsOneRefunded = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  Cf.txIndividualFund (bananaParams t0) (banana 5) `as` wallet 4
  Cf.txRefund `as` wallet 3
  Cf.txProjectFund (bananaParams t0) `as` wallet 2

-- | many contributors, including some with multiple contributions. project is funded
manyContributorsFund :: MonadMockChain m => m ()
manyContributorsFund = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 3
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 4
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 5
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 3
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 4
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 5
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 6
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 7
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 8
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  Cf.txProjectFund (bananaParams t0) `as` wallet 2

-- | many contributors, including some with multiple contributions. owner refunds all
manyContributorsOwnerRefunds :: MonadMockChain m => m ()
manyContributorsOwnerRefunds = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 3
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 4
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 5
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 3
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 4
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 5
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 6
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 7
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 8
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  Cf.txRefundAll (bananaParams t0) `as` wallet 2

-- | many contributors, including some with multiple contributions. some individual
-- refunds. project is ultimately funded
manyContributorsSomeRefundsFund :: MonadMockChain m => m ()
manyContributorsSomeRefundsFund = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 3
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 4
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 5
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 3
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 4
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 5
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 6
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 7
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 8
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  Cf.txRefund `as` wallet 3
  Cf.txRefund `as` wallet 4
  Cf.txRefund `as` wallet 8
  Cf.txProjectFund (bananaParams t0) `as` wallet 2

-- | one contribution, refunding when contribution does not exceed minimum
oneContributionRefundBelowMinimum :: MonadMockChain m => m ()
oneContributionRefundBelowMinimum = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 1) `as` wallet 3
  Cf.txRefund `as` wallet 3

-- | owner refunds all contributors when one contribution does not
-- exceed the minimum contribution
ownerRefundsBelowMinimum :: MonadMockChain m => m ()
ownerRefundsBelowMinimum = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 1) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 4
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  Cf.txRefundAll (bananaParams t0) `as` wallet 2

{-
-- | wallet 2 opens two crowdfunds at the same time
twoCrowdfunds :: MonadMockChain m => m ()
twoCrowdfunds = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txOpen (appleParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 3
  --Cf.txIndividualFund (appleParams t0) (apple 2) `as` wallet 1
  --Cf.txIndividualFund (appleParams t0) (apple 2) `as` wallet 4
  Cf.txProjectFund (bananaParams t0) `as` wallet 2
  --Cf.txProjectFund (appleParams t0) `as` wallet 2
-}

successfulSingle :: TestTree
successfulSingle =
  testGroup
    "Successful single-trace runs"
    [ testCase "one contribution" $ testSucceedsFrom testInit oneContribution,
      testCase "one contribution, refunded" $ testSucceedsFrom testInit oneContributionRefund,
      testCase "one contribution, project funded" $
        testSucceedsFrom testInit oneContributionFund,
      testCase "one contributor, multiple contributions, refunded" $
        testSucceedsFrom testInit oneContributorRefund,
      testCase "one contributor, multiple contributions, project funded" $
        testSucceedsFrom testInit oneContributorRefund,
      testCase "owner contributes" $ testSucceedsFrom testInit ownerContributes,
      testCase "owner refunds" $ testSucceedsFrom testInit ownerRefunds,
      testCase "owner refunds multiple contributions by same contributor" $
        testSucceedsFrom testInit ownerRefundsSameContributor,
      testCase "two contributions, refunded" $
        testSucceedsFrom testInit twoContributionsRefund,
      testCase "two contributions, project funded" $
        testSucceedsFrom testInit twoContributionsFund,
      testCase "multiple contributions with one refund, project is funded" $
        testSucceedsFrom testInit multipleContributionsOneRefunded,
      testCase "many contributors, project funded" $
        testSucceedsFrom testInit (allowBigTransactions manyContributorsFund),
      testCase "many contributors, owner refunds" $
        testSucceedsFrom testInit manyContributorsOwnerRefunds,
      testCase "many contributors, some refund, project funded" $
        testSucceedsFrom testInit (allowBigTransactions manyContributorsSomeRefundsFund),
      testCase "one contribution not exceeding minimum, refunded" $
        testSucceedsFrom testInit oneContributionRefundBelowMinimum,
      testCase "owner refunds, one contribution not exceeding minimum" $
        testSucceedsFrom testInit ownerRefundsBelowMinimum
        -- testCase "two crowdfunds at the same time" $
        --   testSucceedsFrom testInit (allowBigTransactions twoCrowdfunds)
    ]

-- | one contribution, refund error: wallet 1 attempts to refund without contributing
oneContributionRefundError :: MonadMockChain m => m ()
oneContributionRefundError = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 3
  Cf.txRefund `as` wallet 1

-- | one contribution, project funding error: attempting to fund the project
-- when the threshold is not reached
oneContributionFundErrorAmount :: MonadMockChain m => m ()
oneContributionFundErrorAmount = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 4) `as` wallet 1
  Cf.txProjectFund (bananaParams t0) `as` wallet 2

-- | one contribution, project funding error: attempting to fund the project
-- after the deadline
oneContributionFundErrorDeadline :: MonadMockChain m => m ()
oneContributionFundErrorDeadline = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  Cf.txProjectFund (bananaParams t0) `as` wallet 2

-- | attempting to refund all contributors before the deadline
ownerRefundsErrorDeadline :: MonadMockChain m => m ()
ownerRefundsErrorDeadline = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 4
  Cf.txRefundAll (bananaParams t0) `as` wallet 2

-- | two contributions, error: one contribution does not exceed minimum
twoContributionsFundErrorMinimum :: MonadMockChain m => m ()
twoContributionsFundErrorMinimum = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 1) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  Cf.txProjectFund (bananaParams t0) `as` wallet 2

-- | owner attempts to pay self all funds after the deadline, with no tokens
-- minted for the contributors
ownerRefundsVulnerability :: MonadMockChain m => m ()
ownerRefundsVulnerability = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 4
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  Cf.txRefundAllVulnerability (bananaParams t0) `as` wallet 2

failingSingle :: TestTree
failingSingle =
  testGroup
    "Single-trace runs that are expected to fail"
    [ testCase "refunding without contributing" $
        testFailsFrom testInit oneContributionRefundError,
      testCase "threshold not reached" $
        testFailsFrom testInit oneContributionFundErrorAmount,
      testCase "funding after the deadline" $
        testFailsFrom testInit oneContributionFundErrorDeadline,
      testCase "owner refunds before the deadline" $
        testFailsFrom testInit ownerRefundsErrorDeadline,
      testCase "contribution does not exceed minimum" $
        testFailsFrom testInit twoContributionsFundErrorMinimum,
      testCase "owner pays self all funds after the deadline" $
        testFailsFrom testInit ownerRefundsVulnerability
    ]

-- | one contribution, project funding error: wallet 1 attempts to fund project
-- when wallet 2 is the funding target. Funds stay locked in the script
-- TODO: make this throw an error
oneContributionFundErrorOwner :: MonadMockChain m => m ()
oneContributionFundErrorOwner = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  Cf.txProjectFund (bananaParams t0) `as` wallet 1

-- | wallet 1 attempts to refund all contributors with wallet 2 as owner
-- funds are locked in the script
-- TODO: make this throw an error
ownerRefundsErrorOwner :: MonadMockChain m => m ()
ownerRefundsErrorOwner = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 4
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  Cf.txRefundAll (bananaParams t0) `as` wallet 1

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
    (oneContributionFund <|> twoContributionsFund <|> allowBigTransactions manyContributorsFund)

-- Produce two outcomes, which differ only by who the (only) contributor in
-- the auction was. Then test that the owner and contributors in both
-- "worlds" have paid the same amounts.

-- | helper function to compute what the given wallet owns in ada in the
-- current state
adaInState :: UtxoState -> Wallet -> L.Ada
adaInState (UtxoState m) w
  | Just vs <- M.lookup (walletAddress w) m = Ada.fromValue $ utxoValueSetTotal vs
  | otherwise = mempty

-- | Helper function to compute how many tokens of each asset class the wallet
-- owns in the current state. Note the token names will be different in the
-- different alternatives as they are computed as the hash of a UTxO, so we
-- only test for equal amounts here.
tokensInState :: UtxoState -> Wallet -> [Integer]
tokensInState (UtxoState m) w
  | Just vs <- M.lookup (walletAddress w) m =
    map third $ Value.flattenValue $ utxoValueSetTotal vs
  | otherwise = []
  where
    third (_, _, x) = x

oneContributionFundAlternativeTrace :: (Alternative m, MonadMockChain m) => m ()
oneContributionFundAlternativeTrace = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0) `as` wallet 2
  Cf.txIndividualFund (bananaParams t0) (banana 5) `as` wallet 9
    <|> Cf.txIndividualFund (bananaParams t0) (banana 5) `as` wallet 10
  Cf.txProjectFund (bananaParams t0) `as` wallet 2

oneContributionFundAlternative :: TestTree
oneContributionFundAlternative =
  testCase "change in possessions independent of contributor" $
    testBinaryRelatedBy
      ( \a b ->
          testBool $
            adaInState a (wallet 2) == adaInState b (wallet 2)
              && tokensInState a (wallet 2) `setEquals` tokensInState b (wallet 2)
              && adaInState a (wallet 9) == adaInState b (wallet 10)
              && tokensInState a (wallet 9) `setEquals` tokensInState b (wallet 10)
      )
      testInit
      oneContributionFundAlternativeTrace
  where
    setEquals xs ys = null (xs \\ ys) && null (ys \\ xs)

-- * Collecting all the tests in this module

miscTests :: TestTree
miscTests =
  testGroup
    "Miscellaneuos tests"
    [oneContributionFundAlternative]

attacks :: TestTree
attacks =
  testGroup
    "Attacks"
    [ testCase "token duplication" $
        testFailsFrom'
          ( isCekEvaluationFailureWithMsg
              (\msg -> "not minting the right amount" `isPrefixOf` msg)
          )
          testInit
          tryDupTokens
    ]

tests :: TestTree
tests =
  testGroup
    "CrowdfundingSpec"
    [ successfulSingle,
      failingSingle,
      attacks,
      miscTests
    ]
