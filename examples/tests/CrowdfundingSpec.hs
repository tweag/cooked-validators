{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module CrowdfundingSpec where

import qualified Crowdfunding as Cf
import qualified Crowdfunding.Offchain as Cf
import Control.Arrow
import Control.Applicative
import Control.Monad
import Cooked.Attack
import Cooked.Currencies
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Ledger as L
import qualified Ledger.Value as Value
import Test.Tasty
import Test.Tasty.HUnit

-- Just so we have something to fund that's not Ada:
-- Have a banana

bananaAssetClass :: Value.AssetClass
bananaAssetClass = permanentAssetClass "Banana"

-- | Value representing a number of bananas
banana :: Integer -> Value.Value
banana = Value.assetClassValue bananaAssetClass

-- | initial distribution s.t. all wallets own 5 bananas
testInit :: InitialDistribution
testInit = initialDistribution' [(wallet i, [minAda <> banana 5]) | i <- [1 .. 10]]

-- | Parameters of a crowdfund that is attempting to fund 5 bananas to wallet 2,
-- with a deadline in 60 seconds from the given time, and minimum contribution of
-- 2 bananas.
bananaParams :: L.POSIXTime -> Cf.ValParams
bananaParams t =
  Cf.ValParams
    { Cf.projectDeadline = t + 60_000,
      Cf.threshold = banana 5,
      Cf.minContribution = banana 2,
      Cf.fundingTarget = walletPKHash (wallet 2),
      Cf.rewardTokenAssetClass = Cf.getRewardTokenAssetClass $ walletPKHash (wallet 2)
    }

nothing :: MonadMockChain m => m ()
nothing = return ()

-- * Successful single-trace runs

-- These runs use the transactions from Crowdfunding.Offchain as they are
-- meant to be used.

-- | one contribution
oneContribution :: MonadMockChain m => m ()
oneContribution = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0)
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 3

-- | one contribution, refunded
oneContributionRefund :: MonadMockChain m => m ()
oneContributionRefund = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0)
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 3
  Cf.txRefund `as` wallet 3

-- | one contribution, project funded
oneContributionFund :: MonadMockChain m => m ()
oneContributionFund = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0)
  Cf.txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  Cf.txProjectFund (bananaParams t0) `as` wallet 2

-- | one contributer, multiple contributions, refunded
oneContributorRefund :: MonadMockChain m => m ()
oneContributorRefund = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0)
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 3
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 3
  Cf.txRefund `as` wallet 3

-- | one contributor, multiple contributions, project is funded
oneContributorFund :: MonadMockChain m => m ()
oneContributorFund = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0)
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 1
  Cf.txProjectFund (bananaParams t0) `as` wallet 2

-- | owner contributes, project is funded
ownerContributes :: MonadMockChain m => m ()
ownerContributes = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0)
  Cf.txIndividualFund (bananaParams t0) (banana 5) `as` wallet 2
  Cf.txProjectFund (bananaParams t0) `as` wallet 2

-- | owner refunds all contributors
ownerRefunds :: MonadMockChain m => m ()
ownerRefunds = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0)
  Cf.txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 4
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  Cf.txRefundAll (bananaParams t0) `as` wallet 2

-- | owner refunds all contributors
ownerRefundsSameContributor :: MonadMockChain m => m ()
ownerRefundsSameContributor = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0)
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  Cf.txRefundAll (bananaParams t0) `as` wallet 2

-- | two contributions, refunded
twoContributionsRefund :: MonadMockChain m => m ()
twoContributionsRefund = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0)
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  Cf.txRefund `as` wallet 1
  Cf.txRefund `as` wallet 3

-- | two contributions, funded
twoContributionsFund :: MonadMockChain m => m ()
twoContributionsFund = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0)
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  Cf.txProjectFund (bananaParams t0) `as` wallet 2

-- | multiple contributions, one refunded before project is funded
multipleContributionsOneRefunded :: MonadMockChain m => m ()
multipleContributionsOneRefunded = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0)
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  Cf.txIndividualFund (bananaParams t0) (banana 5) `as` wallet 4
  Cf.txRefund `as` wallet 3
  Cf.txProjectFund (bananaParams t0) `as` wallet 2

-- | many contributors, including some with multiple contributions. project is funded
manyContributorsFund :: MonadMockChain m => m ()
manyContributorsFund = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0)
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
  Cf.txOpen (bananaParams t0)
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
  Cf.txOpen (bananaParams t0)
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
        testSucceedsFrom testInit manyContributorsFund,
      testCase "many contributors, owner refunds" $
        testSucceedsFrom testInit manyContributorsOwnerRefunds,
      testCase "many contributors, some refund, project funded" $
        testSucceedsFrom testInit manyContributorsSomeRefundsFund
    ]

-- | one contribution, refund error: wallet 1 attempts to refund without contributing
oneContributionRefundError :: MonadMockChain m => m ()
oneContributionRefundError = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0)
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 3
  Cf.txRefund `as` wallet 1

-- | one contribution, project funding error: attempting to fund the project
-- when the threshold is not reached
oneContributionFundErrorAmount :: MonadMockChain m => m ()
oneContributionFundErrorAmount = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0)
  Cf.txIndividualFund (bananaParams t0) (banana 4) `as` wallet 1
  Cf.txProjectFund (bananaParams t0) `as` wallet 2

-- | one contribution, project funding error: attempting to fund the project
-- after the deadline
oneContributionFundErrorDeadline :: MonadMockChain m => m ()
oneContributionFundErrorDeadline = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0)
  Cf.txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  Cf.txProjectFund (bananaParams t0) `as` wallet 2

-- | attempting to refund all contributors before the deadline
ownerRefundsErrorDeadline :: MonadMockChain m => m ()
ownerRefundsErrorDeadline = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0)
  Cf.txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  Cf.txIndividualFund (bananaParams t0) (banana 3) `as` wallet 4
  Cf.txRefundAll (bananaParams t0) `as` wallet 2

-- | two contributions, error: one contribution does not exceed minimum
twoContributionsFundErrorMinimum :: MonadMockChain m => m ()
twoContributionsFundErrorMinimum = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0)
  Cf.txIndividualFund (bananaParams t0) (banana 1) `as` wallet 1
  Cf.txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  Cf.txProjectFund (bananaParams t0) `as` wallet 2

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
        testFailsFrom testInit twoContributionsFundErrorMinimum
    ]

-- | one contribution, project funding error: wallet 1 attempts to fund project
-- when wallet 2 is the funding target. Funds stay locked in the script
oneContributionFundErrorOwner :: MonadMockChain m => m ()
oneContributionFundErrorOwner = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0)
  Cf.txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  Cf.txProjectFund (bananaParams t0) `as` wallet 1

-- | wallet 1 attempts to refund all contributors with wallet 2 as owner
-- funds are locked in the script
ownerRefundsErrorOwner :: MonadMockChain m => m ()
ownerRefundsErrorOwner = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0)
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
    (oneContributionFund <|> twoContributionsFund <|> manyContributorsFund)

tryDatumHijack :: (Alternative m, MonadModalMockChain m) => m ()
tryDatumHijack =
  somewhere
    ( datumHijackingAttack @Cf.Crowdfunding
        ( \_ d _ -> case d of -- try to steal all outputs that have the 'Funding' datum, no matter their validator or value
            Cf.Funding {} -> True
            _ -> False
        )
        (0 ==) -- if there is more than one 'Funding' output, try stealing only the first
    )
    (oneContributionFund <|> twoContributionsFund <|> manyContributorsFund)

-- Produce two outcomes, which differ only by who the (only) contributor in
-- the auction was. Then test that the owner and contributors in both
-- "worlds" have paid the same amounts.

-- | helper function to compute what the given wallet owns in the
-- given state
holdingInState :: UtxoState -> Wallet -> L.Value
holdingInState (UtxoState m) w
  | Just vs <- M.lookup (walletAddress w) m = utxoValueSetTotal vs
  | otherwise = mempty

oneContributionFundAlternativeTrace :: (Alternative m, MonadMockChain m) => m ()
oneContributionFundAlternativeTrace = do
  t0 <- currentTime
  Cf.txOpen (bananaParams t0)
  Cf.txIndividualFund (bananaParams t0) (banana 5) `as` wallet 9
    <|> Cf.txIndividualFund (bananaParams t0) (banana 5) `as` wallet 10
  Cf.txProjectFund (bananaParams t0) `as` wallet 2

oneContributionFundAlternative :: TestTree
oneContributionFundAlternative =
  testCase "change in possessions independent of contributor" $
    testBinaryRelatedBy
      ( \a b ->
          testBool $
            holdingInState a (wallet 2) == holdingInState b (wallet 2)
              && holdingInState a (wallet 9) == holdingInState b (wallet 10)
      )
      testInit
      oneContributionFundAlternativeTrace

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
          isCekEvaluationFailure
          testInit
          tryDupTokens,
      testCase "datum hijacking" $
        testFailsFrom'
          isCekEvaluationFailure
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
