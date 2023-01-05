{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module CrowdfundingSpec where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Cooked.Attack
import Cooked.Currencies
import Cooked.Ltl
import Cooked.MockChain
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints
import qualified Crowdfunding as Cf
import qualified Crowdfunding.Offchain as Cf
import Data.Default
import Data.List (isPrefixOf, (\\))
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Ledger as L
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import qualified Plutus.Script.Utils.V1.Scripts as Pl
import qualified Plutus.V1.Ledger.Ada as Ada
import Test.Tasty
import Test.Tasty.HUnit

-- adding names to wallets

alice = wallet 1

bob = wallet 2

charlie = wallet 3

dylan = wallet 4

eve = wallet 5

fred = wallet 6

greta = wallet 7

hank = wallet 8

iris = wallet 9

james = wallet 10

-- Just so we have something to fund that's not Ada:
-- Have a banana and an apple

bananaAssetClass :: Value.AssetClass
bananaAssetClass = permanentAssetClass "Banana"

-- | Value representing a number of bananas
banana :: Integer -> Value.Value
banana = Value.assetClassValue bananaAssetClass

-- | Parameters of a crowdfund that is attempting to fund 5 bananas to bob,
-- with a deadline in 60 seconds from the given time, and minimum contribution of
-- 2 bananas.
bananaParams :: L.POSIXTime -> Cf.ValParams
bananaParams t =
  Cf.ValParams
    { Cf.projectDeadline = t + 60_000,
      Cf.threshold = banana 5,
      Cf.minContribution = banana 2,
      Cf.fundingTarget = walletPKHash bob,
      Cf.threadCS = Pl.scriptCurrencySymbol Cf.threadTokenPolicy
    }

appleAssetClass :: Value.AssetClass
appleAssetClass = permanentAssetClass "Apple"

-- | Value representing a number of apples
apple :: Integer -> Value.Value
apple = Value.assetClassValue appleAssetClass

-- | Parameters of a crowdfund that is attempting to fund 5 apples to bob,
-- with a deadline in 60 seconds from the given time, and minimum contribution of
-- 2 apples.
appleParams :: L.POSIXTime -> Cf.ValParams
appleParams t =
  Cf.ValParams
    { Cf.projectDeadline = t + 120_000,
      Cf.threshold = apple 4,
      Cf.minContribution = apple 2,
      Cf.fundingTarget = walletPKHash bob,
      Cf.threadCS = Pl.scriptCurrencySymbol Cf.threadTokenPolicy
    }

-- | initial distribution s.t. all wallets own 5 bananas and 5 apples
testInit :: InitialDistribution
testInit = initialDistribution' [(wallet i, [minAda <> banana 5 <> apple 5]) | i <- [1 .. 10]]

nothing :: MonadMockChain m => m ()
nothing = return ()

-- * Successful single-trace runs

-- These runs use the transactions from Crowdfunding.Offchain as they are
-- meant to be used.

-- | one contribution
oneContribution :: MonadMockChain m => m ()
oneContribution = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txIndividualFund (banana 3) sOut `as` charlie

-- | one contribution, refunded
oneContributionRefund :: MonadMockChain m => m ()
oneContributionRefund = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 3) sOut `as` charlie
  Cf.txRefund `as` charlie

-- | one contribution, project funded
oneContributionFund :: MonadMockChain m => m ()
oneContributionFund = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 5) sOut `as` alice
  Cf.txProjectFund (bananaParams t0) sOut `as` bob

-- | one contributer, multiple contributions, refunded
oneContributorRefund :: MonadMockChain m => m ()
oneContributorRefund = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 3) sOut `as` charlie
  Cf.txIndividualFund (banana 2) sOut `as` charlie
  Cf.txRefund `as` charlie

-- | one contributor, multiple contributions, project is funded
oneContributorFund :: MonadMockChain m => m ()
oneContributorFund = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 2) sOut `as` alice
  Cf.txIndividualFund (banana 3) sOut `as` alice
  Cf.txProjectFund (bananaParams t0) sOut `as` bob

-- | owner contributes, project is funded
ownerContributes :: MonadMockChain m => m ()
ownerContributes = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 5) sOut `as` bob
  Cf.txProjectFund (bananaParams t0) sOut `as` bob

-- | owner refunds all contributors
ownerRefunds :: MonadMockChain m => m ()
ownerRefunds = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 5) sOut `as` alice
  Cf.txIndividualFund (banana 4) sOut `as` charlie
  Cf.txIndividualFund (banana 3) sOut `as` dylan
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  Cf.txRefundAll (bananaParams t0) sOut `as` bob

-- | owner refunds all contributors
ownerRefundsSameContributor :: MonadMockChain m => m ()
ownerRefundsSameContributor = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 2) sOut `as` alice
  Cf.txIndividualFund (banana 2) sOut `as` alice
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  Cf.txRefundAll (bananaParams t0) sOut `as` bob

-- | two contributions, refunded
twoContributionsRefund :: MonadMockChain m => m ()
twoContributionsRefund = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 3) sOut `as` alice
  Cf.txIndividualFund (banana 4) sOut `as` charlie
  Cf.txRefund `as` alice
  Cf.txRefund `as` charlie

-- | two contributions, funded
twoContributionsFund :: MonadMockChain m => m ()
twoContributionsFund = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 3) sOut `as` alice
  Cf.txIndividualFund (banana 4) sOut `as` charlie
  Cf.txProjectFund (bananaParams t0) sOut `as` bob

-- | multiple contributions, one refunded before project is funded
multipleContributionsOneRefunded :: MonadMockChain m => m ()
multipleContributionsOneRefunded = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 3) sOut `as` alice
  Cf.txIndividualFund (banana 4) sOut `as` charlie
  Cf.txIndividualFund (banana 5) sOut `as` dylan
  Cf.txRefund `as` charlie
  Cf.txProjectFund (bananaParams t0) sOut `as` bob

-- | many contributors, including some with multiple contributions. project is funded
manyContributorsFund :: MonadMockChain m => m ()
manyContributorsFund = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 2) sOut `as` alice
  Cf.txIndividualFund (banana 2) sOut `as` charlie
  Cf.txIndividualFund (banana 2) sOut `as` dylan
  Cf.txIndividualFund (banana 2) sOut `as` eve
  Cf.txIndividualFund (banana 2) sOut `as` charlie
  Cf.txIndividualFund (banana 2) sOut `as` dylan
  Cf.txIndividualFund (banana 2) sOut `as` eve
  Cf.txIndividualFund (banana 2) sOut `as` fred
  Cf.txIndividualFund (banana 2) sOut `as` greta
  Cf.txIndividualFund (banana 2) sOut `as` alice
  Cf.txProjectFund (bananaParams t0) sOut `as` bob

-- | many contributors, including some with multiple contributions. owner refunds all
manyContributorsOwnerRefunds :: MonadMockChain m => m ()
manyContributorsOwnerRefunds = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 2) sOut `as` alice
  Cf.txIndividualFund (banana 2) sOut `as` charlie
  Cf.txIndividualFund (banana 2) sOut `as` dylan
  Cf.txIndividualFund (banana 2) sOut `as` eve
  Cf.txIndividualFund (banana 2) sOut `as` charlie
  Cf.txIndividualFund (banana 2) sOut `as` dylan
  Cf.txIndividualFund (banana 2) sOut `as` eve
  Cf.txIndividualFund (banana 2) sOut `as` fred
  Cf.txIndividualFund (banana 2) sOut `as` greta
  Cf.txIndividualFund (banana 2) sOut `as` hank
  Cf.txIndividualFund (banana 2) sOut `as` alice
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  Cf.txRefundAll (bananaParams t0) sOut `as` bob

-- | many contributors, including some with multiple contributions. some individual
-- refunds. project is ultimately funded
manyContributorsSomeRefundsFund :: MonadMockChain m => m ()
manyContributorsSomeRefundsFund = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 2) sOut `as` alice
  Cf.txIndividualFund (banana 2) sOut `as` charlie
  Cf.txIndividualFund (banana 2) sOut `as` dylan
  Cf.txIndividualFund (banana 2) sOut `as` eve
  Cf.txIndividualFund (banana 2) sOut `as` charlie
  Cf.txIndividualFund (banana 2) sOut `as` dylan
  Cf.txIndividualFund (banana 2) sOut `as` eve
  Cf.txIndividualFund (banana 2) sOut `as` fred
  Cf.txIndividualFund (banana 2) sOut `as` greta
  Cf.txIndividualFund (banana 2) sOut `as` hank
  Cf.txIndividualFund (banana 2) sOut `as` alice
  Cf.txRefund `as` charlie
  Cf.txRefund `as` dylan
  Cf.txRefund `as` hank
  Cf.txProjectFund (bananaParams t0) sOut `as` bob

-- | one contribution, refunding when contribution does not exceed minimum
oneContributionRefundBelowMinimum :: MonadMockChain m => m ()
oneContributionRefundBelowMinimum = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 1) sOut `as` charlie
  Cf.txRefund `as` charlie

-- | owner refunds all contributors when one contribution does not
-- exceed the minimum contribution
ownerRefundsBelowMinimum :: MonadMockChain m => m ()
ownerRefundsBelowMinimum = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 1) sOut `as` alice
  Cf.txIndividualFund (banana 4) sOut `as` charlie
  Cf.txIndividualFund (banana 3) sOut `as` dylan
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  Cf.txRefundAll (bananaParams t0) sOut `as` bob

-- | bob opens two crowdfunds at the same time
twoCrowdfunds :: MonadMockChain m => m ()
twoCrowdfunds = do
  t0 <- currentTime
  sOutB <- Cf.txOpen (bananaParams t0) `as` bob
  sOutA <- Cf.txOpen (appleParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOutB `as` bob
  Cf.txMintThreadToken (appleParams t0) sOutA `as` bob
  Cf.txIndividualFund (banana 3) sOutB `as` alice
  Cf.txIndividualFund (banana 2) sOutB `as` charlie
  Cf.txIndividualFund (apple 2) sOutA `as` alice
  Cf.txIndividualFund (apple 2) sOutA `as` dylan
  Cf.txProjectFund (bananaParams t0) sOutB `as` bob
  Cf.txProjectFund (appleParams t0) sOutA `as` bob

-- | launch a crowdfund without consuming all funding utxos
partialCrowdfund :: MonadModalMockChain m => m ()
partialCrowdfund = do
  let evePKH = walletPKHash eve
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 2) sOut `as` alice
  Cf.txIndividualFund (banana 2) sOut `as` charlie
  Cf.txIndividualFund (banana 2) sOut `as` dylan
  Cf.txIndividualFund (banana 3) sOut `as` eve
  Cf.txProjectFund (bananaParams t0) sOut `as` bob
    `withTweak` do
      mintOneLessTweak (fst sOut)
      removeMiscConstraintsTweak
        ( \case
            mc@(SpendsScript _ _ out) | sOutValue out `Value.geq` banana 3 -> Just mc
            _ -> Nothing
        )
      removeOutConstraintsTweak
        ( \case
            oc@(PaysPKWithDatum pkh _ _ _) | pkh == evePKH -> Just oc
            _ -> Nothing
        )

  Cf.txRefund `as` eve
  where
    mintOneLessTweak txOut = do
      let mp = [Cf.rewardTokenPolicy txOut]
          ac = Cf.getRewardTokenAssetClass txOut
      [amount] <-
        removeMiscConstraintsTweak
          ( \case
              mc@(Mints _ pol tok) | pol == mp -> Just $ Value.assetClassValueOf tok ac
              _ -> Nothing
          )
      addMintsTweak
        (Just (Scripts.validatorAddress Cf.crowdfundingValidator))
        mp
        (Cf.rewardToken txOut (amount - 1))

successfulSingle :: TestTree
successfulSingle =
  testGroup
    "Successful single-trace runs"
    [ testCase "one contribution" $ testSucceedsFrom testInit oneContribution,
      testCase "one contribution, refunded" $ testSucceedsFrom testInit oneContributionRefund,
      testCase "one contribution, project funded" $
        testSucceedsFrom testInit (allowBigTransactions oneContributionFund),
      testCase "one contributor, multiple contributions, refunded" $
        testSucceedsFrom testInit oneContributorRefund,
      testCase "one contributor, multiple contributions, project funded" $
        testSucceedsFrom testInit oneContributorRefund,
      testCase "owner contributes" $
        testSucceedsFrom testInit (allowBigTransactions ownerContributes),
      testCase "owner refunds" $ testSucceedsFrom testInit ownerRefunds,
      testCase "owner refunds multiple contributions by same contributor" $
        testSucceedsFrom testInit ownerRefundsSameContributor,
      testCase "two contributions, refunded" $
        testSucceedsFrom testInit twoContributionsRefund,
      testCase "two contributions, project funded" $
        testSucceedsFrom testInit (allowBigTransactions twoContributionsFund),
      testCase "multiple contributions with one refund, project is funded" $
        testSucceedsFrom testInit (allowBigTransactions multipleContributionsOneRefunded),
      testCase "many contributors, project funded" $
        testSucceedsFrom testInit (allowBigTransactions manyContributorsFund),
      testCase "many contributors, owner refunds" $
        testSucceedsFrom testInit (allowBigTransactions manyContributorsOwnerRefunds),
      testCase "many contributors, some refund, project funded" $
        testSucceedsFrom testInit (allowBigTransactions manyContributorsSomeRefundsFund),
      testCase "one contribution not exceeding minimum, refunded" $
        testSucceedsFrom testInit oneContributionRefundBelowMinimum,
      testCase "owner refunds, one contribution not exceeding minimum" $
        testSucceedsFrom testInit ownerRefundsBelowMinimum,
      testCase "two crowdfunds at the same time" $
        testSucceedsFrom testInit (allowBigTransactions twoCrowdfunds),
      testCase "launch crowdfund without consuming all utxos from funders" $
        testSucceedsFrom testInit (allowBigTransactions partialCrowdfund)
    ]

-- | one contribution, refund error: alice attempts to refund without contributing
oneContributionRefundError :: MonadMockChain m => m ()
oneContributionRefundError = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 3) sOut `as` charlie
  Cf.txRefund `as` alice

-- | one contribution, project funding error: attempting to fund the project
-- when the threshold is not reached
oneContributionFundErrorAmount :: MonadMockChain m => m ()
oneContributionFundErrorAmount = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 4) sOut `as` alice
  Cf.txProjectFund (bananaParams t0) sOut `as` bob

-- | one contribution, project funding error: attempting to fund the project
-- after the deadline
oneContributionFundErrorDeadline :: MonadMockChain m => m ()
oneContributionFundErrorDeadline = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 5) sOut `as` alice
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  Cf.txProjectFund (bananaParams t0) sOut `as` bob

-- | one contribution, project funding error: alice attempts to fund project
-- when bob is the funding target
oneContributionFundErrorOwner :: MonadMockChain m => m ()
oneContributionFundErrorOwner = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 5) sOut `as` alice
  Cf.txProjectFund (bananaParams t0) sOut `as` alice

-- | attempting to refund all contributors before the deadline
ownerRefundsErrorDeadline :: MonadMockChain m => m ()
ownerRefundsErrorDeadline = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 5) sOut `as` alice
  Cf.txIndividualFund (banana 4) sOut `as` charlie
  Cf.txIndividualFund (banana 3) sOut `as` dylan
  Cf.txRefundAll (bananaParams t0) sOut `as` bob

-- | alice attempts to refund all contributors with bob as owner
ownerRefundsErrorOwner :: MonadMockChain m => m ()
ownerRefundsErrorOwner = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 5) sOut `as` alice
  Cf.txIndividualFund (banana 4) sOut `as` charlie
  Cf.txIndividualFund (banana 3) sOut `as` dylan
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  Cf.txRefundAll (bananaParams t0) sOut `as` alice

-- | two contributions, error: one contribution does not exceed minimum
twoContributionsFundErrorMinimum :: MonadMockChain m => m ()
twoContributionsFundErrorMinimum = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 1) sOut `as` alice
  Cf.txIndividualFund (banana 4) sOut `as` charlie
  Cf.txProjectFund (bananaParams t0) sOut `as` bob

-- | owner attempts to pay self all funds after the deadline, with no tokens
-- minted for the contributors
ownerRefundsVulnerability :: MonadMockChain m => m ()
ownerRefundsVulnerability = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 5) sOut `as` alice
  Cf.txIndividualFund (banana 4) sOut `as` charlie
  Cf.txIndividualFund (banana 3) sOut `as` dylan
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  Cf.txRefundAllVulnerability (bananaParams t0) sOut `as` bob

-- | contributor attempts to steal contributions by not consuming the proposal utxo
ownerStealsFunds :: MonadMockChain m => m ()
ownerStealsFunds = do
  t0 <- currentTime
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 3) sOut `as` alice
  Cf.txIndividualFund (banana 4) sOut `as` charlie
  Cf.txProjectFundNoProposal (bananaParams t0) sOut `as` bob

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
      testCase "wrong user attempts to fund project" $
        testFailsFrom testInit oneContributionFundErrorOwner,
      testCase "owner refunds before the deadline" $
        testFailsFrom testInit ownerRefundsErrorDeadline,
      testCase "wrong user attempts to refund all" $
        testFailsFrom testInit ownerRefundsErrorOwner,
      testCase "contribution does not exceed minimum" $
        testFailsFrom testInit twoContributionsFundErrorMinimum,
      testCase "owner pays self all funds after the deadline" $
        testFailsFrom testInit ownerRefundsVulnerability,
      testCase "owner launches without consuming proposal, steals contributions" $
        testFailsFrom testInit ownerStealsFunds
    ]

-- * (hopefully) failing attacks

simpleTraces :: (Alternative m, MonadModalMockChain m) => m ()
simpleTraces =
  allowBigTransactions oneContributionFund
    <|> allowBigTransactions twoContributionsFund
    <|> allowBigTransactions manyContributorsFund

-- | Token duplication attack: Whenever we see a transaction that mints
-- something, try to mint one more token and pay it to the attacker. This should
-- be ruled out by the minting policy of the thread token.
tryDupTokens :: (Alternative m, MonadModalMockChain m) => m ()
tryDupTokens =
  somewhere
    ( dupTokenAttack
        (\_ n -> n + 1) -- the modification of the minted value
        fred -- the attacker's wallet
    )
    simpleTraces

-- | Datum hijacking attack: Try to steal outputs from a validator.
tryDatumHijack :: (Alternative m, MonadModalMockChain m) => m ()
tryDatumHijack =
  somewhere
    ( datumHijackingAttack @Cf.Crowdfunding
        ( \_ d val -> case d of
            Cf.Proposal {} -> L.noAdaValue val /= mempty
            _ -> False
        )
        (0 ==)
    )
    simpleTraces

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
  sOut <- Cf.txOpen (bananaParams t0) `as` bob
  Cf.txMintThreadToken (bananaParams t0) sOut `as` bob
  Cf.txIndividualFund (banana 5) sOut `as` iris
    <|> Cf.txIndividualFund (banana 5) sOut `as` james
  Cf.txProjectFund (bananaParams t0) sOut `as` bob

oneContributionFundAlternative :: TestTree
oneContributionFundAlternative =
  testCase "change in possessions independent of contributor" $
    testBinaryRelatedBy
      ( \a b ->
          testBool $
            adaInState a bob == adaInState b bob
              && tokensInState a bob `setEquals` tokensInState b bob
              && adaInState a iris == adaInState b james
              && tokensInState a iris `setEquals` tokensInState b james
      )
      testInit
      (allowBigTransactions oneContributionFundAlternativeTrace)
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
              (\msg -> "Not minting" `isPrefixOf` msg || "Not burning" `isPrefixOf` msg)
          )
          testInit
          tryDupTokens,
      testCase "datum hijacking" $
        testFailsFrom' isCekEvaluationFailure testInit tryDatumHijack
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
