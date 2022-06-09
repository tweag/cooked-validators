{-# Language NumericUnderscores #-}
module Crowdfunding.Offchain where

import qualified Crowdfunding as Cf
import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
import Data.Maybe
import qualified Ledger as L
import Ledger.Ada as Ada
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import qualified Data.Map as M
import Cooked.Currencies
import Ledger.Value (toString)
import qualified Cooked.MockChain as L
import qualified Cooked as L
import PlutusTx.Prelude (sum)

-- | Open the crowdfund
txOpen :: MonadBlockChain m => Cf.PolicyParams -> m ()
txOpen p = do
  let datum = Cf.Proposal p
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True})
      [ PaysScript Cf.crowdfundingValidator datum minAda ] 

-- | Individual can contribute a fund
txIndividualFund :: MonadBlockChain m => Cf.PolicyParams -> L.Value -> m ()
txIndividualFund p fund = do
  funder <- ownPaymentPubKeyHash 
  let datum = Cf.Funding funder (Cf.fundingTarget p)
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True})
      [ PaysScript Cf.crowdfundingValidator datum fund ]

-- | Individual can request a refund
txRefund :: MonadBlockChain m => m ()
txRefund = do
  funder <- ownPaymentPubKeyHash
  utxos <-
    scriptUtxosSuchThat Cf.crowdfundingValidator (\d _ -> Cf.getFunder d == Just funder)
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        map (SpendsScript Cf.crowdfundingValidator Cf.IndividualRefund) utxos
          :=>:
        map (paysPK funder . sOutValue . fst) utxos

-- | Owner can fund the project before the deadline
txProjectFund :: (MonadBlockChain m) => Cf.PolicyParams -> m ()
txProjectFund p = do
  fundingTarget <- ownPaymentPubKeyHash
  utxos <-
    scriptUtxosSuchThat Cf.crowdfundingValidator (\d _ -> Cf.getOwner d == fundingTarget)
  let total = PlutusTx.Prelude.sum $ map (sOutValue . fst) utxos
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        (Before (Cf.projectDeadline p) :
          map (SpendsScript Cf.crowdfundingValidator Cf.Launch) utxos
        )
          :=>:
        [ paysPK fundingTarget total ]

-- | Owner can refund all contributors after the deadline
txRefundAll :: (MonadBlockChain m) => Cf.PolicyParams -> m ()
txRefundAll p = do
  fundingTarget <- ownPaymentPubKeyHash
  utxos <-
    scriptUtxosSuchThat Cf.crowdfundingValidator
    (\d _ -> isJust (Cf.getFunder d) && Cf.getOwner d == fundingTarget)
  void $
    mapM (\x -> validateTxSkel $
           txSkelOpts (def {adjustUnbalTx = True}) $
             [ After (Cf.projectDeadline p),
               SpendsScript Cf.crowdfundingValidator Cf.Launch x ]
             :=>:
             [ paysPK (fromJust $ Cf.getFunder $ snd x) (sOutValue $ fst x) ]
         ) utxos

-- Just so we have something to fund that's not Ada:
-- Have a banana.

bananaAssetClass :: Value.AssetClass
bananaAssetClass = permanentAssetClass "Banana"

-- | Value representing a number of bananas
banana :: Integer -> Value.Value
banana = Value.assetClassValue bananaAssetClass

-- | How many bananas are in the given value? This is a left inverse of 'banana'.
bananasIn :: Value.Value -> Integer
bananasIn v = Value.assetClassValueOf v bananaAssetClass

-- -- | initial distribution s.t. all wallets own 5 bananas
testInit :: InitialDistribution
testInit = initialDistribution' [(wallet i, [minAda <> banana 5]) | i <- [1..10]]

-- | Parameters of a crowdfund that is attempting to fund 5 bananas to wallet 2,
-- with a deadline in 60 seconds from the given time.
bananaParams :: L.POSIXTime -> Cf.PolicyParams
bananaParams t =
  Cf.PolicyParams
    { Cf.projectDeadline = t + 60_000,
      Cf.threshold = banana 5,
      Cf.fundingTarget = walletPKHash (wallet 2)
    }

nothing :: MonadMockChain m => m ()
nothing = return ()

-- | one contribution
oneContribution :: MonadMockChain m => m ()
oneContribution = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 3) `as` wallet 3

-- | one contribution, refunded
-- TODO: why are there still funds in the script
oneContributionRefund :: MonadMockChain m => m ()
oneContributionRefund = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 3) `as` wallet 3
  txRefund `as` wallet 3

-- | one contribution, refund error: wallet 1 attempts to refund without contributing
oneContributionRefundError :: MonadMockChain m => m ()
oneContributionRefundError = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 3) `as` wallet 3
  txRefund `as` wallet 1

-- | one contributer, multiple contributions, refunded
oneContributorRefund :: MonadMockChain m => m ()
oneContributorRefund = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 3) `as` wallet 3
  txIndividualFund (bananaParams t0) (banana 1) `as` wallet 3
  txRefund `as` wallet 3

-- | one contribution, project funded
oneContributionFund :: MonadMockChain m => m ()
oneContributionFund = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  txProjectFund (bananaParams t0) `as` wallet 2

-- | one contribution, project funding error: attempting to fund the project
-- when the threshold is not reached
oneContributionFundErrorAmount :: MonadMockChain m => m ()
oneContributionFundErrorAmount = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 4) `as` wallet 1
  txProjectFund (bananaParams t0) `as` wallet 2

-- | one contribution, project funding error: wallet 1 attempts to fund project
-- when wallet 2 is the funding target. Funds stay locked in the script
oneContributionFundErrorOwner :: MonadMockChain m => m ()
oneContributionFundErrorOwner = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  txProjectFund (bananaParams t0) `as` wallet 1

-- | one contribution, project funding error: attempting to fund the project
-- after the deadline
oneContributionFundErrorDeadline :: MonadMockChain m => m ()
oneContributionFundErrorDeadline = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  txProjectFund (bananaParams t0) `as` wallet 2

-- | owner refunds all contributors
ownerRefunds :: MonadMockChain m => m ()
ownerRefunds = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  txIndividualFund (bananaParams t0) (banana 3) `as` wallet 4
  awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  txRefundAll (bananaParams t0) `as` wallet 2

-- | wallet 1 attempts to refund all contributors with wallet 2 as owner
ownerRefundsErrorOwner :: MonadMockChain m => m ()
ownerRefundsErrorOwner = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  txIndividualFund (bananaParams t0) (banana 3) `as` wallet 4
  awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  txRefundAll (bananaParams t0) `as` wallet 1

-- | attemptint to refund all contributors before the deadline
ownerRefundsErrorDeadline :: MonadMockChain m => m ()
ownerRefundsErrorDeadline = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  txIndividualFund (bananaParams t0) (banana 3) `as` wallet 4
  txRefundAll (bananaParams t0) `as` wallet 2

-- | two contributions, funded
twoContributionsFund :: MonadMockChain m => m ()
twoContributionsFund = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 3) `as` wallet 1
  txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  txProjectFund (bananaParams t0) `as` wallet 2

-- | two contributions, funded
twoContributionsRefund :: MonadMockChain m => m ()
twoContributionsRefund = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 3) `as` wallet 1
  txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  txRefund `as` wallet 1
  txRefund `as` wallet 3
