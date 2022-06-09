{-# Language NumericUnderscores #-}
module CrowdfundingAlt.Offchain where

import qualified CrowdfundingAlt as Cf
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


txOpen :: MonadBlockChain m => Cf.PolicyParams -> m ()
txOpen p = do
  let datum = Cf.Proposal p
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True})
      [ PaysScript Cf.crowdfundingValidator datum minAda ] 

txIndividualFund :: MonadBlockChain m => Cf.PolicyParams -> L.Value -> m ()
txIndividualFund p fund = do
  funder <- ownPaymentPubKeyHash 
  let datum = Cf.Funding funder (Cf.fundingTarget p)
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True})
      [ PaysScript Cf.crowdfundingValidator datum fund ]

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
-- TODO: why is there still funds in the script
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

-- | 
ownerRefunds :: MonadMockChain m => m ()
ownerRefunds = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  txIndividualFund (bananaParams t0) (banana 3) `as` wallet 4
  awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  txRefundAll (bananaParams t0) `as` wallet 2

ownerRefundsErrorOwner :: MonadMockChain m => m ()
ownerRefundsErrorOwner = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  txIndividualFund (bananaParams t0) (banana 3) `as` wallet 4
  awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  txRefundAll (bananaParams t0) `as` wallet 1

ownerRefundsErrorDeadline :: MonadMockChain m => m ()
ownerRefundsErrorDeadline = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  txIndividualFund (bananaParams t0) (banana 3) `as` wallet 4
  txRefundAll (bananaParams t0) `as` wallet 2

-- -- | two contributions
-- twoContributions :: MonadMockChain m => m ()
-- twoContributions = do
--   t0 <- currentTime
--   txIndividualFund (bananaParams t0) (wallet 1) (banana 3)
--   txIndividualFund (bananaParams t0) (wallet 3) (banana 1)

-- -- | two contributions, refunded
-- twoContributionsRefund :: MonadMockChain m => m ()
-- twoContributionsRefund = do
--   t0 <- currentTime
--   txIndividualFund (bananaParams t0) (wallet 1) (banana 3)
--   txIndividualFund (bananaParams t0) (wallet 3) (banana 1)
--   awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
--   txRefund (bananaParams t0)

-- -- | two contributions, project funded
-- twoContributionsFund :: MonadMockChain m => m ()
-- twoContributionsFund = do
--   t0 <- currentTime
--   txIndividualFund (bananaParams t0) (wallet 4) (banana 3)
--   txIndividualFund (bananaParams t0) (wallet 3) (banana 2)

-- {-
-- write test with funding someone else
-- datum: funder, id of the project
-- datum doesn't need fund
-- *anyone* can write offchain code
-- validator is Datum -> Redeemer -> ScriptContext -> Bool
-- list of signatures contains addr

-- look into script context, check only one ouptut points to original funder
--  - enough, automatic check that value is the same

-- validator is called for every datum!

-- connor, jackie
-- -}
