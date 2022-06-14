{-# Language NumericUnderscores #-}
module CrowdfundingExt.Offchain where

import qualified CrowdfundingExt as Cf
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
import PlutusTx.Prelude (sum, nub)
import PlutusTx.Builtins.Class (stringToBuiltinByteString)

import Debug.Trace

-- | Open the crowdfund
txOpen :: MonadBlockChain m => Cf.ValParams -> m ()
txOpen p = do
  let datum = Cf.Proposal p
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True})
      [ PaysScript Cf.crowdfundingValidator datum minAda ]

-- | Individual can contribute a fund
txIndividualFund :: MonadBlockChain m => Cf.ValParams -> L.Value -> m ()
txIndividualFund p fund = do
  funder <- ownPaymentPubKeyHash 
  let datum = Cf.Funding funder (Cf.fundingTarget p) fund
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

-- | Owner can fund the project before the deadline. When funding, mint n reward tokens,
-- one for each funder. Each token should go to the address of the utxo of the funders.
-- TODO - vulnerability: can pay funding target all tokens
txProjectFund :: (MonadBlockChain m) => Cf.ValParams -> m ()
txProjectFund p = do
  fundingTarget <- ownPaymentPubKeyHash
  utxos <-
    scriptUtxosSuchThat Cf.crowdfundingValidator (\d _ -> Cf.getOwner d == fundingTarget)
  let total = PlutusTx.Prelude.sum $ map (sOutValue . fst) utxos
      q = Cf.PolicyParams { Cf.pRewardTokenName = Cf.rewardTokenName }
      uniqueAddrs = nub $ mapMaybe (Cf.getFunder . snd) utxos
      token = Value.assetClassValue (Cf.rewardTokenAssetClass p) 1
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True, unsafeModTx = RawModTx traceShowId}) $
        ( Before (Cf.projectDeadline p) :
          replicate (length uniqueAddrs)
          (Mints
              (Just (Scripts.validatorAddress Cf.crowdfundingValidator))
              [Cf.rewardTokenPolicy q]
              token
          ) ++
          map (SpendsScript Cf.crowdfundingValidator Cf.Launch) utxos
        )
          :=>:
        ( paysPK fundingTarget total :
          map (`paysPK` token) uniqueAddrs
          -- uncomment below to pay owner all tokens
          -- map (\_ -> paysPK fundingTarget token) uniqueAddrs
        )

-- | Owner can refund all contributors after the deadline
txRefundAll :: (MonadBlockChain m) => Cf.ValParams -> m ()
txRefundAll p = do
  fundingTarget <- ownPaymentPubKeyHash
  utxos <-
    scriptUtxosSuchThat Cf.crowdfundingValidator (\d _ -> Cf.getOwner d == fundingTarget)
  void $
    mapM (\x -> validateTxSkel $
           txSkelOpts (def {adjustUnbalTx = True}) $
             [ After (Cf.projectDeadline p),
               SpendsScript Cf.crowdfundingValidator Cf.Launch x ]
             :=>:
             -- TODO: what is owner being paid here?
             [ paysPK (Cf.getFunderOwner $ snd x) (sOutValue $ fst x) ]
         ) utxos

-- Just so we have something to fund that's not Ada:
-- Have a banana

bananaAssetClass :: Value.AssetClass
bananaAssetClass = permanentAssetClass "Banana"

-- | Value representing a number of bananas
banana :: Integer -> Value.Value
banana = Value.assetClassValue bananaAssetClass

-- | How many bananas are in the given value? This is a left inverse of 'banana'.
bananasIn :: Value.Value -> Integer
bananasIn v = Value.assetClassValueOf v bananaAssetClass

-- | initial distribution s.t. all wallets own 5 bananas
testInit :: InitialDistribution
testInit = initialDistribution' [(wallet i, [minAda <> banana 5]) | i <- [1..10]]

-- | Parameters of a crowdfund that is attempting to fund 5 bananas to wallet 2,
-- with a deadline in 60 seconds from the given time.
bananaParams :: L.POSIXTime -> Cf.ValParams
bananaParams t =
  Cf.ValParams
    { Cf.projectDeadline = t + 60_000,
      Cf.threshold = banana 5,
      Cf.minContribution = banana 2,
      Cf.fundingTarget = walletPKHash (wallet 2),
      Cf.rewardTokenAssetClass = Cf.getRewardTokenAssetClass 
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
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  txProjectFund (bananaParams t0) `as` wallet 2

-- | owner refunds all contributors
ownerRefunds :: MonadMockChain m => m ()
ownerRefunds = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  txIndividualFund (bananaParams t0) (banana 3) `as` wallet 4
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  txRefundAll (bananaParams t0) `as` wallet 2

-- | owner contributes, project is funded
ownerContributes :: MonadMockChain m => m ()
ownerContributes = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 5) `as` wallet 2
  txProjectFund (bananaParams t0) `as` wallet 2

-- | owner refunds all contributors
ownerRefundsSameContributor :: MonadMockChain m => m ()
ownerRefundsSameContributor = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 1) `as` wallet 1
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  txIndividualFund (bananaParams t0) (banana 1) `as` wallet 1
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  txRefundAll (bananaParams t0) `as` wallet 2

-- | wallet 1 attempts to refund all contributors with wallet 2 as owner
-- funds are locked in the script
ownerRefundsErrorOwner :: MonadMockChain m => m ()
ownerRefundsErrorOwner = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 5) `as` wallet 1
  txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  txIndividualFund (bananaParams t0) (banana 3) `as` wallet 4
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  txRefundAll (bananaParams t0) `as` wallet 1

-- | attempting to refund all contributors before the deadline
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

-- | two contributions, error: one contribution does not exceed minimum
twoContributionsFundErrorMinimum :: MonadMockChain m => m ()
twoContributionsFundErrorMinimum = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 1) `as` wallet 1
  txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  txProjectFund (bananaParams t0) `as` wallet 2

-- | two contributions, refunded
twoContributionsRefund :: MonadMockChain m => m ()
twoContributionsRefund = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 3) `as` wallet 1
  txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  txRefund `as` wallet 1
  txRefund `as` wallet 3

-- | multiple contributions, one refunded before project is funded
multipleContributionsOneRefunded :: MonadMockChain m => m ()
multipleContributionsOneRefunded = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 3) `as` wallet 1
  txIndividualFund (bananaParams t0) (banana 4) `as` wallet 3
  txIndividualFund (bananaParams t0) (banana 5) `as` wallet 4
  txRefund `as` wallet 3
  txProjectFund (bananaParams t0) `as` wallet 2

-- | one contributor, multiple contributions, project is funded but contributor
oneContributorFund :: MonadMockChain m => m ()
oneContributorFund = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  txIndividualFund (bananaParams t0) (banana 1) `as` wallet 1
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  txProjectFund (bananaParams t0) `as` wallet 2

-- | many contributors, including some with multiple contributions. project is funded
manyContributorsFund :: MonadMockChain m => m ()
manyContributorsFund = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 3
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 4
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 5
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 3
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 4
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 5
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 6
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 7
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 8
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  txProjectFund (bananaParams t0) `as` wallet 2

-- | many contributors, including some with multiple contributions. owner refunds all
manyContributorsOwnerRefunds :: MonadMockChain m => m ()
manyContributorsOwnerRefunds = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 3
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 4
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 5
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 3
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 4
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 5
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 6
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 7
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 8
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  void $ awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  txRefundAll (bananaParams t0) `as` wallet 2

-- | many contributors, including some with multiple contributions. some individual
-- refunds. project is ultimately funded
manyContributorsSomeRefundsFund :: MonadMockChain m => m ()
manyContributorsSomeRefundsFund = do
  t0 <- currentTime
  txOpen (bananaParams t0)
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 3
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 4
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 5
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 3
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 4
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 5
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 6
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 7
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 8
  txIndividualFund (bananaParams t0) (banana 2) `as` wallet 1
  txRefund `as` wallet 3
  txRefund `as` wallet 4
  txRefund `as` wallet 8
  txProjectFund (bananaParams t0) `as` wallet 2
