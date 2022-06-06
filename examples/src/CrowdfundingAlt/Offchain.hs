{-# Language NumericUnderscores #-}
module CrowdfundingAlt.Offchain where

import qualified CrowdfundingAlt as Cf
import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
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

txIndividualFund ::
  (MonadBlockChain m) =>
  Cf.PolicyParams ->
  Wallet ->
  L.Value ->
  m ()
txIndividualFund p w fund = do
  let datum = Cf.FunderInfo fund (walletPKHash w)
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True})
      [ PaysScript (Cf.crowdfundingValidator p) datum fund ]

txRefund ::
  (MonadBlockChain m) =>
  Cf.PolicyParams ->
  m ()
txRefund p = do
  [utxo] <- scriptUtxosSuchThat (Cf.crowdfundingValidator p) (\_ _ -> True)
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        [ After (Cf.projectDeadline p),
          SpendsScript
            (Cf.crowdfundingValidator p)
            Cf.IndividualRefund
            utxo
        ]
          :=>:
        [ paysPK (Cf.funder (snd utxo)) (Cf.fund (snd utxo)) ]

txProjectFund ::
  (MonadBlockChain m) =>
  Cf.PolicyParams ->
  m ()
txProjectFund p = do
  utxos <- scriptUtxosSuchThat (Cf.crowdfundingValidator p) (\_ _ -> True)
  let utxoVals = map fst utxos
  let total = PlutusTx.Prelude.sum $ map sOutValue utxoVals
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        (Before (Cf.projectDeadline p) :
          map (\v -> SpendsScript (Cf.crowdfundingValidator p) Cf.Burn v) utxos)
          :=>:
        [ paysPK (Cf.fundingTarget p) total ]

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
  txIndividualFund (bananaParams t0) (wallet 1) (banana 3)

-- | one contribution, refunded
oneContributionRefund :: MonadMockChain m => m ()
oneContributionRefund = do
  t0 <- currentTime
  txIndividualFund (bananaParams t0) (wallet 1) (banana 3)
  awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  txRefund (bananaParams t0)

-- | one contribution, project funded
oneContributionFund :: MonadMockChain m => m ()
oneContributionFund = do
  t0 <- currentTime
  txIndividualFund (bananaParams t0) (wallet 1) (banana 5)
  txProjectFund (bananaParams t0)

-- | should throw error: attempting to fund the project after the deadline
oneContributionFundError :: MonadMockChain m => m ()
oneContributionFundError = do
  t0 <- currentTime
  txIndividualFund (bananaParams t0) (wallet 1) (banana 5)
  awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  txProjectFund (bananaParams t0)

-- | two contributions
twoContributions :: MonadMockChain m => m ()
twoContributions = do
  t0 <- currentTime
  txIndividualFund (bananaParams t0) (wallet 1) (banana 3)
  txIndividualFund (bananaParams t0) (wallet 3) (banana 1)

-- | two contributions, refunded
twoContributionsRefund :: MonadMockChain m => m ()
twoContributionsRefund = do
  t0 <- currentTime
  txIndividualFund (bananaParams t0) (wallet 1) (banana 3)
  txIndividualFund (bananaParams t0) (wallet 3) (banana 1)
  awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  txRefund (bananaParams t0)

-- | two contributions, project funded
twoContributionsFund :: MonadMockChain m => m ()
twoContributionsFund = do
  t0 <- currentTime
  txIndividualFund (bananaParams t0) (wallet 4) (banana 3)
  txIndividualFund (bananaParams t0) (wallet 3) (banana 2)

