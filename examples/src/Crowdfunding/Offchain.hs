module Crowdfunding.Offchain where

import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import qualified Crowdfunding as Cf
import Data.Maybe
import qualified Ledger as L
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import PlutusTx.Prelude (nub, sum)

-- | Open the crowdfund
txOpen :: MonadBlockChain m => Cf.ValParams -> m ()
txOpen p = do
  let datum = Cf.Proposal p
  void $
    validateTxSkel $
      txSkel
        [PaysScript Cf.crowdfundingValidator datum minAda]

-- | Individual can contribute a fund
txIndividualFund :: MonadBlockChain m => Cf.ValParams -> L.Value -> m ()
txIndividualFund p fund = do
  funder <- ownPaymentPubKeyHash
  let fp = Cf.FundingParams funder (Cf.fundingTarget p) fund
  let datum = Cf.Funding fp
  void $
    validateTxSkel $
      txSkel
        [PaysScript Cf.crowdfundingValidator datum (fund <> minAda)]

-- | Individual can request a refund
txRefund :: MonadBlockChain m => m ()
txRefund = do
  funder <- ownPaymentPubKeyHash
  utxos <-
    scriptUtxosSuchThat Cf.crowdfundingValidator (\d _ -> Cf.getFunder d == Just funder)
  void $
    validateTxSkel $
      txSkel $
        map (SpendsScript Cf.crowdfundingValidator Cf.IndividualRefund) utxos
          :=>: [paysPK funder (PlutusTx.Prelude.sum $ map (sOutValue . fst) utxos)]

-- | Owner can fund the project before the deadline. When funding, mint n reward tokens,
-- one for each funder. Each token should go to the address of the utxo of the funders.
txProjectFund :: (MonadBlockChain m) => Cf.ValParams -> m ()
txProjectFund p = do
  fundingTarget <- ownPaymentPubKeyHash
  utxos <-
    scriptUtxosSuchThat Cf.crowdfundingValidator (\d _ -> Cf.getOwner d == fundingTarget)
  let datumTotal = PlutusTx.Prelude.sum $ mapMaybe (Cf.getValue . snd) utxos
      q = Cf.PolicyParams {Cf.pRewardTokenName = Cf.rewardTokenName fundingTarget}
      uniqueAddrs = nub $ mapMaybe (Cf.getFunder . snd) utxos
      token num = Value.assetClassValue (Cf.rewardTokenAssetClass p) num
  void $
    validateTxSkel $
      txSkel $
        ( Before (Cf.projectDeadline p) :
          Mints
            (Just (Scripts.validatorAddress Cf.crowdfundingValidator))
            [Cf.rewardTokenPolicy q]
            (token $ fromIntegral $ length uniqueAddrs) :
          map (SpendsScript Cf.crowdfundingValidator Cf.Launch) utxos
        )
          :=>: ( paysPK fundingTarget (datumTotal <> minAda) :
                 map (`paysPK` (token 1 <> minAda)) uniqueAddrs
                 -- uncomment below (and comment above) to attempt to introduce a
                 -- vulnerability where the owner receives all tokens
                 -- map (\_ -> paysPK fundingTarget token) uniqueAddrs
               )

-- | Get total contributions from a specific address given all utxos
getContributionsAddr :: [(SpendableOut, Cf.Datum)] -> L.PubKeyHash -> L.Value
getContributionsAddr utxos addr =
  let addrUtxos = filter (\utxo -> Cf.getFunder (snd utxo) == Just addr) utxos
   in PlutusTx.Prelude.sum $ map (sOutValue . fst) addrUtxos

-- | Owner can refund all contributors after the deadline
txRefundAll :: (MonadBlockChain m) => Cf.ValParams -> m ()
txRefundAll p = do
  fundingTarget <- ownPaymentPubKeyHash
  utxos <-
    scriptUtxosSuchThat Cf.crowdfundingValidator (\d _ -> Cf.getOwner d == fundingTarget)
  -- First, gather all contributions from the same address. This confirms that we only use
  -- one output to pay back a contributor, even if they contributed multiple times.
  let addrUtxos = filter (isJust . Cf.getFunder . snd) utxos
      uniqueAddrs = nub $ mapMaybe (Cf.getFunder . snd) utxos
      contributions = map (getContributionsAddr addrUtxos) uniqueAddrs
  void $
    validateTxSkel $
      txSkel $
        ( After (Cf.projectDeadline p) :
          map (SpendsScript Cf.crowdfundingValidator Cf.Launch) utxos
        )
          :=>: zipWith paysPK uniqueAddrs contributions
