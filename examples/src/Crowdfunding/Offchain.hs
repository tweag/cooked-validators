module Crowdfunding.Offchain where

import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import qualified Crowdfunding as Cf
import Data.Default
import Data.Maybe
import qualified Ledger as L
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import qualified PlutusTx.Numeric as Pl
import PlutusTx.Prelude (nub, sum)

-- | Open the crowdfund
txOpen :: MonadBlockChain m => Cf.ValParams -> m SpendableOut
txOpen p = do
  let datum = Cf.Proposal p
  tx <-
    validateTxSkel $
      txSkel
        [paysScript Cf.crowdfundingValidator datum minAda]
  outputs <- spOutsFromCardanoTx tx
  return $ head $ filter (isJust . sBelongsToScript) outputs

-- | Mint the thread token
txMintThreadToken :: MonadBlockChain m => Cf.ValParams -> SpendableOut -> m ()
txMintThreadToken p sOut = do
  let datum = Cf.Proposal p
      txOut = fst sOut
      threadToken = Cf.threadToken txOut
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        [ SpendsScript
            Cf.crowdfundingValidator
            Cf.Minting
            sOut,
          Mints
            (Just txOut)
            [Cf.threadTokenPolicy]
            threadToken
        ]
          :=>: [paysScript Cf.crowdfundingValidator datum threadToken]

-- | Individual can contribute a fund
txIndividualFund :: MonadBlockChain m => Cf.ValParams -> L.Value -> SpendableOut -> m ()
txIndividualFund p fund sOut = do
  funder <- ownPaymentPubKeyHash
  let fp = Cf.FundingParams funder (Cf.fundingTarget p) fund $ fst sOut
  let datum = Cf.Funding fp
  void $
    validateTxSkel $
      txSkelOpts
        (def {adjustUnbalTx = True})
        [paysScript Cf.crowdfundingValidator datum fund]

-- | Individual can request a refund
txRefund :: MonadBlockChain m => m ()
txRefund = do
  funder <- ownPaymentPubKeyHash
  utxos <-
    scriptUtxosSuchThat Cf.crowdfundingValidator (\d _ -> Cf.getFunder d == Just funder)
  void $
    validateTxSkel $
      txSkel $
        map (SpendsScript Cf.crowdfundingValidator Cf.IndividualRefund . fst) utxos
          :=>: [paysPK funder (PlutusTx.Prelude.sum $ map (sOutValue . fst) utxos)]

-- | Owner can fund the project before the deadline. When funding, mint n reward tokens,
-- one for each funder. Each token should go to the address of the utxo of the funders.
txProjectFund :: (MonadBlockChain m) => Cf.ValParams -> SpendableOut -> m ()
txProjectFund p sOut = do
  let txOut = fst sOut
      threadToken = Cf.threadToken txOut
      rewardToken = Cf.rewardToken txOut
  fundingTarget <- ownPaymentPubKeyHash
  [proposalUtxo] <-
    scriptUtxosSuchThat Cf.crowdfundingValidator (\_ x -> x `Value.geq` threadToken)
  fundingUtxos <-
    scriptUtxosSuchThat Cf.crowdfundingValidator (\d _ -> Cf.getUtxoDatum d == Just txOut)
  let datumTotal = PlutusTx.Prelude.sum $ mapMaybe (Cf.getValue . snd) fundingUtxos
      uniqueAddrs = nub $ mapMaybe (Cf.getFunder . snd) fundingUtxos
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        ( Before (Cf.projectDeadline p) :
          Mints
            (Just (Scripts.validatorAddress Cf.crowdfundingValidator))
            [Cf.rewardTokenPolicy txOut]
            (rewardToken $ fromIntegral $ length uniqueAddrs) :
          Mints
            (Just txOut)
            [Cf.threadTokenPolicy]
            (Pl.negate threadToken) :
          map
            (SpendsScript Cf.crowdfundingValidator (Cf.Launch txOut) . fst)
            (proposalUtxo : fundingUtxos)
        )
          :=>: ( paysPK fundingTarget datumTotal :
                 map (`paysPK` rewardToken 1) uniqueAddrs
                 -- uncomment below (and comment above) to attempt to introduce a
                 -- vulnerability where the owner receives all tokens
                 -- map (\_ -> paysPK fundingTarget token) uniqueAddrs
               )

-- | Get total contributions from a specific address given all utxos
getContributionsAddr :: [(SpendableOut, Cf.CfDatum)] -> L.PubKeyHash -> L.Value
getContributionsAddr utxos addr =
  let addrUtxos = filter (\utxo -> Cf.getFunder (snd utxo) == Just addr) utxos
   in PlutusTx.Prelude.sum $ map (sOutValue . fst) addrUtxos

-- | Owner can refund all contributors after the deadline
txRefundAll :: (MonadBlockChain m) => Cf.ValParams -> SpendableOut -> m ()
txRefundAll p sOut = do
  let txOut = fst sOut
      threadToken = Cf.threadToken txOut
  [proposalUtxo] <-
    scriptUtxosSuchThat Cf.crowdfundingValidator (\_ x -> x `Value.geq` threadToken)
  fundingUtxos <-
    scriptUtxosSuchThat Cf.crowdfundingValidator (\d _ -> Cf.getUtxoDatum d == Just txOut)
  -- First, gather all contributions from the same address. This confirms that we only use
  -- one output to pay back a contributor, even if they contributed multiple times.
  let addrUtxos = filter (isJust . Cf.getFunder . snd) fundingUtxos
      uniqueAddrs = nub $ mapMaybe (Cf.getFunder . snd) fundingUtxos
      contributions = map (getContributionsAddr addrUtxos) uniqueAddrs
  void $
    validateTxSkel $
      txSkel $
        ( After (Cf.projectDeadline p) :
          Mints
            (Just txOut)
            [Cf.threadTokenPolicy]
            (Pl.negate threadToken) :
          map
            (SpendsScript Cf.crowdfundingValidator (Cf.Launch txOut) . fst)
            (proposalUtxo : fundingUtxos)
        )
          :=>: zipWith paysPK uniqueAddrs contributions

-- | Vulnerability: owner pays all funds to themself instead of refunding
-- all the contributors
txRefundAllVulnerability :: (MonadBlockChain m) => Cf.ValParams -> SpendableOut -> m ()
txRefundAllVulnerability p sOut = do
  let txOut = fst sOut
      threadToken = Cf.threadToken txOut
  fundingTarget <- ownPaymentPubKeyHash
  [proposalUtxo] <-
    scriptUtxosSuchThat Cf.crowdfundingValidator (\_ x -> x `Value.geq` threadToken)
  fundingUtxos <-
    scriptUtxosSuchThat Cf.crowdfundingValidator (\d _ -> Cf.getUtxoDatum d == Just txOut)
  -- First, gather all contributions from the same address. This confirms that we only use
  -- one output to pay back a contributor, even if they contributed multiple times.
  let addrUtxos = filter (isJust . Cf.getFunder . snd) fundingUtxos
      uniqueAddrs = nub $ mapMaybe (Cf.getFunder . snd) fundingUtxos
      totalContributions = PlutusTx.Prelude.sum $ map (getContributionsAddr addrUtxos) uniqueAddrs
  void $
    validateTxSkel $
      txSkel $
        ( After (Cf.projectDeadline p) :
          Mints
            (Just txOut)
            [Cf.threadTokenPolicy]
            (Pl.negate threadToken) :
          map
            (SpendsScript Cf.crowdfundingValidator (Cf.Launch txOut) . fst)
            (proposalUtxo : fundingUtxos)
        )
          :=>: [paysPK fundingTarget totalContributions]
