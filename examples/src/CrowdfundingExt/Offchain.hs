{-# Language NumericUnderscores #-}
module Crowdfunding.Offchain where

import qualified CrowdfundingExt as Cf
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

txOpen ::
  (MonadBlockChain m) =>
  Cf.StaticValParams ->
  m (Cf.ValParams, Cf.PolicyParams)
txOpen p = do
  utxo : _ <- pkUtxos (Cf.fundingTarget' p)
  let threadTokenAC =
        Value.assetClass
          (L.scriptCurrencySymbol $ Cf.threadTokenPolicy $ Cf.PolicyParams Cf.threadTokenName)
          Cf.threadTokenName
      p' :: Cf.ValParams
      p' =
        Cf.ValParams
          { Cf.staticValParams = p,
            Cf.threadTokenAssetClass = threadTokenAC
          }

      q :: Cf.PolicyParams
      q =
        Cf.PolicyParams { Cf.pThreadTokenName = Cf.threadTokenName }

      token :: L.Value
      token = Value.assetClassValue (Cf.threadTokenAssetClass p') 1

  _ <-
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        [ Mints
            (Just (Scripts.validatorAddress (Cf.crowdfundingValidator p')))
            [Cf.threadTokenPolicy q]
            token,
          SpendsPK utxo
        ]
          :=>: [ PaysScript (Cf.crowdfundingValidator p') Cf.NoFunds token
               ]
  return (p', q)

txFund ::
  (MonadBlockChain m) =>
  Cf.ValParams ->
  L.Value ->
  m ()
txFund p fund = do
  funder <- ownPaymentPubKeyHash
  [utxo] <- scriptUtxosSuchThat (Cf.crowdfundingValidator p) (\_ _ -> True)
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        [ -- no need to ask for the bidder to sign the transaction, that's automatic
          Before (Cf.projectDeadline p),
          SpendsScript
            (Cf.crowdfundingValidator p)
            Cf.Burn
            utxo
        ]
          :=>: [ PaysScript (Cf.crowdfundingValidator p) (Cf.Funding (Cf.FunderInfo fund funder)) $
                     fund <> Value.assetClassValue (Cf.threadTokenAssetClass p) 1,
                 paysPK (Cf.fundingTarget p) fund
               ]

toRefund :: Cf.CrowdfundingState -> Maybe (L.Value, L.PubKeyHash)
toRefund (Cf.Funding (Cf.FunderInfo fund funder)) = Just (fund, funder)
toRefund _ = Nothing

txRefund ::
  (MonadBlockChain m) =>
  Cf.ValParams ->
  Cf.PolicyParams ->
  m ()
txRefund p q = do
  [utxo] <- scriptUtxosSuchThat (Cf.crowdfundingValidator p) (\_ _ -> True)
  void $
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        [ After (Cf.projectDeadline p),
          SpendsScript
            (Cf.crowdfundingValidator p)
            Cf.IndividualRefund
            utxo,
          Mints
            (Just (Scripts.validatorAddress (Cf.crowdfundingValidator p)))
            [Cf.threadTokenPolicy q]
            (Value.assetClassValue (Cf.threadTokenAssetClass p) (-1))
        ]
          :=>:
        case toRefund (snd utxo) of
          Nothing -> []
          Just (fund, funder) -> [ paysPK funder fund ]

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

-- | initial distribution s.t. the first wallet owns five bananas
testInit :: InitialDistribution
testInit =
  InitialDistribution $
    M.insert
      (wallet 1)
      (Ada.lovelaceValueOf 100_000_000 <> banana 5 : (standard M.! wallet 1))
      standard
  where
    InitialDistribution standard = def

-- | Parameters of an auction that sells two bananas at a minimum bid
-- of 2 Lovelace and a bidding deadline in 60 seconds from the given
-- time.
bananaParams :: L.POSIXTime -> Cf.StaticValParams
bananaParams t =
  Cf.StaticValParams
    { Cf.projectDeadline' = t + 60_000,
      Cf.threshold' = banana 3,
      Cf.fundingTarget' = walletPKHash (wallet 1)
    }


nothing :: MonadMockChain m => m ()
nothing = return ()

noContributions :: MonadMockChain m => m ()
noContributions = do
  t0 <- currentTime
  (p, q) <- txOpen (bananaParams t0) `as` wallet 1
  awaitTime (Cf.projectDeadline p - 1_000 + 1)
  txRefund p q

oneContributionEnough :: MonadMockChain m => m ()
oneContributionEnough = do
  t0 <- currentTime
  (p, q) <- txOpen (bananaParams t0) `as` wallet 1
  txFund p (banana 2) `as` wallet 2
  awaitTime (Cf.projectDeadline p - 1_000 + 1)
  txRefund p q

