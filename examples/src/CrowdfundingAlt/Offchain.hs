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

txFund ::
  (MonadBlockChain m) =>
  Cf.PolicyParams ->
  Wallet ->
  L.Value ->
  m ()
txFund p w fund = do
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
      (wallet 2)
      (Ada.lovelaceValueOf 100_000_000 <> banana 5 : (standard M.! wallet 2))
      standard
  where
    InitialDistribution standard = def

-- | Parameters of an auction that sells two bananas at a minimum bid
-- of 2 Lovelace and a bidding deadline in 60 seconds from the given
-- time.
bananaParams :: L.POSIXTime -> Cf.PolicyParams
bananaParams t =
  Cf.PolicyParams
    { Cf.projectDeadline = t + 60_000,
      Cf.threshold = banana 3,
      Cf.fundingTarget = walletPKHash (wallet 1)
    }


nothing :: MonadMockChain m => m ()
nothing = return ()

oneContribution :: MonadMockChain m => m ()
oneContribution = do
  t0 <- currentTime
  txFund (bananaParams t0) (wallet 2) (banana 3)
  -- awaitTime (Cf.projectDeadline (bananaParams t0) + 1)
  -- txRefund (bananaParams t0)

