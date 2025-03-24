module Cooked.InitialDistributionSpec where

import Control.Monad
import Cooked
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Plutus.Script.Utils.V3.Typed.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import Test.Tasty
import Test.Tasty.HUnit

alice, bob :: Wallet
(alice, bob) = (wallet 1, wallet 2)

-- | An initial distribution where alice owns a UTxO with a datum of
-- type Int and value 10 for each datum kind
initialDistributionWithDatum :: InitialDistribution
initialDistributionWithDatum =
  InitialDistribution $ [receives alice] <*> ([VisibleHashedDatum, HiddenHashedDatum] <*> [10 :: Integer])

-- | An initial distribution where alice owns a UTxO with a reference
-- script corresponding to the always succeed validators and bob owns
-- 2 UTxOs with 100 ADA
initialDistributionWithReferenceScript :: InitialDistribution
initialDistributionWithReferenceScript =
  InitialDistribution $
    (alice `receives` (Value (Script.ada 2) <&&> ReferenceScript (Script.trueSpendingMPScript @())))
      : replicate 2 (bob `receives` Value (Script.ada 100))

getValueFromInitialDatum :: (MonadBlockChain m) => m [Integer]
getValueFromInitialDatum = do
  aliceUtxos <- runUtxoSearch $ utxosAtSearch alice
  catMaybes <$> mapM (typedDatumFromTxOutRef @Integer . fst) aliceUtxos

spendReferenceAlwaysTrueValidator :: (MonadBlockChain m) => m ()
spendReferenceAlwaysTrueValidator = do
  [(referenceScriptTxOutRef, _)] <- runUtxoSearch $ utxosAtSearch alice
  (scriptTxOutRef : _) <-
    validateTxSkel' $
      txSkelTemplate
        { txSkelOuts = [Script.trueSpendingMPScript @() `receives` Value (Script.ada 2)],
          txSkelSigners = [bob]
        }
  void $
    validateTxSkel $
      txSkelTemplate
        { txSkelOuts = [alice `receives` Value (Script.ada 2)],
          txSkelIns = Map.singleton scriptTxOutRef $ someTxSkelRedeemer () `withReferenceInput` referenceScriptTxOutRef,
          txSkelSigners = [bob]
        }

tests :: TestTree
tests =
  testGroup
    "Initial distributions"
    [ testCase "Reading datums placed in the initial distribution, inlined or hashed" $
        testToProp $
          mustSucceedTest getValueFromInitialDatum
            `withInitDist` initialDistributionWithDatum
            `withValuePred` (testBool . (== [10, 10])),
      testCase "Spending a script placed as a reference script in the initial distribution" $
        testToProp $
          mustSucceedTest spendReferenceAlwaysTrueValidator
            `withInitDist` initialDistributionWithReferenceScript
    ]
