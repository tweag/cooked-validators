module Cooked.InitialDistributionSpec where

import Control.Monad
import Cooked
import Data.Default
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Test.Tasty
import Test.Tasty.HUnit

alice = wallet 1

bob = wallet 2

-- | An initial distribution where alice owns a UTxO with a datum of
-- type Int and value 10 for each datum kind
initialDistributionWithDatum =
  InitialDistribution $
    (\f -> f (paysPK alice (2 :: Integer)) 10)
      <$> [withDatum @Integer, withInlineDatum, withDatumHash]

-- | An initial distribution where alice owns a UTxO with a reference
-- script corresponding to the always succeed validators and bob owns
-- 2 UTxOs with 100 ada
initialDistributionWithReferenceScript =
  InitialDistribution $
    (paysPK alice (2 :: Integer) `withReferenceScript` alwaysTrueValidator @MockContract)
      : replicate 2 (paysPK bob (100 :: Integer))

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
        { txSkelOuts = [paysScript (alwaysTrueValidator @MockContract) () (2 :: Integer)],
          txSkelSigners = [bob]
        }
  void $
    validateTxSkel $
      txSkelTemplate
        { txSkelOuts = [paysPK alice (2 :: Integer)],
          txSkelIns = Map.singleton scriptTxOutRef (TxSkelRedeemerForReferencedScript referenceScriptTxOutRef ()),
          txSkelSigners = [bob]
        }

tests :: TestTree
tests =
  testGroup
    "Initial distributions"
    [ testCase "Reading datums placed in the initial distribution, inlined, hashed or vanilla" $
        testSucceedsFrom' def (\results _ -> testBool $ results == [10, 10, 10]) initialDistributionWithDatum getValueFromInitialDatum,
      testCase "Spending a script placed as a reference script in the initial distribution" $
        testSucceedsFrom def initialDistributionWithReferenceScript spendReferenceAlwaysTrueValidator
    ]
