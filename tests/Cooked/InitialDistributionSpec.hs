{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.InitialDistributionSpec where

import Control.Monad
import Cooked
import Data.Default
import qualified Data.Map as Map
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Pl
import Test.Tasty
import Test.Tasty.HUnit

-- | An initial distribution where wallet 1 owns a UTxO with a datum
-- of type Int and value 10
initialDistributionWithDatum =
  InitialDistribution
    [withInlineDatum @Integer (paysPK (walletPKHash $ wallet 1) (ada 2)) 10]

-- | An initial distribution where wallet 1 owns a UTxO with a
-- reference script corresponding to the always succeed validators
-- and wallet 2 owns 2 UTxOs with 100 ada
initialDistributionWithReferenceScript =
  InitialDistribution $
    (paysPK (walletPKHash $ wallet 1) (ada 2) `withReferenceScript` alwaysTrueValidator @MockContract)
      : replicate 2 (paysPK (walletPKHash $ wallet 2) (ada 100))

getValueFromInitialDatum :: (MonadBlockChain m) => m Integer
getValueFromInitialDatum = do
  [(txOutRef, _)] <- runUtxoSearch $ utxosAtSearch $ walletAddress $ wallet 1
  Just datum <- typedDatumFromTxOutRef @Integer txOutRef
  return datum

printAndRun initDist trace = printCooked $ fst <$> interpretAndRunWith (runMockChainTFrom initDist) trace

spendReferenceAlwaysTrueValidator :: (MonadBlockChain m) => m ()
spendReferenceAlwaysTrueValidator = do
  [(referenceScriptTxOutRef, _)] <- runUtxoSearch $ utxosAtSearch $ walletAddress $ wallet 1
  tx <-
    validateTxSkel $
      txSkelTemplate
        { txSkelOuts = [paysScript (alwaysTrueValidator @MockContract) () (ada 2)],
          txSkelSigners = [wallet 2]
        }
  let (scriptTxOutRef, _) : _ = utxosFromCardanoTx tx
  void $
    validateTxSkel $
      txSkelTemplate
        { txSkelOuts = [paysPK (walletPKHash $ wallet 1) (ada 2)],
          txSkelIns = Map.singleton scriptTxOutRef (TxSkelRedeemerForReferencedScript referenceScriptTxOutRef ()),
          txSkelSigners = [wallet 2]
        }

tests :: TestTree
tests =
  testGroup
    "initial distributions"
    [ testCase "Reading a datum placed in the initial distribution" $
        testSucceedsFrom' def (\n _ -> testBool $ n == 10) initialDistributionWithDatum getValueFromInitialDatum,
      testCase "Spending a script placed as a reference script in the initial distribution" $
        testSucceedsFrom def initialDistributionWithReferenceScript spendReferenceAlwaysTrueValidator
    ]
