{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.InitialDistributionSpec where

import Control.Monad
import Cooked
import Data.Default
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Pl
import Test.Tasty
import Test.Tasty.HUnit

alice = wallet 1

alicePKH = walletPKHash alice

bob = wallet 2

bobPKH = walletPKHash bob

-- | An initial distribution where alice owns a UTxO with a datum
-- of type Int and value 10 for each datum kind
initialDistributionWithDatum =
  InitialDistribution $
    (\f -> f (paysPK alicePKH (ada 2)) 10)
      <$> [withDatum @Integer, withInlineDatum, withDatumHash]

-- | An initial distribution where alice owns a UTxO with a
-- reference script corresponding to the always succeed validators
-- and bob owns 2 UTxOs with 100 ada
initialDistributionWithReferenceScript =
  InitialDistribution $
    (paysPK alicePKH (ada 2) `withReferenceScript` alwaysTrueValidator @MockContract)
      : replicate 2 (paysPK bobPKH (ada 100))

getValueFromInitialDatum :: (MonadBlockChain m) => m [Integer]
getValueFromInitialDatum = do
  aliceUtxos <- runUtxoSearch $ utxosAtSearch $ walletAddress alice
  catMaybes <$> mapM (typedDatumFromTxOutRef @Integer . fst) aliceUtxos

spendReferenceAlwaysTrueValidator :: (MonadBlockChain m) => m ()
spendReferenceAlwaysTrueValidator = do
  [(referenceScriptTxOutRef, _)] <- runUtxoSearch $ utxosAtSearch $ walletAddress alice
  tx <-
    validateTxSkel $
      txSkelTemplate
        { txSkelOuts = [paysScript (alwaysTrueValidator @MockContract) () (ada 2)],
          txSkelSigners = [bob]
        }
  let (scriptTxOutRef, _) : _ = utxosFromCardanoTx tx
  void $
    validateTxSkel $
      txSkelTemplate
        { txSkelOuts = [paysPK alicePKH (ada 2)],
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
