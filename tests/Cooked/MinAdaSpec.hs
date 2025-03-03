module Cooked.MinAdaSpec where

import Control.Monad
import Cooked
import Data.Default
import Optics.Core ((^.))
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import Test.Tasty
import Test.Tasty.HUnit

heavyDatum :: [Integer]
heavyDatum = take 100 [0 ..]

paymentWithMinAda :: (MonadBlockChain m) => m Integer
paymentWithMinAda = do
  Script.getLovelace . (^. Script.adaL) . outputValue . snd . (!! 0) . utxosFromCardanoTx
    <$> validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelOuts = [wallet 2 `receives` TxSkelOutDatum heavyDatum],
          txSkelSigners = [wallet 1]
        }

paymentWithoutMinAda :: (MonadBlockChain m) => Integer -> m ()
paymentWithoutMinAda paidLovelaces = do
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelOuts = [wallet 2 `receives` paymentTemplate {paymentValue = Script.lovelace paidLovelaces, paymentDatum = Just heavyDatum}],
          txSkelSigners = [wallet 1]
        }

tests :: TestTree
tests =
  testGroup
    "MinAda auto adjustment of transaction outputs"
    [ testCase "adjusted transaction passes" $ testSucceeds paymentWithMinAda,
      testCase "adjusted transaction contains minimal amount" $ testFailsInPhase1 $ paymentWithMinAda >>= paymentWithoutMinAda . (+ (-1))
    ]
