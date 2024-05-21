module Cooked.MinAdaSpec where

import Control.Monad
import Cooked
import Data.Default
import Ledger.Index qualified as Ledger
import Optics.Core ((^.))
import Plutus.Script.Utils.Ada qualified as Script
import Prettyprinter qualified as PP
import Test.Tasty
import Test.Tasty.HUnit

heavyDatum :: [Integer]
heavyDatum = take 100 [0 ..]

paymentWithMinAda :: (MonadBlockChain m) => m Integer
paymentWithMinAda = do
  Script.getLovelace . (^. adaL) . outputValue . snd . (!! 0) . utxosFromCardanoTx
    <$> validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelOuts = [paysPK (wallet 2) (0 :: Integer) `withDatum` heavyDatum],
          txSkelSigners = [wallet 1]
        }

paymentWithoutMinAda :: (MonadBlockChain m) => Integer -> m ()
paymentWithoutMinAda paidLovelaces = do
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelOuts = [paysPK (wallet 2) paidLovelaces `withDatum` heavyDatum],
          txSkelSigners = [wallet 1]
        }

tests :: TestTree
tests =
  testGroup
    "MinAda auto adjustment of transaction outputs"
    [ testCase "adjusted transaction passes" $ testSucceeds def paymentWithMinAda,
      testCase "adjusted transaction contains minimal amount"
        $ testFails
          def
          ( \case
              MCEValidationError Ledger.Phase1 _ -> testSuccess
              MCECalcFee (MCEValidationError Ledger.Phase1 _) -> testSuccess
              _ -> testFailure
          )
        $ paymentWithMinAda >>= paymentWithoutMinAda . (+ (-1))
    ]
