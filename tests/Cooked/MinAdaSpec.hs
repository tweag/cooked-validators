module Cooked.MinAdaSpec where

import Control.Monad
import Cooked
import Data.Default
import Ledger.Index qualified as Pl
import Optics.Core ((^.))
import Plutus.Script.Utils.Ada qualified as Pl
import Plutus.Script.Utils.Scripts qualified as Pl
import PlutusLedgerApi.V3 qualified as Pl hiding (getLovelace)
import Prettyprinter qualified as PP
import Test.Tasty
import Test.Tasty.HUnit

heavyDatum :: [Integer]
heavyDatum = take 100 [0 ..]

paymentWithMinAda :: (MonadBlockChain m) => m Integer
paymentWithMinAda = do
  Pl.getLovelace . (^. adaL) . outputValue . snd . (!! 0) . utxosFromCardanoTx
    <$> validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelOuts =
            [ paysPK
                (walletPKHash $ wallet 2)
                mempty
                `withDatum` heavyDatum
            ],
          txSkelSigners = [wallet 1]
        }

paymentWithoutMinAda :: (MonadBlockChain m) => Integer -> m ()
paymentWithoutMinAda paidLovelaces = do
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelOuts =
            [ paysPK
                (walletPKHash $ wallet 2)
                (Pl.lovelaceValueOf paidLovelaces)
                `withDatum` heavyDatum
            ],
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
              MCEValidationError Pl.Phase1 _ -> testSuccess
              MCECalcFee (MCEValidationError Pl.Phase1 _) -> testSuccess
              _ -> testFailure
          )
        $ paymentWithMinAda >>= paymentWithoutMinAda . (+ (-1))
    ]
