{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.MinAdaSpec where

import Control.Monad
import Cooked
import Data.Default
import qualified Ledger.Index as Pl
import Optics.Core ((^.))
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Plutus.Script.Utils.Scripts as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import qualified Prettyprinter as PP
import Test.Tasty
import Test.Tasty.HUnit

heavyDatum :: [Integer]
heavyDatum = take 100 [0 ..]

paymentWithMinAda :: MonadBlockChain m => m Integer
paymentWithMinAda = do
  Pl.getLovelace . (^. adaL) . outputValue . snd . (!! 0) . utxosFromCardanoTx
    <$> validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelOuts =
            [ Pays $
                ConcreteOutput
                  (walletPKHash $ wallet 2)
                  Nothing
                  mempty
                  (TxSkelOutDatum heavyDatum)
                  (Nothing @(Pl.Versioned Pl.Script))
            ],
          txSkelSigners = [wallet 1]
        }

paymentWithoutMinAda :: MonadBlockChain m => Integer -> m ()
paymentWithoutMinAda paidLovelaces = do
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelOuts =
            [ Pays $
                ConcreteOutput
                  (walletPKHash $ wallet 2)
                  Nothing
                  (Pl.lovelaceValueOf paidLovelaces)
                  (TxSkelOutDatum heavyDatum)
                  (Nothing @(Pl.Versioned Pl.Script))
            ],
          txSkelSigners = [wallet 1]
        }

tests :: TestTree
tests =
  testGroup
    "automatic minAda adjustment of transaction outputs"
    [ testCase "adjusted transaction passes" $ testSucceeds def paymentWithMinAda,
      testCase "adjusted transaction contains minimal amount" $
        testFailsFrom'
          def
          ( \case
              MCEValidationError (Pl.Phase1, _) -> testSuccess
              MCECalcFee (MCEValidationError (Pl.Phase1, _)) -> testSuccess
              _ -> testFailure
          )
          def
          $ paymentWithMinAda >>= paymentWithoutMinAda . (+ (-1))
    ]
