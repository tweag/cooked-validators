{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Attack.DatumHijacking (tests) where

import Cooked
import Data.Bifunctor
import Data.Map qualified as Map
import Optics.Core
import Plutus.Attack.DatumHijacking
import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import Prettyprinter
import Test.Tasty
import Test.Tasty.HUnit

instance PrettyCooked LockDatum where
  prettyCooked = viaShow

-- ** Transactions (and 'TxSkels') for the datum hijacking attack

lockTxSkel :: Api.TxOutRef -> Script.MultiPurposeScript DHContract -> TxSkel
lockTxSkel o v =
  txSkelTemplate
    { txSkelIns = Map.singleton o emptyTxSkelRedeemer,
      txSkelOuts = [v `receives` (InlineDatum FirstLock <&&> Value lockValue)],
      txSkelSigners = [wallet 1]
    }

txLock :: (MonadBlockChain m) => Script.MultiPurposeScript DHContract -> m Api.TxOutRef
txLock v = do
  (oref, _) : _ <- runUtxoSearch $ utxosOwnedBySearch (wallet 1) `filterWithValuePred` (`Api.geq` lockValue)
  head <$> validateTxSkel' (lockTxSkel oref v)

relockTxSkel :: Script.MultiPurposeScript DHContract -> Api.TxOutRef -> TxSkel
relockTxSkel v o =
  txSkelTemplate
    { txSkelIns = Map.singleton o $ someTxSkelRedeemer (),
      txSkelOuts = [v `receives` (InlineDatum SecondLock <&&> Value lockValue)],
      txSkelSigners = [wallet 1]
    }

txRelock ::
  (MonadBlockChain m) =>
  Script.MultiPurposeScript DHContract ->
  Api.TxOutRef ->
  m ()
txRelock v oref = validateTxSkel_ $ relockTxSkel v oref

datumHijackingTrace :: (MonadBlockChain m) => Script.MultiPurposeScript DHContract -> m ()
datumHijackingTrace v = do
  txLock v >>= txRelock v

txSkelFromOuts :: [TxSkelOut] -> TxSkel
txSkelFromOuts os = txSkelTemplate {txSkelOuts = os, txSkelSigners = [wallet 1]}

-- * TestTree for the datum hijacking attack

thief :: Script.MultiPurposeScript DHContract
thief = Script.trueSpendingMPScript @DHContract

tests :: TestTree
tests =
  testGroup
    "datum hijacking attack"
    [ testGroup "unit tests on a 'TxSkel'" $
        let x1 = Script.lovelace 10001
            x2 = Script.lovelace 10000
            x3 = Script.lovelace 9999
            skelIn =
              txSkelFromOuts
                [ carelessValidator `receives` (InlineDatum SecondLock <&&> Value x1),
                  carelessValidator `receives` (InlineDatum SecondLock <&&> Value x3),
                  carefulValidator `receives` (InlineDatum SecondLock <&&> Value x1),
                  carelessValidator `receives` (InlineDatum FirstLock <&&> Value x2),
                  carelessValidator `receives` (InlineDatum SecondLock <&&> Value x2)
                ]
            skelOut bound select =
              ( fmap (second txSkelOuts)
                  <$> runTweak
                    ( datumHijackingAttack $
                        ( txSkelOutPredDatumHijackingParams
                            ( \out ->
                                preview (txSkelOutOwnerL % userScriptHashAF) out == Just (Script.toScriptHash carelessValidator)
                                  && view txSkelOutDatumL out == SomeTxSkelOutDatum SecondLock Inline
                                  && bound `Api.geq` view txSkelOutValueL out
                            )
                            thief
                        )
                          { dhpAllOutputs = True,
                            dhpIndexPred = select
                          }
                    )
                    skelIn
              )
            outsExpected a b =
              [ carelessValidator `receives` (InlineDatum SecondLock <&&> Value x1),
                a `receives` (InlineDatum SecondLock <&&> Value x3),
                carefulValidator `receives` (InlineDatum SecondLock <&&> Value x1),
                carelessValidator `receives` (InlineDatum FirstLock <&&> Value x2),
                b `receives` (InlineDatum SecondLock <&&> Value x2)
              ]
         in [ testCase "no modified transactions if no interesting outputs to steal" $ [] @=? mcrValue <$> skelOut mempty (const True),
              testCase "one modified transaction for one interesting output" $
                [ Right
                    ( [carelessValidator `receives` (InlineDatum SecondLock <&&> Value x3)],
                      outsExpected thief carelessValidator
                    )
                ]
                  @=? mcrValue <$> skelOut x2 (0 ==),
              testCase "two modified transactions for two interesting outputs" $
                [ Right
                    ( [ carelessValidator `receives` (InlineDatum SecondLock <&&> Value x3),
                        carelessValidator `receives` (InlineDatum SecondLock <&&> Value x2)
                      ],
                      outsExpected thief thief
                    )
                ]
                  @=? mcrValue <$> skelOut x2 (const True),
              testCase "select second interesting output to get one modified transaction" $
                [ Right
                    ( [carelessValidator `receives` (InlineDatum SecondLock <&&> Value x2)],
                      outsExpected carelessValidator thief
                    )
                ]
                  @=? mcrValue <$> skelOut x2 (1 ==)
            ],
      testCooked "careful validator" $
        mustFailInPhase2Test $
          somewhere
            ( datumHijackingAttack $
                ( txSkelOutPredDatumHijackingParams
                    ( \out ->
                        preview (txSkelOutOwnerL % userScriptHashAF) out == Just (Script.toScriptHash carefulValidator)
                          && view txSkelOutDatumL out == SomeTxSkelOutDatum SecondLock Inline
                    )
                    thief
                )
                  { dhpAllOutputs = True
                  }
            )
            (datumHijackingTrace carefulValidator),
      testCooked "careless validator" $
        mustSucceedTest $
          somewhere
            ( datumHijackingAttack $
                ( txSkelOutPredDatumHijackingParams
                    ( \out ->
                        preview (txSkelOutOwnerL % userScriptHashAF) out == Just (Script.toScriptHash carelessValidator)
                          && view txSkelOutDatumL out == SomeTxSkelOutDatum SecondLock Inline
                    )
                    thief
                )
                  { dhpAllOutputs = True
                  }
            )
            (datumHijackingTrace carelessValidator)
    ]
