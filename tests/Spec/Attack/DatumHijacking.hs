{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Attack.DatumHijacking (tests) where

import Cooked
import Data.Map qualified as Map
import Optics.Core
import Plutus.Attack.DatumHijacking
import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import Polysemy
import Polysemy.NonDet
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
      txSkelOuts = [v `receives` InlineDatum FirstLock <&&> Value lockValue],
      txSkelSignatories = txSkelSignatoriesFromList [wallet 1]
    }

txLock :: Script.MultiPurposeScript DHContract -> StagedMockChain Api.TxOutRef
txLock v = do
  oref : _ <- getTxOutRefs $ utxosAtSearch (wallet 1) $ ensureAFoldIs (txSkelOutValueL % filtered (`Api.geq` lockValue))
  fst . head <$> validateTxSkel' (lockTxSkel oref v)

relockTxSkel :: Script.MultiPurposeScript DHContract -> Api.TxOutRef -> TxSkel
relockTxSkel v o =
  txSkelTemplate
    { txSkelIns = Map.singleton o $ someTxSkelRedeemer (),
      txSkelOuts = [v `receives` InlineDatum SecondLock <&&> Value lockValue],
      txSkelSignatories = txSkelSignatoriesFromList [wallet 1]
    }

txRelock ::
  Script.MultiPurposeScript DHContract ->
  Api.TxOutRef ->
  StagedMockChain ()
txRelock v oref = validateTxSkel_ $ relockTxSkel v oref

datumHijackingTrace :: Script.MultiPurposeScript DHContract -> StagedMockChain ()
datumHijackingTrace v = do
  txLock v >>= txRelock v

-- * TestTree for the datum hijacking attack

thief :: Script.MultiPurposeScript DHContract
thief = Script.trueSpendingMPScript @DHContract

tests :: TestTree
tests =
  testGroup
    "datum hijacking attack"
    [ testGroup "unit tests on a 'TxSkel'" $
        let value_10_001 = Script.lovelace 10_001
            value_10_000 = Script.lovelace 10000
            value_9_999 = Script.lovelace 9999
            inSkel =
              txSkelTemplate
                { txSkelOuts =
                    [ carelessValidator `receives` InlineDatum SecondLock <&&> Value value_10_001,
                      carelessValidator `receives` InlineDatum SecondLock <&&> Value value_9_999,
                      carefulValidator `receives` InlineDatum SecondLock <&&> Value value_10_001,
                      carelessValidator `receives` InlineDatum FirstLock <&&> Value value_10_000,
                      carelessValidator `receives` InlineDatum SecondLock <&&> Value value_10_000
                    ],
                  txSkelSignatories = txSkelSignatoriesFromList [wallet 1]
                }
            outSkelOutputs :: Api.Value -> (Integer -> Bool) -> [[TxSkelOut]]
            outSkelOutputs bound select =
              (fmap txSkelOuts . run . runNonDet . execTweak inSkel)
                ( datumHijackingAttack $
                    ( outPredDatumHijackingParams
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
            outsExpected a b =
              [ carelessValidator `receives` InlineDatum SecondLock <&&> Value value_10_001,
                a `receives` InlineDatum SecondLock <&&> Value value_9_999,
                carefulValidator `receives` InlineDatum SecondLock <&&> Value value_10_001,
                carelessValidator `receives` InlineDatum FirstLock <&&> Value value_10_000,
                b `receives` InlineDatum SecondLock <&&> Value value_10_000
              ]
         in [ testCase "no modified transactions if no interesting outputs to steal" $
                [] @=? outSkelOutputs mempty (const True),
              testCase "one modified transaction for one interesting output" $
                [outsExpected thief carelessValidator]
                  @=? outSkelOutputs value_10_000 (0 ==),
              testCase "two modified transactions for two interesting outputs" $
                [outsExpected thief thief]
                  @=? outSkelOutputs value_10_000 (const True),
              testCase "select second interesting output to get one modified transaction" $
                [outsExpected carelessValidator thief]
                  @=? outSkelOutputs value_10_000 (1 ==)
            ],
      testCooked "careful validator" $
        mustFailInPhase2Test $
          somewhere
            ( datumHijackingAttack $
                ( outPredDatumHijackingParams
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
                ( outPredDatumHijackingParams
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
