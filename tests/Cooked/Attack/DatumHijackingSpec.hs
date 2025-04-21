{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.Attack.DatumHijackingSpec (tests) where

import Cooked
import Data.Map qualified as Map
import Data.Set qualified as Set
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

txLock :: (MonadBlockChain m) => Script.MultiPurposeScript DHContract -> m ()
txLock v = do
  (oref, _) : _ <- runUtxoSearch $ utxosAtSearch (wallet 1) `filterWithPred` ((`Api.geq` lockValue) . outputValue)
  validateTxSkel_ $ lockTxSkel oref v

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
  m ()
txRelock v = do
  (oref, _) : _ <-
    runUtxoSearch $
      utxosAtSearch v
        `filterWith` resolveDatum
        `filterWithPure` isOutputWithInlineDatumOfType @LockDatum
        `filterWithPred` ((FirstLock ==) . (^. outputDatumL))
  validateTxSkel_ $ relockTxSkel v oref

datumHijackingTrace :: (MonadBlockChain m) => Script.MultiPurposeScript DHContract -> m ()
datumHijackingTrace v = do
  txLock v
  txRelock v

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
        let val1 = carelessValidator
            val2 = carefulValidator
            x1 = Script.lovelace 10001
            x2 = Script.lovelace 10000
            x3 = Script.lovelace 9999
            skelIn =
              txSkelFromOuts
                [ val1 `receives` (InlineDatum SecondLock <&&> Value x1),
                  val1 `receives` (InlineDatum SecondLock <&&> Value x3),
                  val2 `receives` (InlineDatum SecondLock <&&> Value x1),
                  val1 `receives` (InlineDatum FirstLock <&&> Value x2),
                  val1 `receives` (InlineDatum SecondLock <&&> Value x2)
                ]
            skelOut bound select =
              runTweak
                ( do
                    dhRet <-
                      datumHijackingAttackAll @(Script.MultiPurposeScript DHContract)
                        ( \(ConcreteOutput v _ d x _) ->
                            Script.toValidatorHash val1 == Script.toValidatorHash v
                              && d == TxSkelOutInlineDatum SecondLock
                              && bound `Api.geq` Script.toValue x
                        )
                        select
                        thief
                    return $ (\x -> setValue x $ Script.toValue (x ^. outputValueL)) <$> dhRet
                )
                skelIn
            skelExpected a b =
              txSkelTemplate
                { txSkelLabel =
                    Set.singleton . TxLabel . DatumHijackingLbl $ Script.toCredential $ Script.toVersioned @Script.Script thief,
                  txSkelOuts =
                    [ val1 `receives` (InlineDatum SecondLock <&&> Value x1),
                      a `receives` (InlineDatum SecondLock <&&> Value x3),
                      val2 `receives` (InlineDatum SecondLock <&&> Value x1),
                      val1 `receives` (InlineDatum FirstLock <&&> Value x2),
                      b `receives` (InlineDatum SecondLock <&&> Value x2)
                    ],
                  txSkelSigners = [wallet 1]
                }
         in [ testCase "no modified transactions if no interesting outputs to steal" $ [] @=? fst <$> skelOut mempty (const True),
              testCase "one modified transaction for one interesting output" $
                [ Right
                    ( [ConcreteOutput val1 Nothing (TxSkelOutInlineDatum SecondLock) x3 Nothing],
                      skelExpected thief val1
                    )
                ]
                  @=? fst <$> skelOut x2 (0 ==),
              testCase "two modified transactions for two interesting outputs" $
                [ Right
                    ( [ ConcreteOutput val1 Nothing (TxSkelOutInlineDatum SecondLock) x3 Nothing,
                        ConcreteOutput val1 Nothing (TxSkelOutInlineDatum SecondLock) x2 Nothing
                      ],
                      skelExpected thief thief
                    )
                ]
                  @=? fst <$> skelOut x2 (const True),
              testCase "select second interesting output to get one modified transaction" $
                [ Right
                    ( [ConcreteOutput val1 Nothing (TxSkelOutInlineDatum SecondLock) x2 Nothing],
                      skelExpected val1 thief
                    )
                ]
                  @=? fst <$> skelOut x2 (1 ==)
            ],
      testCooked "careful validator" $
        mustFailInPhase2Test $
          somewhere
            ( datumHijackingAttackAll @(Script.MultiPurposeScript DHContract)
                ( \(ConcreteOutput v _ d _ _) ->
                    Script.toValidatorHash v == Script.toValidatorHash carefulValidator
                      && d == TxSkelOutInlineDatum SecondLock
                )
                (const True)
                thief
            )
            (datumHijackingTrace carefulValidator),
      testCooked "careless validator" $
        mustSucceedTest
          ( somewhere
              ( datumHijackingAttackAll @(Script.MultiPurposeScript DHContract)
                  ( \(ConcreteOutput v _ d _ _) ->
                      Script.toValidatorHash v == Script.toValidatorHash carelessValidator
                        && d == TxSkelOutInlineDatum SecondLock
                  )
                  (const True)
                  thief
              )
              (datumHijackingTrace carelessValidator)
          )
    ]
