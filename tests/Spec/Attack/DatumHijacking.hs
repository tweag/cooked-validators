{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Attack.DatumHijacking (tests) where

import Cooked
import Data.Map qualified as Map
import Data.Set qualified as Set
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
  (oref, _) : _ <- runUtxoSearch $ utxosOwnedBySearch (wallet 1) `filterWithPred` ((`Api.geq` lockValue) . txSkelOutValue)
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
        let val1 = carelessValidator
            val2 = carefulValidator
            x1 = Script.lovelace 10001
            x2 = Script.lovelace 10000
            x3 = Script.lovelace 9999
            mkValue x = TxSkelOutValue x True
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
                ( datumHijackingAttackAll @(Script.MultiPurposeScript DHContract)
                    ( \out@(TxSkelOut _ _ dat value _) ->
                        Just (Script.toValidatorHash val1) == (Script.toValidatorHash <$> txSkelOutValidator out)
                          && dat == TxSkelOutSomeDatum (DatumContent SecondLock) Inline
                          && bound `Api.geq` txSkelOutValueContent value
                    )
                    select
                    thief
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
         in [ testCase "no modified transactions if no interesting outputs to steal" $ [] @=? mcrValue <$> skelOut mempty (const True),
              testCase "one modified transaction for one interesting output" $
                [ Right
                    ( [TxSkelOut val1 Nothing (TxSkelOutSomeDatum (DatumContent SecondLock) Inline) (mkValue x3) TxSkelOutNoReferenceScript],
                      skelExpected thief val1
                    )
                ]
                  @=? mcrValue <$> skelOut x2 (0 ==),
              testCase "two modified transactions for two interesting outputs" $
                [ Right
                    ( [ TxSkelOut val1 Nothing (TxSkelOutSomeDatum (DatumContent SecondLock) Inline) (mkValue x3) TxSkelOutNoReferenceScript,
                        TxSkelOut val1 Nothing (TxSkelOutSomeDatum (DatumContent SecondLock) Inline) (mkValue x2) TxSkelOutNoReferenceScript
                      ],
                      skelExpected thief thief
                    )
                ]
                  @=? mcrValue <$> skelOut x2 (const True),
              testCase "select second interesting output to get one modified transaction" $
                [ Right
                    ( [TxSkelOut val1 Nothing (TxSkelOutSomeDatum (DatumContent SecondLock) Inline) (mkValue x2) TxSkelOutNoReferenceScript],
                      skelExpected val1 thief
                    )
                ]
                  @=? mcrValue <$> skelOut x2 (1 ==)
            ],
      testCooked "careful validator" $
        mustFailInPhase2Test $
          somewhere
            ( datumHijackingAttackAll @(Script.MultiPurposeScript DHContract)
                ( \out@(TxSkelOut _ _ d _ _) ->
                    Just (Script.toValidatorHash carefulValidator) == (Script.toValidatorHash <$> txSkelOutValidator out)
                      && d == TxSkelOutSomeDatum (DatumContent SecondLock) Inline
                )
                (const True)
                thief
            )
            (datumHijackingTrace carefulValidator),
      testCooked "careless validator" $
        mustSucceedTest
          ( somewhere
              ( datumHijackingAttackAll @(Script.MultiPurposeScript DHContract)
                  ( \out@(TxSkelOut _ _ d _ _) ->
                      Just (Script.toValidatorHash carelessValidator) == (Script.toValidatorHash <$> txSkelOutValidator out)
                        && d == TxSkelOutSomeDatum (DatumContent SecondLock) Inline
                  )
                  (const True)
                  thief
              )
              (datumHijackingTrace carelessValidator)
          )
    ]
