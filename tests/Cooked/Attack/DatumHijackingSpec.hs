module Cooked.Attack.DatumHijackingSpec (tests) where

import Control.Monad
import Cooked
import Data.Map qualified as Map
import Data.Set qualified as Set
import Optics.Core
import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import PlutusLedgerApi.V3.Contexts qualified as Api
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter
import Test.Tasty
import Test.Tasty.HUnit

-- * Mock contract for the datum hijacking attack

-- This is a very simple contract: The first transaction locks some
-- Ada to the validator, using the datum 'FirstLock', the second
-- transaction then re-locks the same amount to the same validator,
-- using the datum 'SecondLock'. The datum hijacking attack should
-- target the second transaction, and substitute a different
-- recipient.

data LockDatum = FirstLock | SecondLock deriving (Show, Eq)

instance PrettyCooked LockDatum where
  prettyCooked = viaShow

instance PlutusTx.Eq LockDatum where
  {-# INLINEABLE (==) #-}
  FirstLock == FirstLock = True
  SecondLock == SecondLock = True
  _ == _ = False

PlutusTx.makeLift ''LockDatum
PlutusTx.unstableMakeIsData ''LockDatum

data DHContract

instance Script.MultiPurposeScriptTypes DHContract where
  type SpendingDatumType DHContract = LockDatum

-- ** Transactions (and 'TxSkels') for the datum hijacking attack

lockValue :: Api.Value
lockValue = Script.lovelace 12345678

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
  void $ validateTxSkel $ lockTxSkel oref v

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
  void $ validateTxSkel $ relockTxSkel v oref

datumHijackingTrace :: (MonadBlockChain m) => Script.MultiPurposeScript DHContract -> m ()
datumHijackingTrace v = do
  txLock v
  txRelock v

-- * Validators for the datum hijacking attack

-- | Try to extract a datum from an output.
{-# INLINEABLE outputDatum #-}
outputDatum :: Api.TxInfo -> Api.TxOut -> Maybe LockDatum
outputDatum txi o = case Api.txOutDatum o of
  Api.NoOutputDatum -> Nothing
  Api.OutputDatumHash h -> do
    Api.Datum d <- Api.findDatum h txi
    Api.fromBuiltinData d
  Api.OutputDatum (Api.Datum d) -> Api.fromBuiltinData d

{-# INLINEABLE mockValidatorSpendingPurpose #-}
mockValidatorSpendingPurpose :: (Api.TxInfo -> [Api.TxOut]) -> Script.SpendingPurposeType DHContract
mockValidatorSpendingPurpose getOutputs _ (Just FirstLock) _ txi =
  case getOutputs txi of
    o : _ ->
      PlutusTx.traceIfFalse "not in 'SecondLock'-state after re-locking" (outputDatum txi o PlutusTx.== Just SecondLock)
        && PlutusTx.traceIfFalse "not re-locking the right amout" (Api.txOutValue o == lockValue)
    _ -> PlutusTx.trace "there must be a output re-locked" False
mockValidatorSpendingPurpose _ _ _ _ _ = False

carefulValidator :: Script.MultiPurposeScript DHContract
carefulValidator =
  Script.MultiPurposeScript $
    Script.toScript $$(PlutusTx.compile [||script||])
  where
    script =
      Script.mkMultiPurposeScript $
        Script.falseTypedMultiPurposeScript
          `Script.withSpendingPurpose` mockValidatorSpendingPurpose (\txi -> Api.getContinuingOutputs $ Api.ScriptContext txi (PlutusTx.error ()) (PlutusTx.error ()))

carelessValidator :: Script.MultiPurposeScript DHContract
carelessValidator =
  Script.MultiPurposeScript $
    Script.toScript $$(PlutusTx.compile [||script||])
  where
    script =
      Script.mkMultiPurposeScript $
        Script.falseTypedMultiPurposeScript
          `Script.withSpendingPurpose` mockValidatorSpendingPurpose Api.txInfoOutputs

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
      testCase "careful validator" $
        testFailsInPhase2 $
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
      testCase "careless validator" $
        testSucceeds $
          somewhere
            ( datumHijackingAttackAll @(Script.MultiPurposeScript DHContract)
                ( \(ConcreteOutput v _ d _ _) ->
                    Script.toValidatorHash v == Script.toValidatorHash carelessValidator
                      && d == TxSkelOutInlineDatum SecondLock
                )
                (const True)
                thief
            )
            (datumHijackingTrace carelessValidator)
    ]
