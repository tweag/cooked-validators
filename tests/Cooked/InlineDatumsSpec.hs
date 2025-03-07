{-# LANGUAGE NamedFieldPuns #-}

module Cooked.InlineDatumsSpec where

import Control.Monad
import Cooked
import Data.Default
import Data.Map qualified as Map
import Data.Maybe
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.V3.Typed.Scripts.MultiPurpose qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusLedgerApi.V3.Tx qualified as V3
import PlutusTx qualified
import PlutusTx.AssocMap qualified as PlutusTx (member)
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter
import Test.Tasty
import Test.Tasty.HUnit

data SimpleContractDatum = FirstPaymentDatum | SecondPaymentDatum deriving (Show)

instance PrettyCooked SimpleContractDatum where
  prettyCooked = viaShow

instance PlutusTx.Eq SimpleContractDatum where
  FirstPaymentDatum == FirstPaymentDatum = True
  SecondPaymentDatum == SecondPaymentDatum = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''SimpleContractDatum

{-# INLINEABLE inputDatumSpendingPurpose #-}
inputDatumSpendingPurpose :: Bool -> Script.SpendingScriptType SimpleContractDatum () Api.TxInfo
inputDatumSpendingPurpose requireInlineDatum oRef _ _ Api.TxInfo {txInfoInputs} =
  case PlutusTx.find ((oRef PlutusTx.==) . Api.txInInfoOutRef) txInfoInputs of
    Just (Api.TxInInfo _ Api.TxOut {Api.txOutDatum = inDatum}) | requireInlineDatum -> case inDatum of
      Api.OutputDatum _ -> True
      Api.OutputDatumHash _ -> PlutusTx.trace "I want an inline datum, but I got a hash" False
      Api.NoOutputDatum -> PlutusTx.trace "I want an inline datum, but I got neither a datum nor a hash" False
    Just (Api.TxInInfo _ Api.TxOut {Api.txOutDatum = inDatum}) -> case inDatum of
      Api.OutputDatumHash _ -> True
      Api.OutputDatum _ -> PlutusTx.trace "I want a datum hash, but I got an inline datum" False
      Api.NoOutputDatum -> PlutusTx.trace "I want a datum hash, but I got neither a datum nor a hash" False
    _ -> False

requireInlineDatumInInputValidator :: Script.Versioned Script.Validator
requireInlineDatumInInputValidator = Script.toVersioned $ Script.MultiPurposeScript @() $ Script.toScript $$(PlutusTx.compile [||script||])
  where
    script =
      Script.mkMultiPurposeScript $ Script.falseTypedMultiPurposeScript `Script.withSpendingPurpose` inputDatumSpendingPurpose True

requireHashedDatumInInputValidator :: Script.Versioned Script.Validator
requireHashedDatumInInputValidator = Script.toVersioned $ Script.MultiPurposeScript @() $ Script.toScript $$(PlutusTx.compile [||script||])
  where
    script =
      Script.mkMultiPurposeScript $ Script.falseTypedMultiPurposeScript `Script.withSpendingPurpose` inputDatumSpendingPurpose False

data OutputDatumKind = OnlyHash | Datum | Inline

PlutusTx.makeLift ''OutputDatumKind

-- | This defines three validators: @outputDatumValidator OnlyHash@ is a
-- validator that only returns true if there's a continuing transaction output
-- that has a datum hash that's not included in the 'txInfoData', inline datum,
-- @outputDatumSpendingPurpose Datum@ requires an output datum with a hash that's in
-- the 'txInfoData', and @outputDatumSpendingPurpose Inline@ only returns true if the
-- output has an inline datum.
{-# INLINEABLE outputDatumSpendingPurpose #-}
outputDatumSpendingPurpose :: OutputDatumKind -> Script.SpendingScriptType SimpleContractDatum () Api.TxInfo
outputDatumSpendingPurpose datumKind oRef _ _ Api.TxInfo {txInfoInputs, txInfoOutputs, txInfoData} =
  case PlutusTx.find ((oRef PlutusTx.==) . Api.txInInfoOutRef) txInfoInputs of
    Just (Api.TxInInfo _ Api.TxOut {txOutAddress})
      | [Api.TxOut {txOutDatum}] <- PlutusTx.filter ((txOutAddress PlutusTx.==) . Api.txOutAddress) txInfoOutputs ->
          case (datumKind, txOutDatum) of
            (OnlyHash, Api.OutputDatumHash h) -> PlutusTx.not $ PlutusTx.member h txInfoData
            (Datum, Api.OutputDatumHash h) -> PlutusTx.member h txInfoData
            (Inline, Api.OutputDatum _) -> True
            _ -> False
    _ -> False

requireInlineDatumInOutputValidator :: Script.Versioned Script.Validator
requireInlineDatumInOutputValidator = Script.toVersioned $ Script.MultiPurposeScript @() $ Script.toScript $$(PlutusTx.compile [||script||])
  where
    script =
      Script.mkMultiPurposeScript $ Script.falseTypedMultiPurposeScript `Script.withSpendingPurpose` outputDatumSpendingPurpose Inline

requireHashedDatumInOutputValidator :: Script.Versioned Script.Validator
requireHashedDatumInOutputValidator = Script.toVersioned $ Script.MultiPurposeScript @() $ Script.toScript $$(PlutusTx.compile [||script||])
  where
    script =
      Script.mkMultiPurposeScript $ Script.falseTypedMultiPurposeScript `Script.withSpendingPurpose` outputDatumSpendingPurpose Datum

requireOnlyHashedDatumInOutputValidator :: Script.Versioned Script.Validator
requireOnlyHashedDatumInOutputValidator = Script.toVersioned $ Script.MultiPurposeScript @() $ Script.toScript $$(PlutusTx.compile [||script||])
  where
    script =
      Script.mkMultiPurposeScript $ Script.falseTypedMultiPurposeScript `Script.withSpendingPurpose` outputDatumSpendingPurpose OnlyHash

-- | This defines two single-transaction traces: @listUtxosTestTrace True@ will
-- pay a script with an inline datum, while @listUtxosTestTrace False@ will use
-- a datum hash.
listUtxosTestTrace ::
  (MonadBlockChain m) =>
  Bool ->
  Script.Versioned Script.Validator ->
  m [(V3.TxOutRef, Api.TxOut)]
listUtxosTestTrace useInlineDatum validator =
  utxosFromCardanoTx
    <$> validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelOuts =
            [ Pays $
                ConcreteOutput
                  { concreteOutputOwner = validator,
                    concreteOutputValue = Script.ada 2,
                    concreteOutputReferenceScript = Nothing @(Script.Versioned Script.Script),
                    concreteOutputStakingCredential = Nothing,
                    concreteOutputDatum = (if useInlineDatum then TxSkelOutInlineDatum else TxSkelOutDatum) FirstPaymentDatum
                  }
            ],
          txSkelSigners = [wallet 1]
        }

-- | This defines two traces of two transactions each: @spendOutputTestTrace
-- True@ will pay a validator with an inline datum and try spending the the UTxO
-- thus created, @spendOutputTestTrace False@ will do the same, but use a datum
-- hash.
--
-- This is used to test whether a validator will correctly see the
-- _input data_ of a transaction as inline datums or datum hashes.
spendOutputTestTrace ::
  (MonadBlockChain m) =>
  Bool ->
  Script.Versioned Script.Validator ->
  m ()
spendOutputTestTrace useInlineDatum validator = do
  (theTxOutRef, _) : _ <- listUtxosTestTrace useInlineDatum validator
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelIns = Map.singleton theTxOutRef $ someTxSkelRedeemer (),
          txSkelSigners = [wallet 1]
        }

-- | This defines two traces of two transactions each: On the first transaction,
-- a validator will be paid with an inline datum, on the second transaction,
-- that validator will spend the UTxO created by the first transaction and a new
-- output at the same validator will be created. While
-- @continuingOutputTestTrace True@ will use an inline datum on that second
-- payment to the validator, @continuingOutputTestTrace False@ will use a datum
-- hash.
--
-- This is used to test whether a validator will correctly see the _output data_
-- of atransaction as inline datums or datum hashes.
continuingOutputTestTrace ::
  (MonadBlockChain m) =>
  OutputDatumKind ->
  Script.Versioned Script.Validator ->
  m ()
continuingOutputTestTrace datumKindOnSecondPayment validator = do
  (theTxOutRef, theOutput) : _ <- listUtxosTestTrace True validator
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelIns = Map.singleton theTxOutRef $ someTxSkelRedeemer (),
          txSkelOuts =
            [ Pays $
                ConcreteOutput
                  { concreteOutputOwner = validator,
                    concreteOutputValue = outputValue theOutput,
                    concreteOutputReferenceScript = Nothing @(Script.Versioned Script.Script),
                    concreteOutputStakingCredential = Nothing,
                    concreteOutputDatum =
                      ( case datumKindOnSecondPayment of
                          OnlyHash -> TxSkelOutDatumHash
                          Datum -> TxSkelOutDatum
                          Inline -> TxSkelOutInlineDatum
                      )
                        SecondPaymentDatum
                  }
            ],
          txSkelSigners = [wallet 1]
        }

tests :: TestTree
tests =
  testGroup
    "Inline datums vs. datum hashes"
    [ testCase "the first and second datums have different hashes" $
        assertBool "... they do not" $
          (Script.datumHash . Api.Datum . Api.toBuiltinData $ FirstPaymentDatum)
            /= (Script.datumHash . Api.Datum . Api.toBuiltinData $ SecondPaymentDatum),
      testGroup
        "from the MockChain's point of view on Transaction outputs (allUtxos)"
        -- The validator used in these test cases does not actually matter, we
        -- just need some script to pay to.
        [ testCase "the datum is retrieved correctly" $
            assertBool "... it's not" $
              case fst $ runMockChain (listUtxosTestTrace True requireInlineDatumInInputValidator >> allUtxos) of
                Right (utxos, _endState) ->
                  case mapMaybe ((outputOutputDatum <$>) . isScriptOutputFrom requireInlineDatumInInputValidator . snd) utxos of
                    [Api.OutputDatum _] -> True
                    _ -> False
                _ -> False,
          testCase "the datum hash is retrieved correctly" $
            assertBool "... it's not" $
              case fst $ runMockChain (listUtxosTestTrace False requireInlineDatumInInputValidator >> allUtxos) of
                Right (utxos, _endState) ->
                  case mapMaybe ((outputOutputDatum <$>) . isScriptOutputFrom requireInlineDatumInInputValidator . snd) utxos of
                    [Api.OutputDatumHash _] -> True
                    _ -> False
                _ -> False
        ],
      testGroup
        "from the point of view of scripts"
        [ testGroup
            "looking at transaction inputs"
            [ testGroup
                "validator expects an inline datum..."
                [ testCase "...and gets an inline datum, expecting success" $
                    testSucceeds $
                      spendOutputTestTrace True requireInlineDatumInInputValidator,
                  testCase "...and gets a datum hash, expecting script failure" $
                    testFailsInPhase2 $
                      spendOutputTestTrace False requireInlineDatumInInputValidator
                ],
              testGroup
                "validator expects a datum hash..."
                [ testCase "...and gets an inline datum, expecting script failure" $
                    testFailsInPhase2 $
                      spendOutputTestTrace True requireHashedDatumInInputValidator,
                  testCase "...and gets a datum hash, expecting success" $
                    testSucceeds $
                      spendOutputTestTrace False requireHashedDatumInInputValidator
                ]
            ],
          testGroup
            "looking at transaction outputs"
            [ testGroup
                "validator expects a regular datum..."
                [ testCase "...and gets a regular datum, expecting success" $
                    testSucceeds $
                      continuingOutputTestTrace Datum requireHashedDatumInOutputValidator,
                  testCase "...and gets an inline datum, expecting script failure" $
                    testFailsInPhase2 $
                      continuingOutputTestTrace Inline requireHashedDatumInOutputValidator,
                  testCase "...and gets a datum hash, expecting script failure" $
                    testFailsInPhase2 $
                      continuingOutputTestTrace OnlyHash requireHashedDatumInOutputValidator
                ],
              testGroup
                "validator expects an inline datum..."
                [ testCase "...and gets a regular datum, expecting script failure" $
                    testFailsInPhase2 $
                      continuingOutputTestTrace Datum requireInlineDatumInOutputValidator,
                  testCase "...and gets an inline datum, expecting success" $
                    testSucceeds $
                      continuingOutputTestTrace Inline requireInlineDatumInOutputValidator,
                  testCase "...and gets a datum hash, expecting script failure" $
                    testFailsInPhase2 $
                      continuingOutputTestTrace OnlyHash requireInlineDatumInOutputValidator
                ],
              testGroup
                "validator expects a datum hash..."
                [ testCase "...and gets a regular datum, expecting script failure" $
                    testFailsInPhase2 $
                      continuingOutputTestTrace Datum requireOnlyHashedDatumInOutputValidator,
                  testCase "...and gets an inline datum, expecting script failure" $
                    testFailsInPhase2 $
                      continuingOutputTestTrace Inline requireOnlyHashedDatumInOutputValidator,
                  testCase "...and gets a datum hash, expecting success" $
                    testSucceeds $
                      continuingOutputTestTrace OnlyHash requireOnlyHashedDatumInOutputValidator
                ]
            ]
        ]
    ]
