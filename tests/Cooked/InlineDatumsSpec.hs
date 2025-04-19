{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.InlineDatumsSpec where

import Cooked
import Data.Map qualified as Map
import Data.Maybe
import Plutus.Script.Utils.V3 qualified as Script
import PlutusCore.Version qualified as PlutusTx
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

data SimpleContract

instance Script.MultiPurposeScriptTypes SimpleContract where
  type SpendingDatumType SimpleContract = SimpleContractDatum

{-# INLINEABLE inputDatumSpendingPurpose #-}
inputDatumSpendingPurpose :: Bool -> Script.SpendingPurposeType SimpleContract
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

compiledInputDatumSpendingPurpose :: PlutusTx.CompiledCode (Bool -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit)
compiledInputDatumSpendingPurpose = $$(PlutusTx.compile [||script||])
  where
    script b = Script.mkMultiPurposeScript $ Script.falseTypedMultiPurposeScript `Script.withSpendingPurpose` inputDatumSpendingPurpose b

requireInlineDatumInInputValidator :: Script.Versioned Script.Validator
requireInlineDatumInInputValidator =
  Script.toVersioned $
    Script.MultiPurposeScript @SimpleContract $
      Script.toScript $
        compiledInputDatumSpendingPurpose `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PlutusTx.plcVersion110 True

requireHashedDatumInInputValidator :: Script.Versioned Script.Validator
requireHashedDatumInInputValidator =
  Script.toVersioned $
    Script.MultiPurposeScript @SimpleContract $
      Script.toScript $
        compiledInputDatumSpendingPurpose `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PlutusTx.plcVersion110 False

PlutusTx.makeLift ''DatumPlacement

-- | This defines three validators: @outputDatumValidator HashedHiddenInTx@ is a
-- validator that only returns true if there's a continuing transaction output
-- that has a datum hash that's not included in the 'txInfoData', inline datum,
-- @outputDatumSpendingPurpose Datum@ requires an output datum with a hash that's in
-- the 'txInfoData', and @outputDatumSpendingPurpose Inline@ only returns true if the
-- output has an inline datum.
{-# INLINEABLE outputDatumSpendingPurpose #-}
outputDatumSpendingPurpose :: DatumPlacement -> Script.SpendingPurposeType SimpleContract
outputDatumSpendingPurpose datumKind oRef _ _ Api.TxInfo {txInfoInputs, txInfoOutputs, txInfoData} =
  case PlutusTx.find ((oRef PlutusTx.==) . Api.txInInfoOutRef) txInfoInputs of
    Just (Api.TxInInfo _ Api.TxOut {txOutAddress})
      | [Api.TxOut {txOutDatum}] <- PlutusTx.filter ((txOutAddress PlutusTx.==) . Api.txOutAddress) txInfoOutputs ->
          case (datumKind, txOutDatum) of
            (HashedHiddenInTx, Api.OutputDatumHash h) -> PlutusTx.not $ PlutusTx.member h txInfoData
            (HashedVisibleInTx, Api.OutputDatumHash h) -> PlutusTx.member h txInfoData
            (Inline, Api.OutputDatum _) -> True
            _ -> False
    _ -> False

compiledOutputDatumSpendingPurpose :: PlutusTx.CompiledCode (DatumPlacement -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit)
compiledOutputDatumSpendingPurpose = $$(PlutusTx.compile [||script||])
  where
    script b = Script.mkMultiPurposeScript $ Script.falseTypedMultiPurposeScript `Script.withSpendingPurpose` outputDatumSpendingPurpose b

requireInlineDatumInOutputValidator :: Script.Versioned Script.Validator
requireInlineDatumInOutputValidator =
  Script.toVersioned $
    Script.MultiPurposeScript @() $
      Script.toScript $
        compiledOutputDatumSpendingPurpose `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PlutusTx.plcVersion110 Inline

requireHashedDatumInOutputValidator :: Script.Versioned Script.Validator
requireHashedDatumInOutputValidator =
  Script.toVersioned $
    Script.MultiPurposeScript @() $
      Script.toScript $
        compiledOutputDatumSpendingPurpose `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PlutusTx.plcVersion110 HashedVisibleInTx

requireHashedHiddenInTxedDatumInOutputValidator :: Script.Versioned Script.Validator
requireHashedHiddenInTxedDatumInOutputValidator =
  Script.toVersioned $
    Script.MultiPurposeScript @() $
      Script.toScript $
        compiledOutputDatumSpendingPurpose `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PlutusTx.plcVersion110 HashedHiddenInTx

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
        { txSkelOuts = [validator `receives` (if useInlineDatum then InlineDatum else VisibleHashedDatum) FirstPaymentDatum],
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
  validateTxSkel_
    txSkelTemplate
      { txSkelIns = Map.singleton theTxOutRef $ someTxSkelRedeemer (),
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
  DatumPlacement ->
  Script.Versioned Script.Validator ->
  m ()
continuingOutputTestTrace datumKindOnSecondPayment validator = do
  (theTxOutRef, theOutput) : _ <- listUtxosTestTrace True validator
  validateTxSkel_
    txSkelTemplate
      { txSkelIns = Map.singleton theTxOutRef $ someTxSkelRedeemer (),
        txSkelOuts =
          [ validator
              `receives` ( Value (outputValue theOutput)
                             <&&> ( case datumKindOnSecondPayment of
                                      HashedHiddenInTx -> HiddenHashedDatum
                                      HashedVisibleInTx -> VisibleHashedDatum
                                      Inline -> InlineDatum
                                  )
                               SecondPaymentDatum
                         )
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
        [ testCooked "the datum is retrieved correctly" $
            mustSucceedTest (listUtxosTestTrace True requireInlineDatumInInputValidator >> allUtxos)
              `withResultProp` \utxos -> testBool $
                case mapMaybe ((outputOutputDatum <$>) . isScriptOutputFrom requireInlineDatumInInputValidator . snd) utxos of
                  [Api.OutputDatum _] -> True
                  _ -> False,
          testCooked "the datum hash is retrieved correctly" $
            mustSucceedTest (listUtxosTestTrace False requireInlineDatumInInputValidator >> allUtxos)
              `withResultProp` \utxos -> testBool $
                case mapMaybe ((outputOutputDatum <$>) . isScriptOutputFrom requireInlineDatumInInputValidator . snd) utxos of
                  [Api.OutputDatumHash _] -> True
                  _ -> False
        ],
      testGroup
        "from the point of view of scripts"
        [ testGroup
            "looking at transaction inputs"
            [ testGroup
                "validator expects an inline datum..."
                [ testCooked "...and gets an inline datum, expecting success" $
                    mustSucceedTest $
                      spendOutputTestTrace True requireInlineDatumInInputValidator,
                  testCooked "...and gets a datum hash, expecting script failure" $
                    mustFailInPhase2Test $
                      spendOutputTestTrace False requireInlineDatumInInputValidator
                ],
              testGroup
                "validator expects a datum hash..."
                [ testCooked "...and gets an inline datum, expecting script failure" $
                    mustFailInPhase2Test $
                      spendOutputTestTrace True requireHashedDatumInInputValidator,
                  testCooked "...and gets a datum hash, expecting success" $
                    mustSucceedTest $
                      spendOutputTestTrace False requireHashedDatumInInputValidator
                ]
            ],
          testGroup
            "looking at transaction outputs"
            [ testGroup
                "validator expects a regular datum..."
                [ testCooked "...and gets a regular datum, expecting success" $
                    mustSucceedTest $
                      continuingOutputTestTrace HashedVisibleInTx requireHashedDatumInOutputValidator,
                  testCooked "...and gets an inline datum, expecting script failure" $
                    mustFailInPhase2Test $
                      continuingOutputTestTrace Inline requireHashedDatumInOutputValidator,
                  testCooked "...and gets a datum hash, expecting script failure" $
                    mustFailInPhase2Test $
                      continuingOutputTestTrace HashedHiddenInTx requireHashedDatumInOutputValidator
                ],
              testGroup
                "validator expects an inline datum..."
                [ testCooked "...and gets a regular datum, expecting script failure" $
                    mustFailInPhase2Test $
                      continuingOutputTestTrace HashedVisibleInTx requireInlineDatumInOutputValidator,
                  testCooked "...and gets an inline datum, expecting success" $
                    mustSucceedTest $
                      continuingOutputTestTrace Inline requireInlineDatumInOutputValidator,
                  testCooked "...and gets a datum hash, expecting script failure" $
                    mustFailInPhase2Test $
                      continuingOutputTestTrace HashedHiddenInTx requireInlineDatumInOutputValidator
                ],
              testGroup
                "validator expects a datum hash..."
                [ testCooked "...and gets a regular datum, expecting script failure" $
                    mustFailInPhase2Test $
                      continuingOutputTestTrace HashedVisibleInTx requireHashedHiddenInTxedDatumInOutputValidator,
                  testCooked "...and gets an inline datum, expecting script failure" $
                    mustFailInPhase2Test $
                      continuingOutputTestTrace Inline requireHashedHiddenInTxedDatumInOutputValidator,
                  testCooked "...and gets a datum hash, expecting success" $
                    mustSucceedTest $
                      continuingOutputTestTrace HashedHiddenInTx requireHashedHiddenInTxedDatumInOutputValidator
                ]
            ]
        ]
    ]
