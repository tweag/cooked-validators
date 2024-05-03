module Cooked.InlineDatumsSpec where

import Control.Arrow
import Control.Monad
import Cooked
import Data.Default
import Data.Map qualified as Map
import Data.Maybe
import Ledger.Tx qualified as Pl (getCardanoTxOutRefs)
import Ledger.Tx.Internal qualified as Pl (getTxOut)
import Plutus.Script.Utils.Ada qualified as Pl
import Plutus.Script.Utils.Scripts qualified as Pl
import Plutus.Script.Utils.Typed qualified as Pl
import Plutus.Script.Utils.V3.Contexts qualified as Pl
import Plutus.Script.Utils.V3.Typed.Scripts qualified as Pl
import PlutusLedgerApi.V3 qualified as Pl
import PlutusTx qualified (compile, makeLift, unstableMakeIsData)
import PlutusTx.Prelude qualified as Pl
import Prettyprinter
import Test.Tasty
import Test.Tasty.HUnit
import Type.Reflection

data SimpleContract

data SimpleContractDatum = FirstPaymentDatum | SecondPaymentDatum deriving (Show)

instance PrettyCooked SimpleContractDatum where
  prettyCooked = viaShow

instance Pl.Eq SimpleContractDatum where
  FirstPaymentDatum == FirstPaymentDatum = True
  SecondPaymentDatum == SecondPaymentDatum = True
  _ == _ = False

PlutusTx.makeLift ''SimpleContractDatum
PlutusTx.unstableMakeIsData ''SimpleContractDatum

instance Pl.ValidatorTypes SimpleContract where
  type RedeemerType SimpleContract = ()
  type DatumType SimpleContract = SimpleContractDatum

-- | This defines two validators: @inputDatumValidator True@ is a validator that
-- only returns true if the UTxO it is asked to spend has an inline datum,
-- @inputDatumValidator False@ only returns true if the UTxO has a datum hash.
inputDatumValidator :: Bool -> Pl.TypedValidator SimpleContract
inputDatumValidator =
  Pl.mkTypedValidatorParam @SimpleContract
    $$(PlutusTx.compile [||val||])
    $$(PlutusTx.compile [||wrap||])
  where
    val :: Bool -> SimpleContractDatum -> () -> Pl.ScriptContext -> Bool
    val requireInlineDatum _ _ ctx =
      let Just (Pl.TxInInfo _ Pl.TxOut {Pl.txOutDatum = inDatum}) = Pl.findOwnInput ctx
       in if requireInlineDatum
            then case inDatum of
              Pl.OutputDatum _ -> True
              Pl.OutputDatumHash _ -> Pl.trace "I want an inline datum, but I got a hash" False
              Pl.NoOutputDatum -> Pl.trace "I want an inline datum, but I got neither a datum nor a hash" False
            else case inDatum of
              Pl.OutputDatumHash _ -> True
              Pl.OutputDatum _ -> Pl.trace "I want a datum hash, but I got an inline datum" False
              Pl.NoOutputDatum -> Pl.trace "I want a datum hash, but I got neither a datum nor a hash" False

    wrap = Pl.mkUntypedValidator

data OutputDatumKind = OnlyHash | Datum | Inline

PlutusTx.makeLift ''OutputDatumKind

-- | This defines three validators: @outputDatumValidator OnlyHash@ is a
-- validator that only returns true if there's a continuing transaction output
-- that has a datum hash that's not included in the 'txInfoData', inline datum,
-- @outputDatumValidator Datum@ requires an output datum with a hash that's in
-- the 'txInfoData', and @outputDatumValidator Inline@ only returns true if the
-- output has an inline datum.
outputDatumValidator :: OutputDatumKind -> Pl.TypedValidator SimpleContract
outputDatumValidator =
  Pl.mkTypedValidatorParam @SimpleContract
    $$(PlutusTx.compile [||val||])
    $$(PlutusTx.compile [||wrap||])
  where
    val :: OutputDatumKind -> SimpleContractDatum -> () -> Pl.ScriptContext -> Bool
    val requiredOutputKind _ _ ctx =
      let [Pl.TxOut {Pl.txOutDatum = outDatum}] = Pl.getContinuingOutputs ctx
          txi = Pl.scriptContextTxInfo ctx
       in case (requiredOutputKind, outDatum) of
            (OnlyHash, Pl.OutputDatumHash h) -> Pl.isNothing (Pl.findDatum h txi)
            (Datum, Pl.OutputDatumHash h) -> Pl.isJust (Pl.findDatum h txi)
            (Inline, Pl.OutputDatum d) -> True
            _ -> False

    wrap = Pl.mkUntypedValidator

-- | This defines two single-transaction traces: @listUtxosTestTrace True@ will
-- pay a script with an inline datum, while @listUtxosTestTrace False@ will use
-- a datum hash.
listUtxosTestTrace ::
  (MonadBlockChain m) =>
  Bool ->
  Pl.TypedValidator SimpleContract ->
  m [(Pl.TxOutRef, Pl.TxOut)]
listUtxosTestTrace useInlineDatum validator =
  utxosFromCardanoTx
    <$> validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelOuts =
            [ ( if useInlineDatum
                  then paysScriptInlineDatum validator FirstPaymentDatum
                  else paysScript validator FirstPaymentDatum
              )
                (Pl.lovelaceValueOf 3_000_000)
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
  forall a m.
  (MonadBlockChain m) =>
  Bool ->
  Pl.TypedValidator SimpleContract ->
  m ()
spendOutputTestTrace useInlineDatum validator = do
  (theTxOutRef, _) : _ <- listUtxosTestTrace useInlineDatum validator
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelIns = Map.singleton theTxOutRef $ TxSkelRedeemerForScript (),
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
  Pl.TypedValidator SimpleContract ->
  m ()
continuingOutputTestTrace datumKindOnSecondPayment validator = do
  (theTxOutRef, theOutput) : _ <- listUtxosTestTrace True validator
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelIns = Map.singleton theTxOutRef $ TxSkelRedeemerForScript (),
          txSkelOuts =
            [ ( case datumKindOnSecondPayment of
                  OnlyHash -> paysScriptDatumHash validator SecondPaymentDatum
                  Datum -> paysScript validator SecondPaymentDatum
                  Inline -> paysScriptInlineDatum validator SecondPaymentDatum
              )
                (outputValue theOutput)
            ],
          txSkelSigners = [wallet 1]
        }

tests :: TestTree
tests =
  testGroup
    "Inline datums vs. datum hashes"
    [ testCase "the first and second datums have different hashes" $
        assertBool "... they do not" $
          (Pl.datumHash . Pl.Datum . Pl.toBuiltinData $ FirstPaymentDatum)
            /= (Pl.datumHash . Pl.Datum . Pl.toBuiltinData $ SecondPaymentDatum),
      testGroup "from the MockChain's point of view on Transaction outputs (allUtxos)" $
        -- The validator used in these test cases does not actually matter, we
        -- just need some script to pay to.
        let theValidator = inputDatumValidator True
         in [ testCase "the datum is retrieved correctly" $
                assertBool "... it's not" $
                  case runMockChain (listUtxosTestTrace True theValidator >> allUtxos) of
                    Right (utxos, _endState) ->
                      case mapMaybe ((outputOutputDatum <$>) . isScriptOutputFrom theValidator . snd) utxos of
                        [Pl.OutputDatum _] -> True
                        _ -> False
                    _ -> False,
              testCase "the datum hash is retrieved correctly" $
                assertBool "... it's not" $
                  case runMockChain (listUtxosTestTrace False theValidator >> allUtxos) of
                    Right (utxos, _endState) ->
                      case mapMaybe ((outputOutputDatum <$>) . isScriptOutputFrom theValidator . snd) utxos of
                        [Pl.OutputDatumHash _] -> True
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
                    testSucceeds def $
                      spendOutputTestTrace True (inputDatumValidator True),
                  testCase "...and gets a datum hash, expecting script failure" $
                    testFails def (isCekEvaluationFailure def) $
                      spendOutputTestTrace False (inputDatumValidator True)
                ],
              testGroup
                "validator expects a datum hash..."
                [ testCase "...and gets an inline datum, expecting script failure" $
                    testFails def (isCekEvaluationFailure def) $
                      spendOutputTestTrace True (inputDatumValidator False),
                  testCase "...and gets a datum hash, expecting success" $
                    testSucceeds def $
                      spendOutputTestTrace False (inputDatumValidator False)
                ]
            ],
          testGroup
            "looking at transaction outputs"
            [ testGroup
                "validator expects a regular datum..."
                [ testCase "...and gets a regular datum, expecting success" $
                    testSucceeds def $
                      continuingOutputTestTrace Datum (outputDatumValidator Datum),
                  testCase "...and gets an inline datum, expecting script failure" $
                    testFails def (isCekEvaluationFailure def) $
                      continuingOutputTestTrace Inline (outputDatumValidator Datum),
                  testCase "...and gets a datum hash, expecting script failure" $
                    testFails def (isCekEvaluationFailure def) $
                      continuingOutputTestTrace OnlyHash (outputDatumValidator Datum)
                ],
              testGroup
                "validator expects an inline datum..."
                [ testCase "...and gets a regular datum, expecting script failure" $
                    testFails def (isCekEvaluationFailure def) $
                      continuingOutputTestTrace Datum (outputDatumValidator Inline),
                  testCase "...and gets an inline datum, expecting success" $
                    testSucceeds def $
                      continuingOutputTestTrace Inline (outputDatumValidator Inline),
                  testCase "...and gets a datum hash, expecting script failure" $
                    testFails def (isCekEvaluationFailure def) $
                      continuingOutputTestTrace OnlyHash (outputDatumValidator Inline)
                ],
              testGroup
                "validator expects a datum hash..."
                [ testCase "...and gets a regular datum, expecting script failure" $
                    testFails def (isCekEvaluationFailure def) $
                      continuingOutputTestTrace Datum (outputDatumValidator OnlyHash),
                  testCase "...and gets an inline datum, expecting script failure" $
                    testFails def (isCekEvaluationFailure def) $
                      continuingOutputTestTrace Inline (outputDatumValidator OnlyHash),
                  testCase "...and gets a datum hash, expecting success" $
                    testSucceeds def $
                      continuingOutputTestTrace OnlyHash (outputDatumValidator OnlyHash)
                ]
            ]
        ]
    ]
