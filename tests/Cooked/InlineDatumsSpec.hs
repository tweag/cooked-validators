module Cooked.InlineDatumsSpec where

import Control.Monad
import Cooked
import Data.Default
import Data.Map qualified as Map
import Data.Maybe
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Typed qualified as Script
import Plutus.Script.Utils.V3.Typed.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusLedgerApi.V3.Contexts qualified as Api
import PlutusTx qualified
import PlutusTx.AssocMap qualified as PlutusTx
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter
import Test.Tasty
import Test.Tasty.HUnit

data SimpleContract

data SimpleContractDatum = FirstPaymentDatum | SecondPaymentDatum deriving (Show)

instance PrettyCooked SimpleContractDatum where
  prettyCooked = viaShow

instance PlutusTx.Eq SimpleContractDatum where
  FirstPaymentDatum == FirstPaymentDatum = True
  SecondPaymentDatum == SecondPaymentDatum = True
  _ == _ = False

PlutusTx.makeLift ''SimpleContractDatum
PlutusTx.unstableMakeIsData ''SimpleContractDatum

instance Script.ValidatorTypes SimpleContract where
  type RedeemerType SimpleContract = ()
  type DatumType SimpleContract = SimpleContractDatum

hashInPKValidator :: Api.TxOutRef -> Script.TypedValidator MockContract
hashInPKValidator =
  Script.mkTypedValidatorParam @MockContract
    $$(PlutusTx.compile [||val||])
    $$(PlutusTx.compile [||wrap||])
  where
    go :: Api.TxOutRef -> [Api.TxInInfo] -> Maybe Api.TxOut
    go _ [] = Nothing
    go oRef (Api.TxInInfo oRef' txOut : tl) =
      if oRef PlutusTx.== oRef'
        then Just txOut
        else go oRef tl

    val :: Api.TxOutRef -> () -> () -> Api.ScriptContext -> Bool
    val oRef () () ctx =
      let inputs = Api.txInfoInputs $ Api.scriptContextTxInfo ctx
       in case go oRef inputs of
            Nothing -> False
            Just (Api.TxOut _ _ (Api.OutputDatumHash hash) _) ->
              let hashes = Api.txInfoData $ Api.scriptContextTxInfo ctx
               in case PlutusTx.lookup hash hashes of
                    Nothing -> PlutusTx.traceError "NO DATUM IN MAP"
                    Just (Api.Datum dat) -> 10 PlutusTx.== PlutusTx.unsafeFromBuiltinData @Integer dat
            _ -> False

    wrap = Script.mkUntypedValidator

hm :: (MonadBlockChain m) => m ()
hm = do
  (oRef : _) <-
    validateTxSkel' $
      txSkelTemplate
        { txSkelOuts = [paysPK (wallet 1) (Script.ada 2) `withDatum` (10 :: Integer)],
          txSkelSigners = [wallet 1]
        }
  (scriptORef : _) <-
    validateTxSkel' $
      txSkelTemplate
        { txSkelOuts = [paysScript (hashInPKValidator oRef) () (Script.ada 2)],
          txSkelSigners = [wallet 1]
        }
  validateTxSkel_ $
    txSkelTemplate
      { txSkelIns = Map.fromList [(oRef, emptyTxSkelRedeemer), (scriptORef, emptyTxSkelRedeemer)],
        txSkelSigners = [wallet 1]
      }

-- | This defines two validators: @inputDatumValidator True@ is a validator that
-- only returns true if the UTxO it is asked to spend has an inline datum,
-- @inputDatumValidator False@ only returns true if the UTxO has a datum hash.
inputDatumValidator :: Bool -> Script.TypedValidator SimpleContract
inputDatumValidator =
  Script.mkTypedValidatorParam @SimpleContract
    $$(PlutusTx.compile [||val||])
    $$(PlutusTx.compile [||wrap||])
  where
    val :: Bool -> SimpleContractDatum -> () -> Api.ScriptContext -> Bool
    val requireInlineDatum _ _ ctx =
      case Api.findOwnInput ctx of
        Just (Api.TxInInfo _ Api.TxOut {Api.txOutDatum = inDatum}) ->
          if requireInlineDatum
            then case inDatum of
              Api.OutputDatum _ -> True
              Api.OutputDatumHash _ -> PlutusTx.trace "I want an inline datum, but I got a hash" False
              Api.NoOutputDatum -> PlutusTx.trace "I want an inline datum, but I got neither a datum nor a hash" False
            else case inDatum of
              Api.OutputDatumHash _ -> True
              Api.OutputDatum _ -> PlutusTx.trace "I want a datum hash, but I got an inline datum" False
              Api.NoOutputDatum -> PlutusTx.trace "I want a datum hash, but I got neither a datum nor a hash" False
        Nothing -> False

    wrap = Script.mkUntypedValidator

data OutputDatumKind = OnlyHash | Datum | Inline

PlutusTx.makeLift ''OutputDatumKind

-- | This defines three validators: @outputDatumValidator OnlyHash@ is a
-- validator that only returns true if there's a continuing transaction output
-- that has a datum hash that's not included in the 'txInfoData', inline datum,
-- @outputDatumValidator Datum@ requires an output datum with a hash that's in
-- the 'txInfoData', and @outputDatumValidator Inline@ only returns true if the
-- output has an inline datum.
outputDatumValidator :: OutputDatumKind -> Script.TypedValidator SimpleContract
outputDatumValidator =
  Script.mkTypedValidatorParam @SimpleContract
    $$(PlutusTx.compile [||val||])
    $$(PlutusTx.compile [||wrap||])
  where
    val :: OutputDatumKind -> SimpleContractDatum -> () -> Api.ScriptContext -> Bool
    val requiredOutputKind _ _ ctx =
      case Api.getContinuingOutputs ctx of
        [Api.TxOut {Api.txOutDatum = outDatum}] ->
          let txi = Api.scriptContextTxInfo ctx
           in case (requiredOutputKind, outDatum) of
                (OnlyHash, Api.OutputDatumHash h) -> PlutusTx.isNothing (Api.findDatum h txi)
                (Datum, Api.OutputDatumHash h) -> PlutusTx.isJust (Api.findDatum h txi)
                (Inline, Api.OutputDatum _) -> True
                _ -> False
        _ -> False

    wrap = Script.mkUntypedValidator

-- | This defines two single-transaction traces: @listUtxosTestTrace True@ will
-- pay a script with an inline datum, while @listUtxosTestTrace False@ will use
-- a datum hash.
listUtxosTestTrace ::
  (MonadBlockChain m) =>
  Bool ->
  Script.TypedValidator SimpleContract ->
  m [(Api.TxOutRef, Api.TxOut)]
listUtxosTestTrace useInlineDatum validator =
  utxosFromCardanoTx
    <$> validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelOuts = [(if useInlineDatum then paysScriptInlineDatum validator else paysScript validator) FirstPaymentDatum (Script.ada 3)],
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
  Script.TypedValidator SimpleContract ->
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
  Script.TypedValidator SimpleContract ->
  m ()
continuingOutputTestTrace datumKindOnSecondPayment validator = do
  (theTxOutRef, theOutput) : _ <- listUtxosTestTrace True validator
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelIns = Map.singleton theTxOutRef $ someTxSkelRedeemer (),
          txSkelOuts =
            [ ( case datumKindOnSecondPayment of
                  OnlyHash -> paysScriptUnresolvedDatumHash validator SecondPaymentDatum
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
          (Script.datumHash . Api.Datum . Api.toBuiltinData $ FirstPaymentDatum)
            /= (Script.datumHash . Api.Datum . Api.toBuiltinData $ SecondPaymentDatum),
      testGroup "from the MockChain's point of view on Transaction outputs (allUtxos)" $
        -- The validator used in these test cases does not actually matter, we
        -- just need some script to pay to.
        let theValidator = inputDatumValidator True
         in [ testCase "the datum is retrieved correctly" $
                assertBool "... it's not" $
                  case fst $ runMockChain (listUtxosTestTrace True theValidator >> allUtxos) of
                    Right (utxos, _endState) ->
                      case mapMaybe ((outputOutputDatum <$>) . isScriptOutputFrom theValidator . snd) utxos of
                        [Api.OutputDatum _] -> True
                        _ -> False
                    _ -> False,
              testCase "the datum hash is retrieved correctly" $
                assertBool "... it's not" $
                  case fst $ runMockChain (listUtxosTestTrace False theValidator >> allUtxos) of
                    Right (utxos, _endState) ->
                      case mapMaybe ((outputOutputDatum <$>) . isScriptOutputFrom theValidator . snd) utxos of
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
                      spendOutputTestTrace True (inputDatumValidator True),
                  testCase "...and gets a datum hash, expecting script failure" $
                    testFailsInPhase2 $
                      spendOutputTestTrace False (inputDatumValidator True)
                ],
              testGroup
                "validator expects a datum hash..."
                [ testCase "...and gets an inline datum, expecting script failure" $
                    testFailsInPhase2 $
                      spendOutputTestTrace True (inputDatumValidator False),
                  testCase "...and gets a datum hash, expecting success" $
                    testSucceeds $
                      spendOutputTestTrace False (inputDatumValidator False)
                ]
            ],
          testGroup
            "looking at transaction outputs"
            [ testGroup
                "validator expects a regular datum..."
                [ testCase "...and gets a regular datum, expecting success" $
                    testSucceeds $
                      continuingOutputTestTrace Datum (outputDatumValidator Datum),
                  testCase "...and gets an inline datum, expecting script failure" $
                    testFailsInPhase2 $
                      continuingOutputTestTrace Inline (outputDatumValidator Datum),
                  testCase "...and gets a datum hash, expecting script failure" $
                    testFailsInPhase2 $
                      continuingOutputTestTrace OnlyHash (outputDatumValidator Datum)
                ],
              testGroup
                "validator expects an inline datum..."
                [ testCase "...and gets a regular datum, expecting script failure" $
                    testFailsInPhase2 $
                      continuingOutputTestTrace Datum (outputDatumValidator Inline),
                  testCase "...and gets an inline datum, expecting success" $
                    testSucceeds $
                      continuingOutputTestTrace Inline (outputDatumValidator Inline),
                  testCase "...and gets a datum hash, expecting script failure" $
                    testFailsInPhase2 $
                      continuingOutputTestTrace OnlyHash (outputDatumValidator Inline)
                ],
              testGroup
                "validator expects a datum hash..."
                [ testCase "...and gets a regular datum, expecting script failure" $
                    testFailsInPhase2 $
                      continuingOutputTestTrace Datum (outputDatumValidator OnlyHash),
                  testCase "...and gets an inline datum, expecting script failure" $
                    testFailsInPhase2 $
                      continuingOutputTestTrace Inline (outputDatumValidator OnlyHash),
                  testCase "...and gets a datum hash, expecting success" $
                    testSucceeds $
                      continuingOutputTestTrace OnlyHash (outputDatumValidator OnlyHash)
                ]
            ]
        ]
    ]
