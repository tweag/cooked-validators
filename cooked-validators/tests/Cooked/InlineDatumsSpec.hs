{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.InlineDatumsSpec where

import Control.Arrow
import Control.Monad
import Cooked
import Cooked.Tx.Constraints.Type
import Data.Default
import qualified Data.Map as Map
import Data.Maybe
import qualified Ledger.Ada as Pl
import qualified Ledger.Tx as Pl (getCardanoTxOutRefs)
import qualified Ledger.Tx.CardanoAPI.Internal as Pl (fromCardanoTxOutToPV2TxInfoTxOut)
import qualified Ledger.Tx.Internal as Pl (getTxOut)
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import qualified Plutus.V2.Ledger.Contexts as Pl
import qualified PlutusTx as Pl
import qualified PlutusTx.Trace as Pl
import Test.Tasty
import Test.Tasty.HUnit
import Type.Reflection

data SimpleContract

instance Pl.ValidatorTypes SimpleContract where
  type RedeemerType SimpleContract = ()
  type DatumType SimpleContract = ()

-- | This defines two validators: @inputDatumValidator True@ is a validator that
-- only returns true if the UTxO it is asked to spend has an inline datum,
-- @inputDatumValidator False@ only returns true if the UTxO has a datum hash.
inputDatumValidator :: Bool -> Pl.TypedValidator SimpleContract
inputDatumValidator =
  Pl.mkTypedValidatorParam @SimpleContract
    $$(Pl.compile [||val||])
    $$(Pl.compile [||wrap||])
  where
    val :: Bool -> () -> () -> Pl.ScriptContext -> Bool
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

    wrap = Pl.mkUntypedValidator @() @()

-- | This defines two validators: @outputDatumValidator True@ is a validator
-- that only returns true if there's a continuing transaction output that has an
-- inline datum, @outputDatumValidator False@ only returns true if the output
-- has a datum hash.
outputDatumValidator :: Bool -> Pl.TypedValidator SimpleContract
outputDatumValidator =
  Pl.mkTypedValidatorParam @SimpleContract
    $$(Pl.compile [||val||])
    $$(Pl.compile [||wrap||])
  where
    val :: Bool -> () -> () -> Pl.ScriptContext -> Bool
    val requireInlineDatum _ _ ctx =
      let [Pl.TxOut {Pl.txOutDatum = outDatum}] = Pl.getContinuingOutputs ctx
       in if requireInlineDatum
            then case outDatum of
              Pl.OutputDatum _ -> True
              Pl.OutputDatumHash _ -> Pl.trace "I want an inline datum, but I got a hash" False
              Pl.NoOutputDatum -> Pl.trace "I want an inline datum, but I got neither a datum nor a hash" False
            else case outDatum of
              Pl.OutputDatumHash _ -> True
              Pl.OutputDatum _ -> Pl.trace "I want a datum hash, but I got an inline datum" False
              Pl.NoOutputDatum -> Pl.trace "I want a datum hash, but I got neither a datum nor a hash" False

    wrap = Pl.mkUntypedValidator @() @()

-- | This defines two single-transaction traces: @listUtxosTestTrace True@ will
-- pay a script with an inline datum, while @listUtxosTestTrace False@ will use
-- a datum hash.
listUtxosTestTrace ::
  ( MonadBlockChain m,
    Show (Pl.DatumType a),
    Pl.ToData (Pl.DatumType a),
    Pl.FromData (Pl.DatumType a),
    Typeable a,
    Default (Pl.DatumType a),
    Typeable (Pl.DatumType a)
  ) =>
  Bool ->
  Pl.TypedValidator a ->
  m [(Pl.TxOutRef, Pl.TxOut)]
listUtxosTestTrace useInlineDatum validator =
  utxosFromCardanoTx
    <$> validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {adjustUnbalTx = True},
          txSkelOuts =
            [ ( if useInlineDatum
                  then paysScriptInlineDatum validator def
                  else paysScript validator def
              )
                (Pl.lovelaceValueOf 3_000_000)
            ]
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
  ( MonadBlockChain m,
    SpendsScriptConstrs a,
    Show (Pl.DatumType a),
    Pl.ToData (Pl.DatumType a),
    Pl.FromData (Pl.DatumType a),
    Typeable a,
    Default (Pl.DatumType a),
    Typeable (Pl.DatumType a),
    Default (Pl.RedeemerType a)
  ) =>
  Bool ->
  Pl.TypedValidator a ->
  m ()
spendOutputTestTrace useInlineDatum validator = do
  (theTxOutRef, _) : _ <- listUtxosTestTrace useInlineDatum validator
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelOpts =
            def
              { adjustUnbalTx = True
              -- unsafeModTx = [RawModTxAfterBalancing Debug.traceShowId]
              },
          txSkelIns =
            Map.singleton
              theTxOutRef
              TxSkelNoRedeemerForScript
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
  ( MonadBlockChain m,
    SpendsScriptConstrs a,
    Show (Pl.DatumType a),
    Pl.ToData (Pl.DatumType a),
    Pl.FromData (Pl.DatumType a),
    Typeable a,
    Default (Pl.DatumType a),
    Typeable (Pl.DatumType a),
    Default (Pl.RedeemerType a)
  ) =>
  Bool ->
  Pl.TypedValidator a ->
  m ()
continuingOutputTestTrace useInlineDatumOnSecondPayment validator = do
  (theTxOutRef, theOutput) : _ <- listUtxosTestTrace True validator
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {adjustUnbalTx = True},
          txSkelIns =
            Map.singleton
              theTxOutRef
              TxSkelNoRedeemerForScript,
          txSkelOuts =
            [ ( if useInlineDatumOnSecondPayment
                  then paysScriptInlineDatum validator def
                  else paysScript validator def
              )
                (outputValue theOutput)
            ]
        }

tests :: TestTree
tests =
  testGroup
    "inline datums vs. datum hashes"
    [ testGroup "from the MockChain's point of view on Transaction outputs (allUtxos)" $
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
                    testSucceeds $
                      spendOutputTestTrace True (inputDatumValidator True),
                  testCase "...and gets a datum hash, expecting script failure" $
                    testFailsFrom' isCekEvaluationFailure def $
                      spendOutputTestTrace False (inputDatumValidator True)
                ],
              testGroup
                "validator expects a datum hash..."
                [ testCase "...and gets an inline datum, expecting script failure" $
                    testFailsFrom' isCekEvaluationFailure def $
                      spendOutputTestTrace True (inputDatumValidator False),
                  testCase "...and gets a datum hash, expecting success" $
                    testSucceeds $
                      spendOutputTestTrace False (inputDatumValidator False)
                ]
            ],
          testGroup
            "looking at transaction outputs"
            [ testGroup
                "validator expects an inline datum..."
                [ testCase "...and gets an inline datum, expecting success" $
                    testSucceeds $
                      continuingOutputTestTrace True (outputDatumValidator True),
                  testCase "...and gets a datum hash, expecting script failure" $
                    testFailsFrom' isCekEvaluationFailure def $
                      continuingOutputTestTrace False (outputDatumValidator True)
                ],
              testGroup
                "validator expects a datum hash..."
                [ testCase "...and gets an inline datum, expecting script failure" $
                    testFailsFrom' isCekEvaluationFailure def $
                      continuingOutputTestTrace True (outputDatumValidator False),
                  testCase "...and gets a datum hash, expecting success" $
                    testSucceeds $
                      continuingOutputTestTrace False (outputDatumValidator False)
                ]
            ]
        ]
    ]
