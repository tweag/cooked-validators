{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.Behaviour.InlineDatumsSpec where

import Control.Monad (void)
import Cooked
import qualified Cooked.Behaviour.Validators as Validators
import Data.Default
import qualified Data.Map as Map
import Data.Maybe
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Plutus.Script.Utils.Typed as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import Test.Tasty
import Test.Tasty.HUnit

-- | This defines two single-transaction traces: @listUtxosTestTrace True@ will
-- pay a script with an inline datum, while @listUtxosTestTrace False@ will use
-- a datum hash.
listUtxosTestTrace ::
  (MonadBlockChain m) =>
  Bool ->
  Pl.TypedValidator Validators.Unit ->
  m [(Pl.TxOutRef, Pl.TxOut)]
listUtxosTestTrace useInlineDatum validator =
  utxosFromCardanoTx
    <$> validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelOuts =
            [ ( if useInlineDatum
                  then paysScriptInlineDatum validator ()
                  else paysScript validator ()
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
  forall m.
  (MonadBlockChain m) =>
  Bool ->
  Pl.TypedValidator Validators.Unit ->
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
  Validators.DatumKind ->
  Pl.TypedValidator Validators.Unit ->
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
                  Validators.OnlyHash -> paysScriptDatumHash validator ()
                  Validators.ResolvedHash -> paysScript validator ()
                  Validators.Inline -> paysScriptInlineDatum validator ()
              )
                (outputValue theOutput)
            ],
          txSkelSigners = [wallet 1]
        }

tests :: TestTree
tests =
  testGroup
    "Inline datums vs. datum hashes"
    [ testGroup "from the MockChain's point of view on Transaction outputs (allUtxos)" $
        -- The validator used in these test cases does not actually matter, we
        -- just need some script to pay to.
        let theValidator = Validators.inputDatum True
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
                      spendOutputTestTrace True (Validators.inputDatum True),
                  testCase "...and gets a datum hash, expecting script failure" $
                    testFails def (isCekEvaluationFailure def) $
                      spendOutputTestTrace False (Validators.inputDatum True)
                ],
              testGroup
                "validator expects a datum hash..."
                [ testCase "...and gets an inline datum, expecting script failure" $
                    testFails def (isCekEvaluationFailure def) $
                      spendOutputTestTrace True (Validators.inputDatum False),
                  testCase "...and gets a datum hash, expecting success" $
                    testSucceeds def $
                      spendOutputTestTrace False (Validators.inputDatum False)
                ]
            ],
          testGroup
            "looking at transaction outputs"
            [ testGroup
                "validator expects a regular datum..."
                [ testCase "...and gets a regular datum, expecting success" $
                    testSucceeds def $
                      continuingOutputTestTrace
                        Validators.ResolvedHash
                        (Validators.continuingDatum Validators.ResolvedHash),
                  testCase "...and gets an inline datum, expecting script failure" $
                    testFails def (isCekEvaluationFailure def) $
                      continuingOutputTestTrace
                        Validators.Inline
                        (Validators.continuingDatum Validators.ResolvedHash),
                  testCase "...and gets a datum hash, expecting script failure" $
                    testFails def (isCekEvaluationFailure def) $
                      continuingOutputTestTrace
                        Validators.OnlyHash
                        (Validators.continuingDatum Validators.ResolvedHash)
                ],
              testGroup
                "validator expects an inline datum..."
                [ testCase "...and gets a regular datum, expecting script failure" $
                    testFails def (isCekEvaluationFailure def) $
                      continuingOutputTestTrace
                        Validators.ResolvedHash
                        (Validators.continuingDatum Validators.Inline),
                  testCase "...and gets an inline datum, expecting success" $
                    testSucceeds def $
                      continuingOutputTestTrace
                        Validators.Inline
                        (Validators.continuingDatum Validators.Inline),
                  testCase "...and gets a datum hash, expecting script failure" $
                    testFails def (isCekEvaluationFailure def) $
                      continuingOutputTestTrace
                        Validators.OnlyHash
                        (Validators.continuingDatum Validators.Inline)
                ],
              testGroup
                "validator expects a datum hash..."
                [ testCase "...and gets a regular datum, expecting script failure" $
                    testFails def (isCekEvaluationFailure def) $
                      continuingOutputTestTrace
                        Validators.ResolvedHash
                        (Validators.continuingDatum Validators.OnlyHash),
                  testCase "...and gets an inline datum, expecting script failure" $
                    testFails def (isCekEvaluationFailure def) $
                      continuingOutputTestTrace
                        Validators.Inline
                        (Validators.continuingDatum Validators.OnlyHash),
                  testCase "...and gets a datum hash, expecting success" $
                    testSucceeds def $
                      continuingOutputTestTrace
                        Validators.OnlyHash
                        (Validators.continuingDatum Validators.OnlyHash)
                ]
            ]
        ]
    ]
