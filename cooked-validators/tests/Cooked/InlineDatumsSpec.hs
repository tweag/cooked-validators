{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.InlineDatumsSpec where

import Control.Monad
import Cooked
import Cooked.Tx.Constraints.Type
import Data.Default
import qualified Data.Map as Map
import qualified Ledger.Ada as Pl
import qualified Plutus.Script.Utils.V2.Scripts as Pl
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import qualified Plutus.V2.Ledger.Contexts as Pl
import qualified PlutusTx as Pl
import qualified PlutusTx.Trace as Pl
import Test.Tasty
import Test.Tasty.HUnit

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
              _ -> Pl.trace "I want an inline datum" False
            else case inDatum of
              Pl.OutputDatumHash _ -> True
              _ -> Pl.trace "I want a datum hash" False

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
              _ -> Pl.trace "I want an inline datum" False
            else case outDatum of
              Pl.OutputDatumHash _ -> True
              _ -> Pl.trace "I want a datum hash" False

    wrap = Pl.mkUntypedValidator @() @()

listUtxosTestTrace ::
  ( MonadBlockChain m,
    PaysScriptConstrs a,
    Pl.FromData (Pl.DatumType a),
    Default (Pl.DatumType a)
  ) =>
  Pl.TypedValidator a ->
  Bool ->
  m [(SpendableOut, Either Pl.DatumHash (Pl.DatumType a))]
listUtxosTestTrace validator useInlineDatum = do
  _ <-
    validateTxSkel
      mempty
        { txSkelOpts = def {adjustUnbalTx = True},
          txSkelOuts =
            [ paysScript
                validator
                ( let datum = def
                   in if useInlineDatum
                        then Right datum
                        else Left . Pl.datumHash . Pl.Datum . Pl.toBuiltinData $ datum
                )
                (Pl.lovelaceValueOf 3_000_000)
            ]
        }
  scriptUtxosSuchThat validator (\_ _ -> True)

spendOutputTestTrace ::
  ( MonadBlockChain m,
    PaysScriptConstrs a,
    SpendsScriptConstrs a,
    Pl.FromData (Pl.DatumType a),
    Default (Pl.DatumType a),
    Default (Pl.RedeemerType a)
  ) =>
  Pl.TypedValidator a ->
  Bool ->
  m ()
spendOutputTestTrace validator useInlineDatum = do
  (theUtxo, _) : _ <- listUtxosTestTrace validator useInlineDatum
  void $
    validateTxSkel
      mempty
        { txSkelOpts = def {adjustUnbalTx = True},
          txSkelIns =
            Map.singleton
              theUtxo
              (SpendsScript validator def)
        }

continuingOutputTestTrace ::
  ( MonadBlockChain m,
    PaysScriptConstrs a,
    SpendsScriptConstrs a,
    Pl.FromData (Pl.DatumType a),
    Default (Pl.DatumType a),
    Default (Pl.RedeemerType a)
  ) =>
  Pl.TypedValidator a ->
  Bool ->
  m ()
continuingOutputTestTrace validator useInlineDatumOnSecondPayment = do
  (theUtxo, _) : _ <- listUtxosTestTrace validator True
  void $
    validateTxSkel
      mempty
        { txSkelOpts = def {adjustUnbalTx = True},
          txSkelIns =
            Map.singleton
              theUtxo
              (SpendsScript validator def),
          txSkelOuts =
            [ paysScript
                validator
                ( let datum = def
                   in if useInlineDatumOnSecondPayment
                        then Right datum
                        else Left . Pl.datumHash . Pl.Datum . Pl.toBuiltinData $ datum
                )
                (sOutValue theUtxo)
            ]
        }

tests :: TestTree
tests =
  testGroup
    "inline datums vs. datum hashes"
    [ testGroup
        "from the point of view of (script)UtxosSuchThat"
        [ -- The validator used in these test cases does not actually matter, we
          -- just need some script to pay to.
          testCase "the datum is retrieved correctly" $
            assertBool "... it's not" $ case runMockChain (listUtxosTestTrace (inputDatumValidator True) True) of
              Right ([(_, Right ())], _) -> True
              _ -> False,
          testCase "the datum hash is retrieved correctly" $
            assertBool "... it's not" $ case runMockChain (listUtxosTestTrace (inputDatumValidator True) False) of
              Right ([(_, Left dh)], _) -> dh == (Pl.datumHash . Pl.Datum . Pl.toBuiltinData $ ())
              _ -> False
        ],
      testGroup
        "from the point of view of scripts"
        [ testGroup
            "looking at transaction inputs"
            [],
          testGroup
            "looking at transaction outputs"
            [ testGroup
                "validator expects an inline datum..."
                [ testCase "... and gets an inline datum, expecting success" $
                    testSucceeds $
                      continuingOutputTestTrace (outputDatumValidator True) True,
                  testCase "... and gets a datum hash, expecting script failure" $
                    testFailsFrom' isCekEvaluationFailure def $
                      continuingOutputTestTrace (outputDatumValidator True) False
                ],
              testGroup
                "validator expects a datum hash..."
                [ testCase "... and gets an inline datum, expecting script failure" $
                    testFailsFrom' isCekEvaluationFailure def $
                      continuingOutputTestTrace (outputDatumValidator False) True,
                  testCase "... and gets a datum hash, expecting success" $
                    testSucceeds $
                      continuingOutputTestTrace (outputDatumValidator False) False
                ]
            ]
        ]
    ]
