{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.InlineDatums where

import Cooked hiding (Inline)
import Data.Map qualified as Map
import Optics.Core
import Plutus.InlineDatums
import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Prettyprinter
import Test.Tasty
import Test.Tasty.HUnit

instance PrettyCooked SimpleContractDatum where
  prettyCooked = viaShow

-- | This defines two single-transaction traces: @listUtxosTestTrace True@ will
-- pay a script with an inline datum, while @listUtxosTestTrace False@ will use
-- a datum hash.
listUtxosTestTrace ::
  (MonadBlockChain m) =>
  Bool ->
  Script.Versioned Script.Validator ->
  m (Api.TxOutRef, TxSkelOut)
listUtxosTestTrace useInlineDatum validator =
  (\oref -> (oref,) <$> txSkelOutByRef oref) . head
    =<< validateTxSkel'
      txSkelTemplate
        { txSkelOuts = [validator `receives` (if useInlineDatum then InlineDatum else VisibleHashedDatum) FirstPaymentDatum],
          txSkelSigners = txSkelSignatoriesFromList [wallet 1]
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
  (theTxOutRef, _) <- listUtxosTestTrace useInlineDatum validator
  validateTxSkel_
    txSkelTemplate
      { txSkelIns = Map.singleton theTxOutRef $ someTxSkelRedeemer (),
        txSkelSigners = txSkelSignatoriesFromList [wallet 1]
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
  (theTxOutRef, theOutput) <- listUtxosTestTrace True validator
  validateTxSkel_
    txSkelTemplate
      { txSkelIns = Map.singleton theTxOutRef $ someTxSkelRedeemer (),
        txSkelOuts =
          [ validator
              `receives` Value (txSkelOutValue theOutput)
              <&&> ( case datumKindOnSecondPayment of
                       OnlyHash -> HiddenHashedDatum
                       Datum -> VisibleHashedDatum
                       Inline -> InlineDatum
                   )
                SecondPaymentDatum
          ],
        txSkelSigners = txSkelSignatoriesFromList [wallet 1]
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
            mustSucceedTest (listUtxosTestTrace True requireInlineDatumInInputValidator)
              `withResultProp` \(_, output) -> case Script.toOutputDatum (output ^. txSkelOutDatumL) of
                Api.OutputDatum _ -> testSuccess
                _ -> testFailure,
          testCooked "the datum hash is retrieved correctly" $
            mustSucceedTest (listUtxosTestTrace False requireInlineDatumInInputValidator)
              `withResultProp` \(_, output) -> case Script.toOutputDatum (output ^. txSkelOutDatumL) of
                Api.OutputDatumHash _ -> testSuccess
                _ -> testFailure
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
                      continuingOutputTestTrace Datum requireHashedDatumInOutputValidator,
                  testCooked "...and gets an inline datum, expecting script failure" $
                    mustFailInPhase2Test $
                      continuingOutputTestTrace Inline requireHashedDatumInOutputValidator,
                  testCooked "...and gets a datum hash, expecting script failure" $
                    mustFailInPhase2Test $
                      continuingOutputTestTrace OnlyHash requireHashedDatumInOutputValidator
                ],
              testGroup
                "validator expects an inline datum..."
                [ testCooked "...and gets a regular datum, expecting script failure" $
                    mustFailInPhase2Test $
                      continuingOutputTestTrace Datum requireInlineDatumInOutputValidator,
                  testCooked "...and gets an inline datum, expecting success" $
                    mustSucceedTest $
                      continuingOutputTestTrace Inline requireInlineDatumInOutputValidator,
                  testCooked "...and gets a datum hash, expecting script failure" $
                    mustFailInPhase2Test $
                      continuingOutputTestTrace OnlyHash requireInlineDatumInOutputValidator
                ],
              testGroup
                "validator expects a datum hash..."
                [ testCooked "...and gets a regular datum, expecting script failure" $
                    mustFailInPhase2Test $
                      continuingOutputTestTrace Datum requireOnlyHashedDatumInOutputValidator,
                  testCooked "...and gets an inline datum, expecting script failure" $
                    mustFailInPhase2Test $
                      continuingOutputTestTrace Inline requireOnlyHashedDatumInOutputValidator,
                  testCooked "...and gets a datum hash, expecting success" $
                    mustSucceedTest $
                      continuingOutputTestTrace OnlyHash requireOnlyHashedDatumInOutputValidator
                ]
            ]
        ]
    ]
