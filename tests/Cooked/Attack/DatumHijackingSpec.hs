module Cooked.Attack.DatumHijackingSpec (tests) where

import Control.Monad
import Cooked
import Data.Default
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Typeable
import Optics.Core
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Typed qualified as Script
import Plutus.Script.Utils.V3.Typed.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script
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

instance Script.ValidatorTypes DHContract where
  type DatumType DHContract = LockDatum
  type RedeemerType DHContract = ()

-- ** Transactions (and 'TxSkels') for the datum hijacking attack

lockValue :: Api.Value
lockValue = Script.lovelaceValueOf 12345678

mkPayment :: (Show a, PrettyCooked a, PlutusTx.ToData a, PlutusTx.Eq a, Typeable a, ToValue v) => a -> v -> Payment
mkPayment dat val = paymentTemplate {paymentDatum = Just dat, paymentDatumKind = InlineDatum, paymentValue = val}

lockTxSkel :: Api.TxOutRef -> Script.TypedValidator DHContract -> TxSkel
lockTxSkel o v =
  txSkelTemplate
    { txSkelOpts = def {txOptEnsureMinAda = True},
      txSkelIns = Map.singleton o emptyTxSkelRedeemer,
      txSkelOuts = [v `receives` mkPayment FirstLock lockValue],
      txSkelSigners = [wallet 1]
    }

txLock :: (MonadBlockChain m) => Script.TypedValidator DHContract -> m ()
txLock v = do
  (oref, _) : _ <- runUtxoSearch $ utxosAtSearch (wallet 1) `filterWithPred` ((`Script.geq` lockValue) . outputValue)
  void $ validateTxSkel $ lockTxSkel oref v

relockTxSkel :: Script.TypedValidator DHContract -> Api.TxOutRef -> TxSkel
relockTxSkel v o =
  txSkelTemplate
    { txSkelOpts = def {txOptEnsureMinAda = True},
      txSkelIns = Map.singleton o $ someTxSkelRedeemer (),
      txSkelOuts = [v `receives` mkPayment SecondLock lockValue],
      txSkelSigners = [wallet 1]
    }

txRelock ::
  (MonadBlockChain m) =>
  Script.TypedValidator DHContract ->
  m ()
txRelock v = do
  (oref, _) : _ <-
    runUtxoSearch $
      utxosAtSearch v
        `filterWith` resolveDatum
        `filterWithPure` isOutputWithInlineDatumOfType @LockDatum
        `filterWithPred` ((FirstLock ==) . (^. outputDatumL))
  void $ validateTxSkel $ relockTxSkel v oref

datumHijackingTrace :: (MonadBlockChain m) => Script.TypedValidator DHContract -> m ()
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

{-# INLINEABLE mkMockValidator #-}
mkMockValidator :: (Api.ScriptContext -> [Api.TxOut]) -> LockDatum -> () -> Api.ScriptContext -> Bool
mkMockValidator getOutputs datum _ ctx =
  let txi = Api.scriptContextTxInfo ctx
   in case datum of
        FirstLock ->
          case getOutputs ctx of
            o : _ ->
              PlutusTx.traceIfFalse
                "not in 'SecondLock'-state after re-locking"
                (outputDatum txi o PlutusTx.== Just SecondLock)
                && PlutusTx.traceIfFalse
                  "not re-locking the right amout"
                  (Api.txOutValue o == lockValue)
            _ -> PlutusTx.trace "there must be a output re-locked" False
        SecondLock -> False

{-# INLINEABLE mkCarefulValidator #-}
mkCarefulValidator :: LockDatum -> () -> Api.ScriptContext -> Bool
mkCarefulValidator = mkMockValidator Api.getContinuingOutputs

carefulValidator :: Script.TypedValidator DHContract
carefulValidator =
  Script.mkTypedValidator @DHContract
    $$(PlutusTx.compile [||mkCarefulValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Script.mkUntypedValidator

{-# INLINEABLE mkCarelessValidator #-}
mkCarelessValidator :: LockDatum -> () -> Api.ScriptContext -> Bool
mkCarelessValidator = mkMockValidator (Api.txInfoOutputs . Api.scriptContextTxInfo)

carelessValidator :: Script.TypedValidator DHContract
carelessValidator =
  Script.mkTypedValidator @DHContract
    $$(PlutusTx.compile [||mkCarelessValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Script.mkUntypedValidator

txSkelFromOuts :: [TxSkelOut] -> TxSkel
txSkelFromOuts os = txSkelTemplate {txSkelOuts = os, txSkelSigners = [wallet 1]}

-- * TestTree for the datum hijacking attack

tests :: TestTree
tests =
  testGroup
    "datum hijacking attack"
    [ testGroup "unit tests on a 'TxSkel'" $
        let val1 = carelessValidator
            val2 = carefulValidator
            thief = alwaysTrueValidator @DHContract
            x1 = Script.lovelaceValueOf 10001
            x2 = Script.lovelaceValueOf 10000
            x3 = Script.lovelaceValueOf 9999
            skelIn =
              txSkelFromOuts
                [ val1 `receives` mkPayment SecondLock x1,
                  val1 `receives` mkPayment SecondLock x3,
                  val2 `receives` mkPayment SecondLock x1,
                  val1 `receives` mkPayment FirstLock x2,
                  val1 `receives` mkPayment SecondLock x2
                ]
            skelOut bound select =
              runTweak
                ( datumHijackingAttack @DHContract
                    ( \(ConcreteOutput v _ d x _) ->
                        Script.validatorHash val1
                          == Script.validatorHash v
                          && d
                            == TxSkelOutInlineDatum SecondLock
                          && bound
                            `Script.geq` x
                    )
                    select
                )
                skelIn
            skelExpected a b =
              txSkelTemplate
                { txSkelLabel =
                    Set.singleton . TxLabel . DatumHijackingLbl $
                      Script.validatorAddress thief,
                  txSkelOuts =
                    [ val1 `receives` mkPayment SecondLock x1,
                      a `receives` mkPayment SecondLock x3,
                      val2 `receives` mkPayment SecondLock x1,
                      val1 `receives` mkPayment FirstLock x2,
                      b `receives` mkPayment SecondLock x2
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
            ( datumHijackingAttack @DHContract
                ( \(ConcreteOutput v _ d _ _) ->
                    Script.validatorHash v == Script.validatorHash carefulValidator
                      && d == TxSkelOutInlineDatum SecondLock
                )
                (const True)
            )
            (datumHijackingTrace carefulValidator),
      testCase "careless validator" $
        testSucceeds $
          somewhere
            ( datumHijackingAttack @DHContract
                ( \(ConcreteOutput v _ d _ _) ->
                    Script.validatorHash v == Script.validatorHash carelessValidator
                      && d == TxSkelOutInlineDatum SecondLock
                )
                (const True)
            )
            (datumHijackingTrace carelessValidator)
    ]
