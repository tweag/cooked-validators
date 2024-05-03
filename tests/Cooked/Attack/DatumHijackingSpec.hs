module Cooked.Attack.DatumHijackingSpec (tests) where

import Control.Monad
import Cooked
import Data.Default
import Data.Map qualified as Map
import Data.Set qualified as Set
import Optics.Core
import Plutus.Script.Utils.Ada qualified as Pl
import Plutus.Script.Utils.Typed qualified as Pl
import Plutus.Script.Utils.V3.Contexts qualified as Pl
import Plutus.Script.Utils.V3.Typed.Scripts qualified as Pl
import Plutus.Script.Utils.Value qualified as Pl
import PlutusLedgerApi.V3 qualified as Pl
import PlutusTx qualified (compile, makeLift, unstableMakeIsData)
import PlutusTx.Prelude qualified as Pl
import Prettyprinter
import Test.Tasty
import Test.Tasty.HUnit

-- * Mock contract for the datum hijacking attack

-- This is a very simple contract: The first transaction locks some Ada to the
-- validator, using the datum 'FirstLock', the second transaction then re-locks
-- the same amount to the same validator, using the datum 'SecondLock'. The
-- datum hijacking attack should target the second transaction, and substitute a
-- different recipient.

data LockDatum = FirstLock | SecondLock deriving (Show, Eq)

instance PrettyCooked LockDatum where
  prettyCooked = viaShow

instance Pl.Eq LockDatum where
  {-# INLINEABLE (==) #-}
  FirstLock == FirstLock = True
  SecondLock == SecondLock = True
  _ == _ = False

PlutusTx.makeLift ''LockDatum
PlutusTx.unstableMakeIsData ''LockDatum

data DHContract

instance Pl.ValidatorTypes DHContract where
  type DatumType DHContract = LockDatum
  type RedeemerType DHContract = ()

-- ** Transactions (and 'TxSkels') for the datum hijacking attack

lockValue :: Pl.Value
lockValue = Pl.lovelaceValueOf 12345678

lockTxSkel :: Pl.TxOutRef -> Pl.TypedValidator DHContract -> TxSkel
lockTxSkel o v =
  txSkelTemplate
    { txSkelOpts = def {txOptEnsureMinAda = True},
      txSkelIns = Map.singleton o TxSkelNoRedeemerForPK,
      txSkelOuts = [paysScriptInlineDatum v FirstLock lockValue],
      txSkelSigners = [wallet 1]
    }

txLock :: (MonadBlockChain m) => Pl.TypedValidator DHContract -> m ()
txLock v = do
  (oref, _) : _ <-
    runUtxoSearch $
      utxosAtSearch (walletAddress $ wallet 1)
        `filterWithPred` ((`Pl.geq` lockValue) . outputValue)
  void $ validateTxSkel $ lockTxSkel oref v

relockTxSkel :: Pl.TypedValidator DHContract -> Pl.TxOutRef -> TxSkel
relockTxSkel v o =
  txSkelTemplate
    { txSkelOpts = def {txOptEnsureMinAda = True},
      txSkelIns = Map.singleton o $ TxSkelRedeemerForScript (),
      txSkelOuts = [paysScriptInlineDatum v SecondLock lockValue],
      txSkelSigners = [wallet 1]
    }

txRelock ::
  (MonadBlockChain m) =>
  Pl.TypedValidator DHContract ->
  m ()
txRelock v = do
  (oref, _) : _ <-
    runUtxoSearch $
      utxosAtSearch (Pl.validatorAddress v)
        `filterWith` resolveDatum
        `filterWithPure` isOutputWithInlineDatumOfType @LockDatum
        `filterWithPred` ((FirstLock ==) . (^. outputDatumL))
  void $ validateTxSkel $ relockTxSkel v oref

datumHijackingTrace :: (MonadBlockChain m) => Pl.TypedValidator DHContract -> m ()
datumHijackingTrace v = do
  txLock v
  txRelock v

-- * Validators for the datum hijacking attack

-- | Try to extract a datum from an output.
{-# INLINEABLE outputDatum #-}
outputDatum :: Pl.TxInfo -> Pl.TxOut -> Maybe LockDatum
outputDatum txi o = case Pl.txOutDatum o of
  Pl.NoOutputDatum -> Nothing
  Pl.OutputDatumHash h -> do
    Pl.Datum d <- Pl.findDatum h txi
    Pl.fromBuiltinData d
  Pl.OutputDatum (Pl.Datum d) -> Pl.fromBuiltinData d

{-# INLINEABLE mkMockValidator #-}
mkMockValidator :: (Pl.ScriptContext -> [Pl.TxOut]) -> LockDatum -> () -> Pl.ScriptContext -> Bool
mkMockValidator getOutputs datum _ ctx =
  let txi = Pl.scriptContextTxInfo ctx
   in case datum of
        FirstLock ->
          case getOutputs ctx of
            o : _ ->
              Pl.traceIfFalse
                "not in 'SecondLock'-state after re-locking"
                (outputDatum txi o Pl.== Just SecondLock)
                && Pl.traceIfFalse
                  "not re-locking the right amout"
                  (Pl.txOutValue o == lockValue)
            _ -> Pl.trace "there must be a output re-locked" False
        SecondLock -> False

{-# INLINEABLE mkCarefulValidator #-}
mkCarefulValidator :: LockDatum -> () -> Pl.ScriptContext -> Bool
mkCarefulValidator = mkMockValidator Pl.getContinuingOutputs

carefulValidator :: Pl.TypedValidator DHContract
carefulValidator =
  Pl.mkTypedValidator @DHContract
    $$(PlutusTx.compile [||mkCarefulValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Pl.mkUntypedValidator

{-# INLINEABLE mkCarelessValidator #-}
mkCarelessValidator :: LockDatum -> () -> Pl.ScriptContext -> Bool
mkCarelessValidator = mkMockValidator (Pl.txInfoOutputs . Pl.scriptContextTxInfo)

carelessValidator :: Pl.TypedValidator DHContract
carelessValidator =
  Pl.mkTypedValidator @DHContract
    $$(PlutusTx.compile [||mkCarelessValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Pl.mkUntypedValidator

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
            x1 = Pl.lovelaceValueOf 10001
            x2 = Pl.lovelaceValueOf 10000
            x3 = Pl.lovelaceValueOf 9999
            skelIn =
              txSkelFromOuts
                [ paysScriptInlineDatum val1 SecondLock x1,
                  paysScriptInlineDatum val1 SecondLock x3,
                  paysScriptInlineDatum val2 SecondLock x1,
                  paysScriptInlineDatum val1 FirstLock x2,
                  paysScriptInlineDatum val1 SecondLock x2
                ]
            skelOut bound select =
              runTweak
                ( datumHijackingAttack @DHContract
                    ( \(ConcreteOutput v _ x d _) ->
                        Pl.validatorHash val1
                          == Pl.validatorHash v
                          && d
                            == TxSkelOutInlineDatum SecondLock
                          && bound
                            `Pl.geq` x
                    )
                    select
                )
                skelIn
            skelExpected a b =
              txSkelTemplate
                { txSkelLabel =
                    Set.singleton . TxLabel . DatumHijackingLbl $
                      Pl.validatorAddress thief,
                  txSkelOuts =
                    [ paysScriptInlineDatum val1 SecondLock x1,
                      paysScriptInlineDatum a SecondLock x3,
                      paysScriptInlineDatum val2 SecondLock x1,
                      paysScriptInlineDatum val1 FirstLock x2,
                      paysScriptInlineDatum b SecondLock x2
                    ],
                  txSkelSigners = [wallet 1]
                }
         in [ testCase "no modified transactions if no interesting outputs to steal" $ [] @=? skelOut mempty (const True),
              testCase "one modified transaction for one interesting output" $
                [ Right
                    ( [ConcreteOutput val1 Nothing x3 (TxSkelOutInlineDatum SecondLock) Nothing],
                      skelExpected thief val1
                    )
                ]
                  @=? skelOut x2 (0 ==),
              testCase "two modified transactions for two interesting outputs" $
                [ Right
                    ( [ ConcreteOutput val1 Nothing x3 (TxSkelOutInlineDatum SecondLock) Nothing,
                        ConcreteOutput val1 Nothing x2 (TxSkelOutInlineDatum SecondLock) Nothing
                      ],
                      skelExpected thief thief
                    )
                ]
                  @=? skelOut x2 (const True),
              testCase "select second interesting output to get one modified transaction" $
                [ Right
                    ( [ConcreteOutput val1 Nothing x2 (TxSkelOutInlineDatum SecondLock) Nothing],
                      skelExpected val1 thief
                    )
                ]
                  @=? skelOut x2 (1 ==)
            ],
      testCase "careful validator" $
        testFails
          def
          (isCekEvaluationFailure def)
          ( somewhere
              ( datumHijackingAttack @DHContract
                  ( \(ConcreteOutput v _ _ d _) ->
                      Pl.validatorHash v == Pl.validatorHash carefulValidator
                        && d == TxSkelOutInlineDatum SecondLock
                  )
                  (const True)
              )
              (datumHijackingTrace carefulValidator)
          ),
      testCase "careless validator" $
        testSucceeds
          def
          ( somewhere
              ( datumHijackingAttack @DHContract
                  ( \(ConcreteOutput v _ _ d _) ->
                      Pl.validatorHash v == Pl.validatorHash carelessValidator
                        && d == TxSkelOutInlineDatum SecondLock
                  )
                  (const True)
              )
              (datumHijackingTrace carelessValidator)
          )
    ]
