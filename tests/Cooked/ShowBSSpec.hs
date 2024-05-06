module Cooked.ShowBSSpec (tests) where

import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Monad
import Cooked
import Data.Default
import Data.Either
import Data.Map qualified as Map
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Typed qualified as Script
import Plutus.Script.Utils.V3.Typed.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import PlutusTx.Prelude qualified as PlutusTx
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec

data UnitContract

instance Script.ValidatorTypes UnitContract where
  type RedeemerType UnitContract = Bool
  type DatumType UnitContract = ()

{-# INLINEABLE traceValidator #-}
traceValidator :: () -> Bool -> Api.ScriptContext -> Bool
traceValidator _ _ ctx = PlutusTx.trace (showBS ctx) False

printValidator :: Script.TypedValidator UnitContract
printValidator =
  Script.mkTypedValidator @UnitContract
    $$(PlutusTx.compile [||traceValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Script.mkUntypedValidator

printTrace :: (MonadBlockChain m) => m ()
printTrace = do
  (oref, _) : _ <-
    utxosFromCardanoTx
      <$> validateTxSkel
        txSkelTemplate
          { txSkelSigners = [wallet 1],
            txSkelOuts =
              [ paysScriptInlineDatum
                  printValidator
                  ()
                  (Script.lovelaceValueOf 30_000_000)
              ]
          }
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {txOptEmulatorParamsModification = Just $ EmulatorParamsModification Emulator.increaseTransactionLimits},
          txSkelSigners = [wallet 1],
          txSkelIns = Map.singleton oref $ TxSkelRedeemerForScript True
        }

tests :: TestTree
tests =
  testGroup
    "Printing transaction data as bytestrings"
    [ testCase "a few simple examples" $
        testConjoin $
          map
            (uncurry (@?=))
            [ (showBS @PlutusTx.Integer 123, "123"),
              (showBS @PlutusTx.Integer (-123), "-123"),
              (showBS @[PlutusTx.Integer] [1, 2, 3], "[1,2,3]"),
              (showBS (PlutusTx.True, PlutusTx.False), "(True,False)"),
              (showBS @PlutusTx.BuiltinByteString "abca", "\"61626361\""),
              (showBS @Api.Value mempty, "(Value (fromList []))"),
              ( showBS @Api.Value (Script.lovelaceValueOf 123),
                "(Value (fromList [((CurrencySymbol \"\"),(fromList [((TokenName \"\"),123)]))]))"
              ),
              ( showBS @Api.Value (quickValue "banana" 4),
                "(Value (fromList [((CurrencySymbol \"5cf8bdd7aa3d027821e6033816847034b0418a8959c40e54b6977f95\"),(fromList [((TokenName \"62616e616e61\"),4)]))]))"
              ),
              ( showBS (PlutusTx.mkConstr 0 [PlutusTx.mkMap [(PlutusTx.mkI 1, PlutusTx.mkList [PlutusTx.mkB "abc"])]]),
                "(BuiltinData (Constr 0 [(Map [((I 1),(List [(B \"616263\")]))])]))"
              ),
              ( showBS
                  ( Api.Interval
                      (Api.LowerBound (Api.Finite $ Api.POSIXTime 123) True)
                      (Api.UpperBound (Api.Finite $ Api.POSIXTime 234) False)
                  ),
                "(Interval (LowerBound (Finite (POSIXTime 123)) True) (UpperBound (Finite (POSIXTime 234)) False))"
              )
            ],
      testCase "printing the 'TxInfo' from a validator produces the expected string" $
        let isExpectedString = (==) "(Script context:Script Tx info:(inputs:[(TxInInfo (TxOutRef (TxId \"52fc6c6528d8f75562f11488235d30f6e1e2e0d5f42c44205a41c1142f6a53c4\") 0) (TxOut (Address (ScriptCredential (ScriptHash \"547fb2f07774227057df1533d509ddb42af7c8eb79c5658b3d37b4bf\")) Nothing) (Value (fromList [((CurrencySymbol \"\"),(fromList [((TokenName \"\"),30000000)]))])) (OutputDatum (Datum (BuiltinData (Constr 0 [])))) Nothing))]reference inputs:[]outputs:[(TxOut (Address (PubKeyCredential (PubKeyHash \"a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2\")) Nothing) (Value (fromList [((CurrencySymbol \"\"),(fromList [((TokenName \"\"),29522935)]))])) NoOutputDatum Nothing)]fees:(Lovelace 477065)minted value:(Value (fromList []))certificates:[]wdrl:(fromList [])valid range:(Interval (LowerBound NegInf True) (UpperBound PosInf True))signatories:[(PubKeyHash \"a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2\")]redeemers:(fromList [((Spending (TxOutRef (TxId \"52fc6c6528d8f75562f11488235d30f6e1e2e0d5f42c44205a41c1142f6a53c4\") 0)),(Redeemer (BuiltinData (Constr 1 []))))])datums:(fromList [])transaction id:(TxId \"0f9be025d3a49e0ecb55026218e0f3f29163cf21e5fdb243a4a6226ea36bd1e5\")votes:(fromList [])proposals:[]treasury amount:Nothingtreasury donation:Nothing)Script purpose:(Spending (TxOutRef (TxId \"52fc6c6528d8f75562f11488235d30f6e1e2e0d5f42c44205a41c1142f6a53c4\") 0)))"
         in testFails
              (def @PrettyCookedOpts)
              ( isCekEvaluationFailureWithMsg
                  (def @PrettyCookedOpts)
                  isExpectedString
              )
              printTrace
    ]
