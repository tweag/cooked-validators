{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.ShowBSSpec (tests) where

import qualified Cardano.Node.Emulator as Emulator
import Control.Monad
import Cooked
import Data.Default
import Data.Either
import qualified Data.Map as Map
import qualified Plutus.Script.Utils.Ada as Ada
import qualified Plutus.Script.Utils.Typed as Pl
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import qualified Plutus.V2.Ledger.Contexts as Pl
import qualified PlutusTx as Pl
import qualified PlutusTx.Builtins as Pl
import qualified PlutusTx.Prelude as Pl
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec

data UnitContract

instance Pl.ValidatorTypes UnitContract where
  type RedeemerType UnitContract = Bool
  type DatumType UnitContract = ()

printValidator :: Pl.TypedValidator UnitContract
printValidator =
  Pl.mkTypedValidator @UnitContract
    $$(Pl.compile [||print||])
    $$(Pl.compile [||wrap||])
  where
    wrap = Pl.mkUntypedValidator
    print _ _ ctx = Pl.trace (showBS . Pl.scriptContextTxInfo Pl.$ ctx) False

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
                  (Ada.lovelaceValueOf 30_000_000)
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
    "printing to BuiltinString"
    [ testCase "a few simple examples" $
        testConjoin $
          map
            (uncurry (@?=))
            [ (showBS @Pl.Integer 123, "123"),
              (showBS @Pl.Integer (-123), "-123"),
              (showBS @[Pl.Integer] [1, 2, 3], "[1,2,3]"),
              (showBS (Pl.True, Pl.False), "(True,False)"),
              (showBS @Pl.BuiltinByteString "abca", "\"61626361\""),
              (showBS @Pl.Value mempty, "Value (fromList [])"),
              ( showBS @Pl.Value (Ada.lovelaceValueOf 123),
                "Value (fromList [(CurrencySymbol \"\",fromList [(TokenName \"\",123)])])"
              ),
              ( showBS @Pl.Value (quickValue "banana" 4),
                "Value (fromList [(CurrencySymbol \"ce10165e35b6d1e09617cb0f8a2bf6e2fedc347ddd497c8d4b02741c\",fromList [(TokenName \"62616e616e61\",4)])])"
              ),
              ( showBS (Pl.mkConstr 0 [Pl.mkMap [(Pl.mkI 1, Pl.mkList [Pl.mkB "abc"])]]),
                "BuiltinData (Constr 0 [Map [(I 1,List [B \"616263\"])]])"
              ),
              ( showBS
                  ( Pl.Interval
                      (Pl.LowerBound (Pl.Finite $ Pl.POSIXTime 123) True)
                      (Pl.UpperBound (Pl.Finite $ Pl.POSIXTime 234) False)
                  ),
                "Interval (LowerBound (Finite (POSIXTime 123)) True) (UpperBound (Finite (POSIXTime 234)) False)"
              )
            ],
      testCase "printing the 'TxInfo' from a validator produces the expected string" $
        let isExpectedString =
              isRight
                . parse
                  ( string "TxInfo [TxInInfo (TxOutRef (TxId \""
                      *> many1 hexDigit
                      *> string "\") 0) (TxOut (Address (ScriptCredential (ValidatorHash \""
                      *> many1 hexDigit
                      *> string "\")) Nothing) (Value (fromList [(CurrencySymbol \"\",fromList [(TokenName \"\",30000000)])])) (OutputDatum (Datum (BuiltinData (Constr 0 [])))) Nothing)] [] [TxOut (Address (PubKeyCredential (PubKeyHash \""
                      *> many1 hexDigit
                      *> string "\")) Nothing) (Value (fromList [(CurrencySymbol \"\",fromList [(TokenName \"\",27000000)])])) NoOutputDatum Nothing] (Value (fromList [(CurrencySymbol \"\",fromList [(TokenName \"\",3000000)])])) (Value (fromList [(CurrencySymbol \"\",fromList [(TokenName \"\",0)])])) [] (fromList []) (Interval (LowerBound NegInf True) (UpperBound PosInf True)) [PubKeyHash \""
                      *> many1 hexDigit
                      *> string "\"] (fromList [(Spending (TxOutRef (TxId \""
                      *> many1 hexDigit
                      *> string "\") 0),Redeemer (BuiltinData (Constr 1 [])))]) (fromList []) (TxId \""
                      *> many1 hexDigit
                      *> string "\")"
                  )
                  ""
         in testFails
              (def @PrettyCookedOpts)
              ( isCekEvaluationFailureWithMsg
                  (def @PrettyCookedOpts)
                  isExpectedString
              )
              printTrace
    ]
