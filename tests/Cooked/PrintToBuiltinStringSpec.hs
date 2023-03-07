{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.PrintToBuiltinStringSpec (tests) where

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
    print _ _ ctx = Pl.trace (printBS . Pl.txInfoInputs . Pl.scriptContextTxInfo Pl.$ ctx) False

printTrace :: MonadBlockChain m => m ()
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
        { txSkelSigners = [wallet 1],
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
            [ (printBS @Pl.Integer 123, "123"),
              (printBS @Pl.Integer (-123), "-123"),
              (printBS @[Pl.Integer] [1, 2, 3], "[1,2,3]"),
              (printBS (Pl.True, Pl.False), "(True,False)"),
              (printBS @Pl.BuiltinByteString "abca", "\"61626361\""),
              (printBS @Pl.Value mempty, "Value (fromList [])"),
              ( printBS @Pl.Value (Ada.lovelaceValueOf 123),
                "Value (fromList [(CurrencySymbol \"\",fromList [(TokenName \"\",123)])])"
              ),
              ( printBS @Pl.Value (quickValue "banana" 4),
                "Value (fromList [(CurrencySymbol \"35a527970f2e1b64ed7cf429f1594ce8b5f0cf09e37473ab628082bd\",fromList [(TokenName \"62616e616e61\",4)])])"
              ),
              ( printBS (Pl.mkConstr 0 [Pl.mkMap [(Pl.mkI 1, Pl.mkList [Pl.mkB "abc"])]]),
                "BuiltinData (Constr 0 [Map [(I 1,List [B \"616263\"])]])"
              ),
              ( printBS
                  ( Pl.Interval
                      (Pl.LowerBound (Pl.Finite $ Pl.POSIXTime 123) True)
                      (Pl.UpperBound (Pl.Finite $ Pl.POSIXTime 234) False)
                  ),
                "Interval (LowerBound (Finite (POSIXTime 123)) True) (UpperBound (Finite (POSIXTime 234)) False)"
              )
            ],
      testCase "printing 'txInfoInputs' from a validator produces the expected string" $
        testFailsFrom'
          def
          ( isCekEvaluationFailureWithMsg
              def
              ( isRight
                  . parse
                    ( string "[TxInInfo (TxOutRef (TxId \""
                        *> many1 hexDigit
                        *> string "\") 0) (TxOut (Address (ScriptCredential (ValidatorHash \""
                        *> many1 hexDigit
                        *> string "\")) Nothing) (Value (fromList [(CurrencySymbol \"\",fromList [(TokenName \"\",30000000)])])) (OutputDatum (Datum (BuiltinData (Constr 0 [])))) Nothing)]"
                    )
                    ""
              )
          )
          def
          printTrace
    ]
