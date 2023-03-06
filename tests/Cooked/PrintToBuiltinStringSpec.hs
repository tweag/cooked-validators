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
              (printBS (Pl.True, Pl.False), "(True, False)"),
              (printBS @Pl.BuiltinByteString "abca", "\"61626361\""),
              ( printBS (Pl.mkConstr 0 [Pl.mkMap [(Pl.mkI 1, Pl.mkList [Pl.mkB "abc"])]]),
                "BuiltinData (Constr 0 [Map [(I 1, List [B \"616263\"])]])"
              )
            ],
      testCase "print the 'txInfoInputs' from a validator" $
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
                        *> string "\")) Nothing) (Value (fromList [(CurrencySymbol  \"\", fromList [(TokenName \"\", 30000000)])])) (OutputDatum (Datum (BuiltinData (Constr 0 [])))) Nothing)]"
                    )
                    ""
              )
          )
          def
          printTrace
    ]
