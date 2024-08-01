module Cooked.WithdrawalsSpec where

import Control.Monad
import Cooked
import Data.Default
import Data.Map qualified as Map
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx qualified
import PlutusTx.AssocMap qualified as PMap
import PlutusTx.Prelude qualified as PlutusTx
import Test.Tasty
import Test.Tasty.HUnit

checkWithdrawalScript :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
checkWithdrawalScript red ctx =
  let scriptContext = PlutusTx.unsafeFromBuiltinData @Api.ScriptContext ctx
      withdrawals = Api.txInfoWdrl PlutusTx.$ Api.scriptContextTxInfo scriptContext
      quantity = PlutusTx.unsafeFromBuiltinData @Integer red
      purpose = Api.scriptContextPurpose scriptContext
   in case purpose of
        Api.Rewarding cred -> case PMap.toList withdrawals of
          [(cred', Api.Lovelace n)] ->
            if cred PlutusTx.== cred'
              then
                if n PlutusTx.== quantity
                  then ()
                  else PlutusTx.traceError "Wrong quantity."
              else PlutusTx.traceError "Wrong credential."
          _ -> PlutusTx.traceError "Wrong withdrawal."
        _ -> PlutusTx.traceError "Wrong script purpose."

checkWithdrawalVersionedScript :: Script.Versioned Script.Script
checkWithdrawalVersionedScript = mkScript $$(PlutusTx.compile [||checkWithdrawalScript||])

testWithdrawingScript :: (MonadBlockChain m) => Integer -> Integer -> m ()
testWithdrawingScript n1 n2 =
  void $
    validateTxSkel $
      txSkelTemplate
        { txSkelSigners = [wallet 1],
          txSkelWithdrawals =
            Map.singleton
              (Left (checkWithdrawalVersionedScript, txSkelSomeRedeemer (n1 * 1_000 :: Integer)))
              (Script.Lovelace $ n2 * 1_000)
        }

tests :: TestTree
tests =
  testGroup
    "Withdrawing scripts"
    [ testCase "We can use a withdrawing script" $
        testSucceeds def $
          testWithdrawingScript 2 2,
      testCase "But the script might fail" $
        testFailsFrom def (isCekEvaluationFailure def) def $
          testWithdrawingScript 2 1
    ]
