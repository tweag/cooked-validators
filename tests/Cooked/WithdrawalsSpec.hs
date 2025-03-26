module Cooked.WithdrawalsSpec where

import Control.Monad
import Cooked
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.V3.Typed.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx qualified
import PlutusTx.AssocMap qualified as PMap
import PlutusTx.Prelude qualified as PlutusTx
import Test.Tasty
import Test.Tasty.HUnit

{-# INLINEABLE checkWithdrawalPurpose #-}
checkWithdrawalPurpose :: Script.RewardingScriptType Integer Api.TxInfo
checkWithdrawalPurpose cred quantity (Api.TxInfo {txInfoWdrl}) =
  case PMap.toList txInfoWdrl of
    [(cred', Api.Lovelace n)] ->
      if cred PlutusTx.== cred'
        then (n PlutusTx.== quantity) || PlutusTx.traceError "Wrong quantity."
        else PlutusTx.traceError "Wrong credential."
    _ -> PlutusTx.traceError "Wrong withdrawal."

checkWithdrawalVersionedScript :: Script.Versioned Script.Script
checkWithdrawalVersionedScript =
  Script.toVersioned $
    Script.MultiPurposeScript @() $
      Script.toScript $$(PlutusTx.compile [||script||])
  where
    script =
      Script.mkMultiPurposeScript $
        Script.falseTypedMultiPurposeScript `Script.withRewardingPurpose` checkWithdrawalPurpose

testWithdrawingScript :: (MonadBlockChain m) => Integer -> Integer -> m ()
testWithdrawingScript n1 n2 =
  void $
    validateTxSkel $
      txSkelTemplate
        { txSkelSigners = [wallet 1],
          txSkelWithdrawals = scriptWithdrawal checkWithdrawalVersionedScript (someTxSkelRedeemer (n1 * 1_000 :: Integer)) $ Api.Lovelace $ n2 * 1_000
        }

tests :: TestTree
tests =
  testGroup
    "Withdrawing scripts"
    [ testCase "We can use a withdrawing script" $
        testSucceeds $
          testWithdrawingScript 2 2,
      testCase "But the script might fail" $
        testFailsInPhase2 $
          testWithdrawingScript 2 1
    ]
