module Cooked.WithdrawalsSpec where

import Cooked
import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx qualified
import PlutusTx.AssocMap qualified as PMap
import PlutusTx.Prelude qualified as PlutusTx
import Test.Tasty

{-# INLINEABLE checkWithdrawalPurpose #-}
checkWithdrawalPurpose :: Script.RewardingPurposeType' Integer Api.TxInfo
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
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSigners = [wallet 1],
        txSkelWithdrawals = scriptWithdrawal checkWithdrawalVersionedScript (someTxSkelRedeemer (n1 * 1_000 :: Integer)) $ Api.Lovelace $ n2 * 1_000
      }

tests :: TestTree
tests =
  testGroup
    "Withdrawing scripts"
    [ testCooked "We can use a withdrawing script" $ mustSucceedTest $ testWithdrawingScript 2 2,
      testCooked "But the script might fail" $ mustFailInPhase2Test $ testWithdrawingScript 2 1
    ]
