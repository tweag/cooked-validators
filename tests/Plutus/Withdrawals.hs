{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Withdrawals where

import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Prelude

{-# INLINEABLE checkWithdrawalPurpose #-}
checkWithdrawalPurpose :: Script.RewardingPurposeType' Integer Api.TxInfo
checkWithdrawalPurpose cred quantity (Api.TxInfo {txInfoWdrl}) =
  case Map.toList txInfoWdrl of
    [(cred', Api.Lovelace n)] ->
      if cred == cred'
        then (n == quantity) || traceError "Wrong quantity."
        else traceError "Wrong credential."
    _ -> traceError "Wrong withdrawal."

checkWithdrawalMPScript :: Script.MultiPurposeScript ()
checkWithdrawalMPScript =
  Script.MultiPurposeScript $ Script.toScript $$(PlutusTx.compile [||script||])
  where
    script =
      Script.mkMultiPurposeScript
        $ Script.falseTypedMultiPurposeScript
        `Script.withRewardingPurpose` checkWithdrawalPurpose

trueWithdrawalMPScript :: Script.MultiPurposeScript ()
trueWithdrawalMPScript =
  Script.MultiPurposeScript $ Script.toScript $$(PlutusTx.compile [||script||])
  where
    script =
      Script.mkMultiPurposeScript
        $ Script.falseTypedMultiPurposeScript
        `Script.withRewardingPurpose` ((\_ _ _ -> True) :: Script.RewardingPurposeType' Integer Api.TxInfo)
