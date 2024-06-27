module Cooked.ProposingScriptSpec where

import Control.Monad
import Cooked
import Data.Default
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.AssocMap qualified as PlutusTx
import PlutusTx.Builtins qualified as PlutusTx hiding (head)
import PlutusTx.Eq qualified as PlutusTx
import PlutusTx.IsData qualified as PlutusTx
import PlutusTx.List qualified as PlutusTx
import PlutusTx.TH qualified as PlutusTx
import PlutusTx.Trace qualified as PlutusTx
import Test.Tasty
import Test.Tasty.HUnit

checkParameterChangeScript :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
checkParameterChangeScript _ ctx =
  let scriptContext = PlutusTx.unsafeFromBuiltinData @Api.ScriptContext ctx
      proposalProcedure = PlutusTx.head $ Api.txInfoProposalProcedures $ Api.scriptContextTxInfo scriptContext
   in case Api.ppGovernanceAction proposalProcedure of
        Api.ParameterChange _ (Api.ChangedParameters dat) _ ->
          let innerMap = PlutusTx.unsafeFromBuiltinData @(PlutusTx.Map PlutusTx.Integer PlutusTx.Integer) dat
           in if innerMap PlutusTx.== PlutusTx.fromList [(0, 100)] then () else PlutusTx.traceError "wrong map"
        _ -> PlutusTx.traceError "Wrong proposal procedure"

checkProposingScript :: Script.Versioned Script.Script
checkProposingScript = mkProposingScript $$(PlutusTx.compile [||checkParameterChangeScript||])

testProposingScript :: (MonadBlockChain m) => Script.Versioned Script.Script -> m ()
testProposingScript script =
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelSigners = [wallet 1],
          txSkelProposals =
            [ simpleTxSkelProposal
                (wallet 1)
                (TxGovActionParameterChange [FeePerByte 100])
                `withWitness` (script, TxSkelNoRedeemer)
            ]
        }

tests :: TestTree
tests =
  testGroup
    "Proposing scripts"
    [ testCase "The always True proposing script succeeds" $
        testSucceeds def $
          testProposingScript alwaysTrueProposingValidator,
      testCase "The always False proposing script fails" $
        testFails def (isCekEvaluationFailure def) $
          testProposingScript alwaysFalseProposingValidator,
      testCase "A more advanced proposing script can succeed" $
        testSucceeds def $
          testProposingScript checkProposingScript
    ]
