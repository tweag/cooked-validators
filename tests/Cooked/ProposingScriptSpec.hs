module Cooked.ProposingScriptSpec where

import Control.Monad
import Cooked
import Data.Default
import Data.Map qualified as Map
import Ledger.Index qualified as Ledger
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script
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
           in if innerMap PlutusTx.== PlutusTx.safeFromList [(0, 100)] then () else PlutusTx.traceError "wrong map"
        _ -> PlutusTx.traceError "Wrong proposal procedure"

checkProposingScript :: Script.Versioned Script.Script
checkProposingScript = mkProposingScript $$(PlutusTx.compile [||checkParameterChangeScript||])

testProposingScript :: (MonadBlockChain m) => Script.Versioned Script.Script -> TxGovAction -> m ()
testProposingScript script govAction =
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelSigners = [wallet 1],
          txSkelProposals = [simpleTxSkelProposal (wallet 1) govAction `withWitness` (script, txSkelEmptyRedeemer)]
        }

testProposingRefScript :: (MonadBlockChain m) => Script.Versioned Script.Script -> TxGovAction -> m ()
testProposingRefScript script govAction = do
  pOutRef : _ <-
    validateTxSkel' $
      txSkelTemplate
        { txSkelOuts = [paysPK (wallet 1) (Script.ada 2) `withReferenceScript` script, paysPK (wallet 1) (Script.ada 10)],
          txSkelSigners = [wallet 1]
        }
  void $
    validateTxSkel $
      txSkelTemplate
        { txSkelSigners = [wallet 1],
          txSkelProposals = [simpleTxSkelProposal (wallet 1) govAction `withWitness` (script, txSkelEmptyRedeemerAndReferenceScript pOutRef)]
        }

tests :: TestTree
tests =
  testGroup
    "Proposing scripts"
    [ testCase "The always True proposing script succeeds" $
        testSucceeds def $
          testProposingScript alwaysTrueProposingValidator (TxGovActionTreasuryWithdrawals Map.empty),
      testCase "The always True proposing script suceeds as a reference script" $
        testSucceeds def $
          testProposingRefScript alwaysTrueProposingValidator (TxGovActionTreasuryWithdrawals Map.empty),
      testCase "The always False proposing script fails" $
        testFails def (isCekEvaluationFailure def) $
          testProposingScript alwaysFalseProposingValidator (TxGovActionTreasuryWithdrawals Map.empty),
      testCase "A more advanced proposing script can succeed" $
        testSucceeds def $
          testProposingScript checkProposingScript (TxGovActionParameterChange [FeePerByte 100]),
      testCase "A more advanced proposing script can succeed as a reference script" $
        testSucceeds def $
          testProposingRefScript checkProposingScript (TxGovActionParameterChange [FeePerByte 100]),
      testCase "Proposing scripts are restricted to parameter changes or treasury withdrawals" $
        testFails def (\case (MCEValidationError Ledger.Phase1 _) -> testBool True; _ -> testBool False) $
          testProposingScript alwaysFalseProposingValidator TxGovActionNoConfidence
    ]
