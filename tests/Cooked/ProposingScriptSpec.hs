module Cooked.ProposingScriptSpec where

import Control.Monad
import Cooked
import Data.Map qualified as Map
import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx.AssocMap qualified as PlutusTx
import PlutusTx.Builtins qualified as PlutusTx hiding (head)
import PlutusTx.Eq qualified as PlutusTx
import PlutusTx.IsData qualified as PlutusTx
import PlutusTx.TH qualified as PlutusTx
import PlutusTx.Trace qualified as PlutusTx
import Test.Tasty
import Test.Tasty.HUnit

{-# INLINEABLE checkParameterChangeProposingPurpose #-}
checkParameterChangeProposingPurpose :: Script.ProposingPurposeType ()
checkParameterChangeProposingPurpose _ (Api.ProposalProcedure _ _ (Api.ParameterChange _ (Api.ChangedParameters dat) _)) _ _ =
  let innerMap = PlutusTx.unsafeFromBuiltinData @(PlutusTx.Map PlutusTx.Integer PlutusTx.Integer) dat
   in ((PlutusTx.toList innerMap PlutusTx.== [(0, 100)]) || PlutusTx.traceError "wrong map")
checkParameterChangeProposingPurpose _ _ _ _ = PlutusTx.traceError "Wrong proposal procedure"

checkProposingScript :: Script.Versioned Script.Script
checkProposingScript =
  Script.toVersioned $
    Script.MultiPurposeScript @() $
      Script.toScript $$(PlutusTx.compile [||script||])
  where
    script =
      Script.mkMultiPurposeScript $
        Script.falseTypedMultiPurposeScript `Script.withProposingPurpose` checkParameterChangeProposingPurpose

-- | A dummy false proposing validator
alwaysFalseProposingValidator :: Script.Versioned Script.Script
alwaysFalseProposingValidator =
  Script.toVersioned $
    Script.MultiPurposeScript @() $
      Script.toScript $$(PlutusTx.compile [||script||])
  where
    script = Script.mkMultiPurposeScript Script.falseTypedMultiPurposeScript

-- | A dummy true proposing validator
alwaysTrueProposingValidator :: Script.Versioned Script.Script
alwaysTrueProposingValidator =
  Script.toVersioned $
    Script.MultiPurposeScript @() $
      Script.toScript $$(PlutusTx.compile [||script||])
  where
    script =
      Script.mkMultiPurposeScript $
        Script.falseTypedMultiPurposeScript `Script.withProposingPurpose` (\_ _ () () -> True)

testProposingScript :: (MonadBlockChain m) => Script.Versioned Script.Script -> TxGovAction -> m ()
testProposingScript script govAction =
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelSigners = [wallet 1],
          txSkelProposals = [simpleTxSkelProposal (wallet 1) govAction `withWitness` (script, emptyTxSkelRedeemer)]
        }

testProposingRefScript :: (MonadBlockChain m) => Script.Versioned Script.Script -> TxGovAction -> m ()
testProposingRefScript script govAction = do
  pOutRef : _ <-
    validateTxSkel' $
      txSkelTemplate
        { txSkelOuts =
            [ wallet 1 `receives` ReferenceScript script,
              wallet 1 `receives` Value (Script.ada 10)
            ],
          txSkelSigners = [wallet 1]
        }
  void $
    validateTxSkel $
      txSkelTemplate
        { txSkelSigners = [wallet 1],
          txSkelProposals = [simpleTxSkelProposal (wallet 1) govAction `withWitness` (script, emptyTxSkelRedeemer `withReferenceInput` pOutRef)]
        }

tests :: TestTree
tests =
  testGroup
    "Proposing scripts"
    [ testCase "The always True proposing script succeeds" $
        testSucceeds $
          testProposingScript alwaysTrueProposingValidator (TxGovActionTreasuryWithdrawals Map.empty),
      testCase "The always True proposing script suceeds as a reference script" $
        testSucceeds $
          testProposingRefScript alwaysTrueProposingValidator (TxGovActionTreasuryWithdrawals Map.empty),
      testCase "The always False proposing script fails" $
        testFailsInPhase2 $
          testProposingScript alwaysFalseProposingValidator (TxGovActionTreasuryWithdrawals Map.empty),
      testCase "A more advanced proposing script can succeed" $
        testSucceeds $
          testProposingScript checkProposingScript (TxGovActionParameterChange [FeePerByte 100]),
      testCase "A more advanced proposing script can succeed as a reference script" $
        testSucceeds $
          testProposingRefScript checkProposingScript (TxGovActionParameterChange [FeePerByte 100]),
      testCase "Proposing scripts are restricted to parameter changes or treasury withdrawals" $
        testFailsInPhase1 $
          testProposingScript alwaysFalseProposingValidator TxGovActionNoConfidence
    ]
