module Cooked.ProposingScriptSpec where

import Cooked
import Data.Map qualified as Map
import Plutus.ProposingScript
import Plutus.Script.Utils.V3 qualified as Script
import Test.Tasty

testProposingScript :: (MonadBlockChain m) => Script.Versioned Script.Script -> TxGovAction -> m ()
testProposingScript script govAction =
  validateTxSkel_
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
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSigners = [wallet 1],
        txSkelProposals = [simpleTxSkelProposal (wallet 1) govAction `withWitness` (script, emptyTxSkelRedeemer `withReferenceInput` pOutRef)]
      }

tests :: TestTree
tests =
  testGroup
    "Proposing scripts"
    [ testCooked "The always True proposing script succeeds" $
        mustSucceedTest $
          testProposingScript alwaysTrueProposingValidator (TxGovActionTreasuryWithdrawals Map.empty),
      testCooked "The always True proposing script suceeds as a reference script" $
        mustSucceedTest $
          testProposingRefScript alwaysTrueProposingValidator (TxGovActionTreasuryWithdrawals Map.empty),
      testCooked "The always False proposing script fails" $
        mustFailInPhase2Test $
          testProposingScript alwaysFalseProposingValidator (TxGovActionTreasuryWithdrawals Map.empty),
      testCooked "A more advanced proposing script can succeed" $
        mustSucceedTest $
          testProposingScript checkProposingScript (TxGovActionParameterChange [FeePerByte 100]),
      testCooked "A more advanced proposing script can succeed as a reference script" $
        mustSucceedTest $
          testProposingRefScript checkProposingScript (TxGovActionParameterChange [FeePerByte 100]),
      testCooked "Proposing scripts are restricted to parameter changes or treasury withdrawals" $
        mustFailInPhase1Test $
          testProposingScript alwaysFalseProposingValidator TxGovActionNoConfidence
    ]
