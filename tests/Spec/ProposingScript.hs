module Spec.ProposingScript where

import Cooked
import Plutus.ProposingScript
import Plutus.Script.Utils.V3 qualified as Script
import Test.Tasty

testProposingScript :: (MonadBlockChain m) => Bool -> Script.Versioned Script.Script -> TxGovAction -> m ()
testProposingScript useRefScript script govAction = do
  setConstitutionScript script
  validateTxSkel_ $
    txSkelTemplate
      { txSkelOuts = [wallet 1 `receives` ReferenceScript script],
        txSkelSigners = [wallet 1]
      }
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSigners = [wallet 1],
        txSkelProposals =
          [ simpleTxSkelProposal (wallet 1) govAction
              `withWitness` (script, if useRefScript then emptyTxSkelRedeemer else emptyTxSkelRedeemerNoAutoFill)
          ]
      }

tests :: TestTree
tests =
  testGroup
    "Proposing scripts"
    [ testCooked "The always True proposing script succeeds" $
        mustSucceedTest $
          testProposingScript False alwaysTrueProposingValidator (TxGovActionParameterChange [FeePerByte 100]),
      testCooked "The always True proposing script suceeds as a reference script" $
        mustSucceedTest $
          testProposingScript True alwaysTrueProposingValidator (TxGovActionParameterChange [FeePerByte 100]),
      testCooked "The always False proposing script fails" $
        mustFailInPhase2Test $
          testProposingScript False alwaysFalseProposingValidator (TxGovActionParameterChange [FeePerByte 100]),
      testCooked "A more advanced proposing script can succeed" $
        mustSucceedTest $
          testProposingScript False checkProposingScript (TxGovActionParameterChange [FeePerByte 100]),
      testCooked "A more advanced proposing script can succeed as a reference script" $
        mustSucceedTest $
          testProposingScript True checkProposingScript (TxGovActionParameterChange [FeePerByte 100]),
      testCooked "Proposing scripts are restricted to parameter changes or treasury withdrawals" $
        mustFailInPhase2Test $
          testProposingScript False alwaysTrueProposingValidator TxGovActionNoConfidence
    ]
