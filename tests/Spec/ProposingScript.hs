module Spec.ProposingScript where

import Cooked
import Plutus.ProposingScript
import Test.Tasty

testProposingScript ::
  (MonadBlockChain m) =>
  -- | Whether or not to automatically fetch a reference script
  Bool ->
  -- | Whether or not to automatically attach the constitution
  Bool ->
  -- | The official constitution script
  VScript ->
  -- | The optionally attached unofficial constitution script
  Maybe VScript ->
  -- | The governance action to propose
  TxSkelGovAction IsScript ->
  m ()
testProposingScript autoRefScript autoConstitution constitution mScript govAction = do
  setConstitutionScript constitution
  validateTxSkel_ $
    txSkelTemplate
      { txSkelOuts = [wallet 1 `receives` ReferenceScript constitution],
        txSkelSigners = [wallet 1]
      }
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSigners = [wallet 1],
        txSkelProposals =
          [ TxSkelProposal
              (wallet 1)
              govAction
              ( if autoConstitution
                  then
                    Nothing
                  else
                    ( \vScript ->
                        Just $
                          UserRedeemedScript
                            vScript
                            ( if autoRefScript
                                then emptyTxSkelRedeemer
                                else emptyTxSkelRedeemerNoAutoFill
                            )
                    )
                      =<< mScript
              )
              Nothing
          ]
      }

tests :: TestTree
tests =
  testGroup
    "Proposing scripts"
    [ testGroup
        "No automated constitution attachment"
        [ testCooked "Failure when executing the wrong constitution script" $
            mustFailInPhase1WithMsgTest "InvalidPolicyHash" $
              testProposingScript False False checkProposingScript (Just alwaysTrueProposingValidator) (ParameterChange [FeePerByte 100]),
          testCooked "Success when executing the right constitution script" $
            mustSucceedTest $
              testProposingScript False False alwaysTrueProposingValidator (Just alwaysTrueProposingValidator) (ParameterChange [FeePerByte 100]),
          testCooked "Success when executing a more complex constitution script" $
            mustSucceedTest $
              testProposingScript False False checkProposingScript (Just checkProposingScript) (ParameterChange [FeePerByte 100]),
          testCooked "Failure when executing a more complex constitution script with the wrong proposal" $
            mustFailInPhase2Test $
              testProposingScript False False checkProposingScript (Just checkProposingScript) (ParameterChange [FeePerByte 50]),
          testCooked "Success when executing a more complex constitution script as a reference script" $
            mustSucceedTest (testProposingScript True False checkProposingScript (Just checkProposingScript) (ParameterChange [FeePerByte 100]))
              `withJournalProp` happened "MCLogAddedReferenceScript"
        ],
      testGroup
        "Automated constitution attachment"
        [ testCooked "Success when auto assigning the constitution script" $
            mustSucceedTest $
              testProposingScript False True checkProposingScript Nothing (ParameterChange [FeePerByte 100]),
          testCooked "Success when auto assigning the constitution script and using it as a reference script" $
            mustSucceedTest (testProposingScript True True checkProposingScript Nothing (ParameterChange [FeePerByte 100]))
              `withJournalProp` happened "MCLogAddedReferenceScript",
          testCooked "Success when auto assigning the constitution script while overriding an existing one" $
            mustSucceedTest $
              testProposingScript False True checkProposingScript (Just alwaysFalseProposingValidator) (ParameterChange [FeePerByte 100])
        ]
    ]
