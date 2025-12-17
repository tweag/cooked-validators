module Spec.Withdrawals where

import Cooked
import Optics.Core
import Plutus.Withdrawals
import PlutusLedgerApi.V3 qualified as Api
import Test.Tasty

testWithdrawingScript :: (MonadModalBlockChain m) => Integer -> Integer -> Integer -> Integer -> m ()
testWithdrawingScript reward deposit inRedeemer actual = do
  registerStakingCred checkWithdrawalMPScript (reward * 1_000) (deposit * 1_000)
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [wallet 1],
        txSkelWithdrawals = review txSkelWithdrawalsListI [scriptWithdrawal checkWithdrawalMPScript (inRedeemer * 1_000) (actual * 1_000)]
      }

testWithdrawingPK :: (MonadModalBlockChain m) => m ()
testWithdrawingPK = do
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [wallet 1],
        txSkelCertificates = [pubKeyCertificate (wallet 1) $ StakingRegisterDelegate $ Api.DelegVote Api.DRepAlwaysAbstain]
      }
  registerStakingCred (wallet 1) 2_000_000 0
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [wallet 1],
        txSkelWithdrawals = review txSkelWithdrawalsListI [pubKeyWithdrawal (wallet 1) 2_000_000]
      }

tests :: TestTree
tests =
  testGroup
    "Withdrawing scripts"
    [ testCooked "We can use a withdrawing script" $ mustSucceedTest $ testWithdrawingScript 2 2 2 2,
      testCooked "But the script might fail" $ mustFailInPhase2WithMsgTest "Wrong quantity" $ testWithdrawingScript 2 2 2 1,
      testCooked "The amount of deposited lovelace is irrelevant" $ mustSucceedTest $ testWithdrawingScript 2 100 2 2,
      testCooked "We cannot withdraw more than our rewards" $ mustFailInPhase1WithMsgTest "WithdrawalsNotInRewardsCERTS" $ testWithdrawingScript 1 2 2 2,
      testCooked "We cannot withdraw less than our rewards either" $ mustFailInPhase1WithMsgTest "WithdrawalsNotInRewardsCERTS" $ testWithdrawingScript 3 2 2 2,
      testCooked "We cannot withdraw if we are not registered" $
        mustFailInPhase1WithMsgTest "WithdrawalsNotInRewardsCERTS" $
          testWithdrawingScript 2 2 2 2
            `withTweak` do
              setTweak
                (txSkelWithdrawalsL % txSkelWithdrawalsListI % _head % withdrawalUserL % userEitherScriptP % userTypedScriptAT)
                trueWithdrawalMPScript,
      testCooked "A wallet can also make a withdrawal" $
        mustSucceedTest testWithdrawingPK
    ]
