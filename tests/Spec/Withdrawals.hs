module Spec.Withdrawals where

import Cooked
import Optics.Core
import Plutus.Withdrawals
import PlutusLedgerApi.V3 qualified as Api
import Test.Tasty

alice :: Wallet
alice = wallet 1

testWithdrawingScript :: (MonadModalBlockChain m) => Integer -> Integer -> m ()
testWithdrawingScript inRedeemer actual = do
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [alice],
        txSkelCertificates = [scriptCertificate checkWithdrawalMPScript () StakingRegister]
      }
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [alice],
        txSkelWithdrawals = txSkelWithdrawalsFromList [scriptWithdrawal checkWithdrawalMPScript (inRedeemer * 1_000_000)]
      }

testWithdrawingPK :: (MonadModalBlockChain m) => m ()
testWithdrawingPK = do
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [alice],
        txSkelCertificates = [pubKeyCertificate alice (StakingRegisterDelegate $ Api.DelegVote Api.DRepAlwaysAbstain)]
      }
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [alice],
        txSkelWithdrawals = txSkelWithdrawalsFromList [pubKeyWithdrawal alice]
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
