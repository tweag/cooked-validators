module Spec.Withdrawals where

import Cooked
import Plutus.Withdrawals
import Test.Tasty

testWithdrawingScript :: (MonadModalBlockChain m) => Integer -> Integer -> Integer -> Integer -> m ()
testWithdrawingScript reward deposit inRedeemer actual = do
  registerStakingCred checkWithdrawalMPScript (reward * 1_000) (deposit * 1_000)
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSigners = [wallet 1],
        txSkelWithdrawals =
          scriptWithdrawal
            checkWithdrawalMPScript
            (someTxSkelRedeemer (inRedeemer * 1_000))
            (actual * 1_000)
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
            `withTweak` setTweak txSkelWithdrawalsL (scriptWithdrawal trueWithdrawalMPScript (someTxSkelRedeemer (2_000 :: Integer)) 2_000),
      testCooked "A wallet can also make a withdrawal" $
        mustSucceedTest $
          testWithdrawingScript 2 2 2 2
            `withTweak` do
              registerStakingCred (wallet 1) 2_000 0
              setTweak txSkelWithdrawalsL (pkWithdrawal (wallet 1) 2_000)
    ]
