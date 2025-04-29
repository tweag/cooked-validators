module Spec.Withdrawals where

import Cooked
import Plutus.Withdrawals
import Test.Tasty

testWithdrawingScript :: (MonadBlockChain m) => Integer -> Integer -> m ()
testWithdrawingScript n1 n2 =
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSigners = [wallet 1],
        txSkelWithdrawals =
          scriptWithdrawal
            checkWithdrawalVersionedScript
            (someTxSkelRedeemer (n1 * 1_000 :: Integer))
            (n2 * 1_000)
      }

tests :: TestTree
tests =
  testGroup
    "Withdrawing scripts"
    [ testCooked "We can use a withdrawing script" $ mustSucceedTest $ testWithdrawingScript 2 2,
      testCooked "But the script might fail" $ mustFailInPhase2Test $ testWithdrawingScript 2 1
    ]
