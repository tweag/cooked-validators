module Spec.Withdrawals where

import Control.Monad
import Cooked
import Data.Maybe
import Optics.Core.Extras
import Plutus.Withdrawals
import PlutusLedgerApi.V3 qualified as Api
import Test.Tasty

alice :: Wallet
alice = wallet 1

testWithdrawingScript ::
  Maybe (User IsEither Redemption) ->
  User IsEither Redemption ->
  Maybe Integer ->
  StagedMockChain ()
testWithdrawingScript userCertifying userRewarding mAmount = do
  when (isJust userCertifying) $
    validateTxSkel_ $
      txSkelTemplate
        { txSkelSignatories = txSkelSignatoriesFromList [alice],
          txSkelCertificates =
            [ TxSkelCertificate (fromJust userCertifying) $
                if is userScriptHashAF (fromJust userCertifying)
                  then StakingRegister
                  else StakingRegisterDelegate (Api.DelegVote Api.DRepAlwaysAbstain)
            ]
        }
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [alice],
        txSkelWithdrawals = txSkelWithdrawalsFromList [Withdrawal userRewarding (Api.Lovelace . (1_000_000 *) <$> mAmount)]
      }

aliceUser :: User IsEither Redemption
aliceUser = UserPubKey alice

scriptUserCertifying :: User IsEither Redemption
scriptUserCertifying = UserRedeemedScript checkWithdrawalMPScript emptyTxSkelRedeemer

scriptUserWithdrawing :: Integer -> User IsEither Redemption
scriptUserWithdrawing amount = UserRedeemedScript checkWithdrawalMPScript (someTxSkelRedeemer (amount * 1_000_000))

tests :: TestTree
tests =
  testGroup
    "Withdrawals"
    [ testCooked "We cannot withdraw rewards without first registering..." $
        mustFailTest
          ( testWithdrawingScript
              Nothing
              (scriptUserWithdrawing 0)
              Nothing
          )
          `withFailureProp` isPhase1FailureWithMsg "WithdrawalsNotInRewardsCERTS",
      testCooked "We can register a script and use it to withdraw rewards..." $
        mustSucceedTest
          ( testWithdrawingScript
              (Just scriptUserCertifying)
              (scriptUserWithdrawing 0)
              Nothing
          )
          `withJournalProp` happened "MCLogAutoFilledWithdrawalAmount",
      testCooked ".. but the script's logic might say No" $
        mustFailTest
          ( testWithdrawingScript
              (Just scriptUserCertifying)
              (scriptUserWithdrawing 2)
              Nothing
          )
          `withFailureProp` isPhase2FailureWithMsg "Wrong quantity: 0 instead of 2000000"
          `withJournalProp` happened "MCLogAutoFilledWithdrawalAmount",
      testCooked "We cannot withdraw more than our rewards (0)" $
        mustFailTest
          ( testWithdrawingScript
              (Just scriptUserCertifying)
              (scriptUserWithdrawing 2)
              (Just 2)
          )
          `withFailureProp` isPhase1FailureWithMsg "WithdrawalsNotInRewardsCERTS"
          `withJournalProp` didNotHappen "MCLogAutoFilledWithdrawalAmount",
      testCooked "A peer can also make a withdrawal" $
        mustSucceedTest
          ( testWithdrawingScript
              (Just aliceUser)
              aliceUser
              Nothing
          )
          `withJournalProp` happened "MCLogAutoFilledWithdrawalAmount"
    ]
