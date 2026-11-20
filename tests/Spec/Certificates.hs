module Spec.Certificates where

import Cooked
import Ledger.Slot qualified as P.Ledger
import Optics.Core
import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Test.Tasty

alice :: Wallet
alice = wallet 1

bob :: Wallet
bob = wallet 2

publishCertificateWithSignatories :: [TxSkelSignatory] -> TxSkelCertificate -> DirectMockChain ()
publishCertificateWithSignatories signatories cert = do
  forceOutputs_ [alice `receives` AdaValue 100]
  validateTxSkel_ $
    txSkelEmulatorTemplate
      { txSkelSignatories = signatories,
        txSkelCertificates = [cert]
      }

publishCertificate :: TxSkelCertificate -> DirectMockChain ()
publishCertificate =
  publishCertificateWithSignatories [signatoryWallet alice]

publishPoolCertificate :: Pool -> TxSkelCertificate -> DirectMockChain ()
publishPoolCertificate p =
  publishCertificateWithSignatories [signatoryWallet alice, signatoryPoolCold p]

registerPool :: Pool -> DirectMockChain ()
registerPool p =
  publishPoolCertificate p $
    TxSkelCertificate
      (UserPubKey $ poolId p)
      (PoolRegister $ poolVRFHash p)

retirePool :: Pool -> P.Ledger.Slot -> DirectMockChain ()
retirePool p slot =
  publishPoolCertificate p $
    TxSkelCertificate
      (UserPubKey $ poolId p)
      (PoolRetire slot)

withdraw :: User IsEither Redemption -> DirectMockChain ()
withdraw user =
  validateTxSkel_ $
    txSkelEmulatorTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [alice],
        txSkelWithdrawals = review txSkelWithdrawalsListI [Withdrawal user Nothing]
      }

trueScriptUser :: User IsEither Redemption
trueScriptUser = UserRedeemedScript Script.trueMPScript emptyTxSkelRedeemer

falseScriptUser :: User IsEither Redemption
falseScriptUser = UserRedeemedScript Script.falseMPScript emptyTxSkelRedeemer

aliceUser :: User IsEither Redemption
aliceUser = UserPubKey $ Script.toPubKeyHash alice

tests :: TestTree
tests =
  testGroup
    "Certificates"
    [ testGroup
        "Staking register certificates"
        [ testCooked "Success when registering a wallet" $
            mustSucceedTest $
              publishCertificate $
                TxSkelCertificate aliceUser StakingRegister,
          testCooked "Success when registering the true script" $
            mustSucceedTest $
              publishCertificate $
                TxSkelCertificate trueScriptUser StakingRegister,
          testCooked "Failure when registering the false script" $
            mustFailInPhase2WithMsgTest "Script used with an unsupported purpose." $
              publishCertificate $
                TxSkelCertificate falseScriptUser StakingRegister,
          testCooked "Failure when withdrawing directly after registering" $
            mustFailTest $ do
              publishCertificate $ TxSkelCertificate aliceUser StakingRegister
              withdraw aliceUser
        ],
      testGroup
        "Staking unregister certificates"
        [ testCooked "Failure when unregistering a wallet not yet registered" $
            mustFailInPhase1Test $
              publishCertificate $
                TxSkelCertificate aliceUser StakingUnRegister,
          testCooked "Success when unregistering a registered wallet" $
            mustSucceedTest $ do
              publishCertificate $ TxSkelCertificate aliceUser StakingRegister
              publishCertificate $ TxSkelCertificate aliceUser StakingUnRegister
        ],
      testGroup
        "DRep registration certificates"
        [ testCooked "We can register a wallet DRep" $
            mustSucceedTest $
              publishCertificate $
                TxSkelCertificate aliceUser DRepRegister,
          testCooked "We cannot update a DRep not register" $
            mustFailInPhase1Test $
              publishCertificate $
                TxSkelCertificate aliceUser DRepUpdate,
          testCooked "We can update a registered DRep" $
            mustSucceedTest $ do
              publishCertificate $ TxSkelCertificate aliceUser DRepRegister
              publishCertificate $ TxSkelCertificate aliceUser DRepUpdate,
          testCooked "We cannot unregister an unregistered DRep" $
            mustFailInPhase1Test $
              publishCertificate $
                TxSkelCertificate aliceUser DRepUnRegister,
          testCooked "We can unregister a registered DRep" $
            mustSucceedTest $ do
              publishCertificate $ TxSkelCertificate aliceUser DRepRegister
              publishCertificate $ TxSkelCertificate aliceUser DRepUnRegister,
          testCooked "We can use a script as DRep..." $
            mustSucceedTest $
              publishCertificate $
                TxSkelCertificate trueScriptUser DRepRegister,
          testCooked "... but the script might fail at registration" $
            mustFailInPhase2WithMsgTest "Script used with an unsupported purpose." $
              publishCertificate $
                TxSkelCertificate falseScriptUser DRepRegister,
          testCooked "We can update and unregister a script DRep" $
            mustSucceedTest $ do
              publishCertificate $ TxSkelCertificate trueScriptUser DRepRegister
              publishCertificate $ TxSkelCertificate trueScriptUser DRepUpdate
              publishCertificate $ TxSkelCertificate trueScriptUser DRepUnRegister
        ],
      testGroup
        "Staking delegation certificates"
        [ testCooked "Failure when delegating stake to an unregistered pool" $
            mustFailInPhase1Test $ do
              publishCertificate $ TxSkelCertificate aliceUser StakingRegister
              publishCertificate $
                TxSkelCertificate aliceUser (StakingDelegate $ Api.DelegStake $ poolId $ pool 1),
          testCooked "Success when delegating stake to a registered pool" $
            mustSucceedTest $ do
              let p = pool 1
              registerPool p
              publishCertificate $ TxSkelCertificate aliceUser StakingRegister
              publishCertificate $ TxSkelCertificate aliceUser (StakingDelegate $ Api.DelegStake $ poolId p),
          testCooked "Success when registering and delegating stake to a pool" $
            mustSucceedTest $ do
              let p = pool 1
              registerPool p
              publishCertificate $
                TxSkelCertificate aliceUser (StakingRegisterDelegate $ Api.DelegStake $ poolId p),
          testCooked "Success when delegating stake and vote together" $
            mustSucceedTest $ do
              let p = pool 1
              registerPool p
              publishCertificate $ TxSkelCertificate aliceUser StakingRegister
              publishCertificate $
                TxSkelCertificate
                  aliceUser
                  (StakingDelegate $ Api.DelegStakeVote (poolId p) Api.DRepAlwaysAbstain),
          testCooked "Success when registering and delegating stake and vote together" $
            mustSucceedTest $ do
              let p = pool 1
              registerPool p
              publishCertificate $
                TxSkelCertificate
                  aliceUser
                  (StakingRegisterDelegate $ Api.DelegStakeVote (poolId p) Api.DRepAlwaysAbstain),
          testCooked "Success when delegating vote to always abstain" $
            mustSucceedTest $ do
              publishCertificate $ TxSkelCertificate aliceUser StakingRegister
              publishCertificate $ TxSkelCertificate aliceUser (StakingDelegate $ Api.DelegVote Api.DRepAlwaysAbstain),
          testCooked "Success when delegating vote to always no confidence" $
            mustSucceedTest $ do
              publishCertificate $ TxSkelCertificate aliceUser StakingRegister
              publishCertificate $ TxSkelCertificate aliceUser (StakingDelegate $ Api.DelegVote Api.DRepAlwaysNoConfidence),
          testCooked "Success when delegating vote to a registered DRep credential" $
            mustSucceedTest $ do
              publishCertificate $ TxSkelCertificate aliceUser DRepRegister
              publishCertificate $ TxSkelCertificate aliceUser StakingRegister
              publishCertificate $
                TxSkelCertificate
                  aliceUser
                  (StakingDelegate $ Api.DelegVote $ Api.DRep $ Api.DRepCredential $ Script.toCredential alice)
        ],
      testGroup
        "Pool certificates"
        [ testCooked "Success when registering a pool with its cold key and VRF hash" $
            mustSucceedTest $
              registerPool (pool 1),
          testCooked "Success when retiring a registered pool" $
            mustSucceedTest $ do
              let p = pool 1
              registerPool p
              retirePool p (P.Ledger.Slot 1_000_000),
          testCooked "Failure when retiring an unregistered pool" $
            mustFailInPhase1Test $
              retirePool (pool 1) (P.Ledger.Slot 1_000_000)
        ],
      testGroup
        "Committee certificates"
        [ testCooked "Failure when registering a hot key for a non-member" $
            mustFailInPhase1Test $
              publishCertificate $
                TxSkelCertificate aliceUser (CommitteeRegisterHot $ Script.toCredential bob),
          testCooked "Failure when registering a script hot credential for a non-member" $
            mustFailInPhase1Test $
              publishCertificate $
                TxSkelCertificate
                  aliceUser
                  (CommitteeRegisterHot $ Api.ScriptCredential $ Script.toScriptHash Script.trueMPScript),
          testCooked "Failure when resigning from the committee as a non-member" $
            mustFailInPhase1Test $
              publishCertificate $
                TxSkelCertificate aliceUser CommitteeResign
        ]
    ]
