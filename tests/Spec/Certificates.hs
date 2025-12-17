module Spec.Certificates where

import Cooked
import Data.Default
import Optics.Core
import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Test.Tasty

alice :: Wallet
alice = wallet 1

bob :: Wallet
bob = wallet 1

publishCertificate :: (MonadModalBlockChain m) => TxSkelCertificate -> m ()
publishCertificate cert =
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [alice],
        txSkelCertificates = [cert],
        -- This fee is huge, we use it to force an output to be consumed when
        -- the balancing equation is in favour of the output value.
        txSkelOpts = def {txSkelOptFeePolicy = ManualFee 5_000_000}
      }

withdraw :: (MonadBlockChain m) => User IsEither Redemption -> m ()
withdraw user =
  validateTxSkel_ $
    txSkelTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [alice],
        txSkelWithdrawals = review txSkelWithdrawalsListI [Withdrawal user 0],
        txSkelOpts = def {txSkelOptFeePolicy = ManualFee 5_000_000}
      }

trueScriptUser :: User IsEither Redemption
trueScriptUser = UserRedeemedScript (toVScript $ Script.trueMPScript @()) emptyTxSkelRedeemer

falseScriptUser :: User IsEither Redemption
falseScriptUser = UserRedeemedScript (toVScript $ Script.falseMPScript @()) emptyTxSkelRedeemer

aliceUser :: User IsEither Redemption
aliceUser = UserPubKey $ Script.toPubKeyHash alice

bobUser :: User IsEither Redemption
bobUser = UserPubKey $ Script.toPubKeyHash bob

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
            mustFailInPhase2WithMsgTest "Unsupported purpose: Certifying" $
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
            mustFailInPhase2WithMsgTest "Unsupported purpose: Certifying" $
              publishCertificate $
                TxSkelCertificate falseScriptUser DRepRegister
        ],
      -- testGroup
      --   "Pool registration certificates"
      --   [
      --     testCooked "Success when registering a pool" $
      --       mustSucceedTest $
      --         publishCertificate $
      --           TxSkelCertificate aliceUser (PoolRegister undefined)
      --   ],
      testGroup
        "Staking delegation certificates"
        [ testCooked "Failure when delegating vote for a non registered credential" $
            mustFailInPhase1Test $
              publishCertificate $
                TxSkelCertificate aliceUser (StakingDelegate $ Api.DelegStake $ Script.toPubKeyHash bob),
          testCooked "Success when delegating vote for a registered credential" $
            mustSucceedTest $ do
              publishCertificate $ TxSkelCertificate aliceUser StakingRegister
              publishCertificate $ TxSkelCertificate aliceUser (StakingDelegate $ Api.DelegVote Api.DRepAlwaysAbstain)
        ]
    ]

-- gov action: 1
-- d rep: 1
-- pool: 0
-- address: 2
