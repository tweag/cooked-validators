{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CrowdfundingSpec where

import Control.Monad
import Control.Monad.Writer
import Cooked.MockChain
import Cooked.Tx.Constraints
import Crowdfunding
import Data.Default
import Data.Either (isRight)
import Debug.Trace
import qualified Ledger
import qualified Ledger.Ada as Ada
import qualified Ledger.Contexts as Validation
import qualified Ledger.TimeSlot as TimeSlot
import qualified Ledger.Typed.Scripts as TScripts
import qualified Ledger.Value as Value
import qualified Plutus.Contract.StateMachine.ThreadToken as ThreadToken
import qualified Plutus.Contracts.Currency as Currency
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified PlutusTx (compile)
import Test.Hspec
import qualified Test.QuickCheck as QC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)
import Text.Heredoc

tests :: TestTree
tests =
  localOption (QuickCheckTests 25) $
    testGroup
      "Crowdfunding"
      [ ownerCanStartCampaign,
        funderCanCancel,
        otherFunderCannotCancel,
        successfulFunding,
        notEnoughFunding
      ]

traceSucceeds :: (Show e) => Either e t -> IO ()
traceSucceeds (Right _) = pure ()
traceSucceeds (Left e) = assertFailure $ "trace failed: " <> show e

traceFails :: (Show t) => Either e t -> IO ()
traceFails (Left _) = pure ()
traceFails (Right e) = assertFailure $ "trace succeeded: " <> show e

ownerCanStartCampaign :: TestTree
ownerCanStartCampaign =
  testCase "Owner can start campaing" $ traceSucceeds (runMockChain mtrace)
  where
    mtrace :: MonadMockChain m => m ()
    mtrace = do
      t <- currentTime
      txStartProject $
        ProjectData
          (Ada.adaOf 10)
          (t + 20000)
          (Ledger.PaymentPubKeyHash $ walletPKHash (wallet 1))

funderCanCancel :: TestTree
funderCanCancel =
  testCase "Funds can be cancelled" $ traceSucceeds (runMockChain mtrace)
  where
    mtrace :: MonadMockChain m => m ()
    mtrace = do
      t <- currentTime
      txStartProject $
        ProjectData
          (Ada.adaOf 10)
          (t + 20000)
          (Ledger.PaymentPubKeyHash $ walletPKHash (wallet 1))
      txFundProject (FundProjectArgs (wallet 1) (Ada.adaOf 2)) `as` wallet 2
      txCancelFund () `as` wallet 2

otherFunderCannotCancel :: TestTree
otherFunderCannotCancel =
  testCase "Funds cannot be cancelled by other" $ traceFails (runMockChain mtrace)
  where
    mtrace :: MonadMockChain m => m ()
    mtrace = do
      t <- currentTime
      txStartProject $
        ProjectData
          (Ada.adaOf 10)
          (t + 20000)
          (Ledger.PaymentPubKeyHash $ walletPKHash (wallet 1))
      txFundProject (FundProjectArgs (wallet 1) (Ada.adaOf 2)) `as` wallet 2
      txCancelFund () `as` wallet 3

successfulFunding :: TestTree
successfulFunding =
  testCase "Successful funding" $ traceSucceeds (runMockChain mtrace)
  where
    mtrace :: MonadMockChain m => m ()
    mtrace = do
      t <- currentTime
      txStartProject $
        ProjectData
          (Ada.adaOf 10)
          (t + 20000)
          (Ledger.PaymentPubKeyHash $ walletPKHash (wallet 1))
      txFundProject (FundProjectArgs (wallet 1) (Ada.adaOf 7)) `as` wallet 2
      txFundProject (FundProjectArgs (wallet 1) (Ada.adaOf 7)) `as` wallet 3
      waitNMilliSeconds 50000
      txGetFunds () `as` wallet 1

notEnoughFunding :: TestTree
notEnoughFunding =
  testCase "Not enough funding" $ traceFails (runMockChain mtrace)
  where
    mtrace :: MonadMockChain m => m ()
    mtrace = do
      t <- currentTime
      txStartProject $
        ProjectData
          (Ada.adaOf 10)
          (t + 20000)
          (Ledger.PaymentPubKeyHash $ walletPKHash (wallet 1))
      txFundProject (FundProjectArgs (wallet 1) (Ada.adaOf 2)) `as` wallet 2
      txFundProject (FundProjectArgs (wallet 1) (Ada.adaOf 2)) `as` wallet 3
      txGetFunds () `as` wallet 1
