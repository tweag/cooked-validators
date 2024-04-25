{-# LANGUAGE OverloadedStrings #-}

module Cooked.BasicUsageSpec where

import Control.Monad
import Cooked
import Data.Default
import qualified Data.Map as Map
import qualified Plutus.Script.Utils.Scripts as Pl
import Test.Tasty
import Test.Tasty.HUnit

alice, bob, carrie :: Wallet
[alice, bob, carrie] = wallet <$> [1, 2, 3]

pkToPk :: (MonadBlockChain m) => Wallet -> Wallet -> Integer -> m ()
pkToPk sender recipient amount =
  void $
    validateTxSkel $
      txSkelTemplate
        { txSkelOuts = [paysPK (walletPKHash recipient) (ada amount)],
          txSkelSigners = [sender]
        }

multiplePksToPks :: (MonadBlockChain m) => m ()
multiplePksToPks =
  do
    pkToPk alice bob 10
    pkToPk bob carrie 10
    pkToPk carrie alice 10

mintingQuickValue :: (MonadBlockChain m) => m ()
mintingQuickValue =
  void $
    validateTxSkel $
      txSkelTemplate
        { txSkelMints = txSkelMintsFromList [(Pl.Versioned quickCurrencyPolicy Pl.PlutusV3, NoMintsRedeemer, "banana", 10)],
          txSkelOuts = [paysPK (walletPKHash alice) (quickValue "banana" 10)],
          txSkelSigners = [alice]
        }

tests :: TestTree
tests =
  testGroup
    "Basic usage"
    [ testCase "Payment from alice to bob, with auto-balancing" $ testSucceedsFrom def def (pkToPk alice bob 10),
      testCase "Circular payments of 10 ada between alice bob and carrie" $ testSucceedsFrom def def multiplePksToPks
    ]
