module Cooked.BasicUsageSpec where

import Control.Monad
import Cooked
import Data.Map qualified as Map
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.V3.Typed.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as V3
import Test.Tasty
import Test.Tasty.HUnit

alice, bob, carrie :: Wallet
alice = wallet 1
bob = wallet 2
carrie = wallet 3

pkToPk :: (MonadBlockChain m) => Wallet -> Wallet -> Integer -> m ()
pkToPk sender recipient amount =
  void $
    validateTxSkel $
      txSkelTemplate
        { txSkelOuts = [recipient `receives` Value (Script.ada amount)],
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
        { txSkelMints = txSkelMintsFromList [(Script.toVersioned Script.trueMintingMPScript, emptyTxSkelRedeemer, "banana", 10)],
          txSkelOuts = [alice `receives` Value (Script.multiPurposeScriptValue Script.trueMintingMPScript "banana" 10)],
          txSkelSigners = [alice]
        }

payToAlwaysTrueValidator :: (MonadBlockChain m) => m V3.TxOutRef
payToAlwaysTrueValidator =
  head
    <$> ( validateTxSkel' $
            txSkelTemplate
              { txSkelOuts = [Script.trueSpendingMPScript @() `receives` Value (Script.ada 10)],
                txSkelSigners = [alice]
              }
        )

consumeAlwaysTrueValidator :: (MonadBlockChain m) => m ()
consumeAlwaysTrueValidator = do
  outref <- payToAlwaysTrueValidator
  void $
    validateTxSkel $
      txSkelTemplate
        { txSkelIns = Map.fromList [(outref, someTxSkelRedeemer ())],
          txSkelOuts = [alice `receives` Value (Script.ada 10)],
          txSkelSigners = [alice]
        }

tests :: TestTree
tests =
  testGroup
    "Basic usage"
    [ testCase "Payment from alice to bob, with auto-balancing" $ testSucceeds $ pkToPk alice bob 10,
      testCase "Circular payments of 10 ada between alice bob and carrie" $ testSucceeds multiplePksToPks,
      testCase "Minting quick tokens" $ testSucceeds mintingQuickValue,
      testCase "Paying to the always true validator" $ testSucceeds payToAlwaysTrueValidator,
      testCase "Consuming the always true validator" $ testSucceeds consumeAlwaysTrueValidator
    ]
