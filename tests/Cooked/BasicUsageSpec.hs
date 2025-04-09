module Cooked.BasicUsageSpec where

import Control.Monad
import Cooked
import Data.Map qualified as Map
import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V3 qualified as Api
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
        { txSkelMints = txSkelMintsFromList [mint Script.trueMintingMPScript emptyTxSkelRedeemer (Api.TokenName "banana") 10],
          txSkelOuts = [alice `receives` Value (Script.multiPurposeScriptValue Script.trueMintingMPScript (Api.TokenName "banana") 10)],
          txSkelSigners = [alice]
        }

payToAlwaysTrueValidator :: (MonadBlockChain m) => m Api.TxOutRef
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
  validateTxSkel_ $
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
