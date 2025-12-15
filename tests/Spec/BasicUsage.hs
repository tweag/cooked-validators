module Spec.BasicUsage where

import Cooked
import Data.Map qualified as Map
import Optics.Core
import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Test.Tasty

alice, bob, carrie :: Wallet
alice = wallet 1
bob = wallet 2
carrie = wallet 3

pkToPk :: (MonadBlockChain m) => Wallet -> Wallet -> Integer -> m ()
pkToPk sender recipient amount =
  validateTxSkel_ $
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
  validateTxSkel_
    txSkelTemplate
      { txSkelMints = review txSkelMintsListI [mint (Script.trueMintingMPScript @()) () (Api.TokenName "banana") 10],
        txSkelOuts = [alice `receives` Value (Script.multiPurposeScriptValue (Script.trueMintingMPScript @()) (Api.TokenName "banana") 10)],
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
    [ testCooked "Payment from alice to bob, with auto-balancing" $ mustSucceedTest $ pkToPk alice bob 10,
      testCooked "Circular payments of 10 ada between alice bob and carrie" $ mustSucceedTest multiplePksToPks,
      testCooked "Minting quick tokens" $ mustSucceedTest mintingQuickValue,
      testCooked "Paying to the always true validator" $ mustSucceedTest payToAlwaysTrueValidator,
      testCooked "Consuming the always true validator" $ mustSucceedTest consumeAlwaysTrueValidator
    ]
