module Cooked.BasicUsageSpec where

import Control.Monad
import Cooked
import Data.Default
import Data.Map qualified as Map
import Debug.Trace
import Plutus.Script.Utils.Scripts qualified as Pl
import Plutus.Script.Utils.Typed qualified as Pl
import Plutus.Script.Utils.V3.Generators qualified as Pl
import Plutus.Script.Utils.V3.Typed.Scripts qualified as Pl
import PlutusLedgerApi.V3 qualified as Pl
import PlutusTx qualified
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
          txSkelSigners = [alice],
          txSkelOpts = def {txOptEnsureMinAda = True}
        }

payToAlwaysTrueValidator :: (MonadBlockChain m) => m (Pl.TxOutRef, Pl.TxOut)
payToAlwaysTrueValidator = do
  tx <-
    validateTxSkel $
      txSkelTemplate
        { txSkelOuts =
            [ paysScript
                (alwaysTrueValidator @MockContract)
                ()
                (ada 10)
            ],
          txSkelSigners = [alice]
        }
  return $ head $ utxosFromCardanoTx tx

consumeAlwaysTrueValidator :: (MonadBlockChain m) => m ()
consumeAlwaysTrueValidator = do
  (outref, out) <- payToAlwaysTrueValidator
  void $
    validateTxSkel $
      txSkelTemplate
        { txSkelIns = Map.fromList [(outref, TxSkelRedeemerForScript ())],
          txSkelOuts = [paysPK (walletPKHash alice) (ada 10)],
          txSkelSigners = [alice]
        }

tests :: TestTree
tests =
  testGroup
    "Basic usage"
    [ testCase "Payment from alice to bob, with auto-balancing" $ testSucceedsFrom def def (pkToPk alice bob 10),
      testCase "Circular payments of 10 ada between alice bob and carrie" $ testSucceedsFrom def def multiplePksToPks,
      testCase "Minting quick tokens" $ testSucceedsFrom def def mintingQuickValue,
      testCase "Paying to the always true validator" $ testSucceedsFrom def def payToAlwaysTrueValidator,
      testCase "Consuming the always true validator" $ testSucceedsFrom def def consumeAlwaysTrueValidator
    ]
