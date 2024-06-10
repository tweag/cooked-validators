module Cooked.BalancingSpec where

import Control.Monad
import Cooked
import Data.Default
import Data.Set
import Ledger.Index qualified as Ledger
import PlutusLedgerApi.V3 qualified as Api
import Test.Tasty
import Test.Tasty.HUnit

alice, bob :: Wallet
(alice, bob) = (wallet 1, wallet 2)

initialDistributionBalancing :: InitialDistribution
initialDistributionBalancing =
  InitialDistribution $
    (<>)
      [ paysPK bob (ada 80),
        paysPK bob (ada 4 <> permanentValue "banana" 1),
        paysPK bob (ada 85),
        paysPK bob (ada 30 <> permanentValue "orange" 7),
        paysPK bob (ada 8 <> permanentValue "orange" 2),
        paysPK bob (ada 83)
      ]
      [ paysPK alice (ada 2 <> permanentValue "apple" 3),
        paysPK alice (ada 25),
        paysPK alice (ada 40 <> permanentValue "orange" 6),
        paysPK alice (ada 8),
        paysPK alice (ada 30),
        paysPK alice (ada 12 <> permanentValue "banana" 3) `withDatum` (10 :: Integer),
        paysPK alice (ada 2 <> permanentValue "banana" 7) `withReferenceScript` (alwaysTrueValidator @MockContract),
        paysPK alice (ada 100 <> permanentValue "banana" 2) `withDatumHash` ()
      ]

alicePaysToBobTemplate :: (MonadBlockChain m) => Integer -> (TxOpts -> TxOpts) -> m (TxSkel, Integer, Set Api.TxOutRef, Wallet)
alicePaysToBobTemplate val f = do
  let txSkel =
        txSkelTemplate
          { txSkelOuts = [paysPK bob (lovelace val)],
            txSkelSigners = [alice],
            txSkelOpts = f def
          }
  res <- balanceTxSkel txSkel
  void $ validateTxSkel txSkel
  return res

tests :: TestTree
tests =
  testGroup
    "Balancing"
    [ testGroup
        "Fee fixed"
        $ let setFixedFee fee txOpts = txOpts {txOptFeePolicy = ManualFee fee}
           in [ testCase "Successful 1-utxo balancing" $
                  testSucceedsFrom'
                    def
                    ( \(TxSkel {..}, fee, refs, wal) _ ->
                        testBool $
                          fee == 1_000_000 && length txSkelIns == 1 && length refs == 1 && wal == alice
                    )
                    initialDistributionBalancing
                    (alicePaysToBobTemplate 20_000_000 (setFixedFee 1_000_000)),
                testCase "Successful 3-utxo balancing with ridiculously high fee" $
                  testSucceedsFrom'
                    def
                    ( \(TxSkel {..}, fee, refs, wal) _ ->
                        testBool $
                          fee == 40_000_000 && length txSkelIns == 3 && length refs == 3 && wal == alice
                    )
                    initialDistributionBalancing
                    (alicePaysToBobTemplate 20_000_000 (setFixedFee 40_000_000)),
                testCase "Unsuccessful 1-utxo balancing with too little fee" $
                  testFailsFrom
                    def
                    ( \case
                        (MCEValidationError Ledger.Phase1 _) -> testBool True
                        _ -> testBool False
                    )
                    initialDistributionBalancing
                    (alicePaysToBobTemplate 20_000_000 (setFixedFee 150_000))
              ]
    ]
