module Cooked.BalancingSpec where

import Control.Monad
import Cooked
import Data.Default
import Data.Maybe
import Data.Set
import Data.Set qualified as Set
import Data.Text (isInfixOf)
import Debug.Trace
import Ledger.Index qualified as Ledger
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import Test.Tasty
import Test.Tasty.HUnit

alice, bob :: Wallet
(alice, bob) = (wallet 1, wallet 2)

apple, orange, banana :: Integer -> Api.Value
apple = permanentValue "apple"
orange = permanentValue "orange"
banana = permanentValue "banana"

initialDistributionBalancing :: InitialDistribution
initialDistributionBalancing =
  InitialDistribution
    [ paysPK alice (ada 2 <> apple 3),
      paysPK alice (ada 25),
      paysPK alice (ada 40 <> orange 6),
      paysPK alice (ada 8),
      paysPK alice (ada 30),
      paysPK alice (ada 12 <> banana 3) `withDatum` (10 :: Integer),
      paysPK alice (ada 5 <> banana 7) `withReferenceScript` (alwaysTrueValidator @MockContract),
      paysPK alice (ada 100 <> banana 2) `withDatumHash` ()
    ]

template1 :: (MonadBlockChain m) => Integer -> (TxOpts -> TxOpts) -> m ((TxSkel, Integer, Set Api.TxOutRef, Wallet), Integer)
template1 val f = do
  let txSkel =
        txSkelTemplate
          { txSkelOuts = [paysPK bob (lovelace val)],
            txSkelSigners = [alice],
            txSkelOpts = f def
          }
  res <- balanceTxSkel txSkel
  void $ validateTxSkel txSkel
  utxos <- runUtxoSearch $ utxosAtSearch alice `filterWithPred` \o -> isJust (Api.txOutReferenceScript o) || (Api.txOutDatum o /= Api.NoOutputDatum)
  return (res, toInteger $ length utxos)

template2 :: (MonadBlockChain m) => Integer -> (TxOpts -> TxOpts) -> m ((TxSkel, Integer, Set Api.TxOutRef, Wallet), Integer)
template2 val f = do
  let txSkel =
        txSkelTemplate
          { txSkelOuts = [paysPK bob (lovelace val), paysPK alice (lovelace val)],
            txSkelSigners = [alice],
            txSkelOpts = f def
          }
  res <- balanceTxSkel txSkel
  void $ validateTxSkel txSkel
  utxos <- runUtxoSearch $ utxosAtSearch alice `filterWithPred` \o -> isJust (Api.txOutReferenceScript o) || (Api.txOutDatum o /= Api.NoOutputDatum)
  return (res, toInteger $ length utxos)

template3 :: (MonadBlockChain m) => Integer -> Integer -> Integer -> (TxOpts -> TxOpts) -> m ((TxSkel, Integer, Set Api.TxOutRef, Wallet), Integer)
template3 apples oranges bananas f = do
  let txSkel =
        txSkelTemplate
          { txSkelOuts = [paysPK bob (apple apples <> orange oranges <> banana bananas)],
            txSkelSigners = [alice],
            txSkelOpts = f (def {txOptEnsureMinAda = True})
          }
  res <- balanceTxSkel txSkel
  void $ validateTxSkel txSkel
  utxos <- runUtxoSearch $ utxosAtSearch alice `filterWithPred` \o -> isJust (Api.txOutReferenceScript o) || (Api.txOutDatum o /= Api.NoOutputDatum)
  return (res, toInteger $ length utxos)

template4 :: (MonadBlockChain m) => Integer -> Integer -> Integer -> (TxOpts -> TxOpts) -> m ((TxSkel, Integer, Set Api.TxOutRef, Wallet), Integer)
template4 apples oranges bananas f = do
  aliceUtxos <- runUtxoSearch $ utxosAtSearch alice
  let txSkel =
        txSkelTemplate
          { txSkelOuts = [paysPK bob (apple apples <> orange oranges <> banana bananas)],
            txSkelSigners = [alice],
            txSkelOpts =
              f
                ( def
                    { txOptEnsureMinAda = True,
                      txOptBalancingUtxos = BalancingUtxosWith (Set.fromList $ fst <$> aliceUtxos)
                    }
                )
          }
  res <- balanceTxSkel txSkel
  void $ validateTxSkel txSkel
  utxos <- runUtxoSearch $ utxosAtSearch alice `filterWithPred` \o -> isJust (Api.txOutReferenceScript o)
  return (res, toInteger $ length utxos)

tests :: TestTree
tests =
  testGroup
    "Balancing"
    [ testGroup
        "Fee fixed"
        $ let setFixedFee fee txOpts = txOpts {txOptFeePolicy = ManualFee fee}
              setDontAdjustOutput txOpts = txOpts {txOptBalanceOutputPolicy = DontAdjustExistingOutput}
           in [ testCase "Successful 1-utxo balancing" $
                  testSucceedsFrom'
                    def
                    ( \((TxSkel {..}, fee, refs, wal), nb) _ ->
                        testBool $
                          fee == 1_000_000 && length txSkelIns == 1 && length txSkelOuts == 2 && length refs == 1 && wal == alice && nb == 3
                    )
                    initialDistributionBalancing
                    (template1 20_000_000 (setFixedFee 1_000_000)),
                testCase "Successful 3-utxo balancing with ridiculously high fee" $
                  testSucceedsFrom'
                    def
                    ( \((TxSkel {..}, fee, refs, wal), nb) _ ->
                        testBool $
                          fee == 40_000_000 && length txSkelIns == 3 && length txSkelOuts == 2 && length refs == 3 && wal == alice && nb == 3
                    )
                    initialDistributionBalancing
                    (template1 20_000_000 (setFixedFee 40_000_000)),
                testCase "Unsuccessful 1-utxo balancing with too little fee" $
                  testFailsFrom
                    def
                    ( \case
                        (MCEValidationError Ledger.Phase1 (Ledger.CardanoLedgerValidationError text)) -> testBool $ isInfixOf "FeeTooSmallUTxO" text
                        _ -> testBool False
                    )
                    initialDistributionBalancing
                    (template1 20_000_000 (setFixedFee 150_000)),
                testCase "Unsuccessful balancing with too much fee" $
                  testFailsFrom
                    def
                    ( \case
                        (MCEUnbalanceable wal val _) -> testBool $ wal == alice && val == ada 106
                        _ -> testBool False
                    )
                    initialDistributionBalancing
                    (template1 100_000_000 (setFixedFee 6_000_000)),
                testCase "Unsuccessful balancing with too high collaterals" $
                  testFailsFrom
                    def
                    ( \case
                        (MCENoSuitableCollateral fee percentage val) -> testBool $ fee == 80_000_000 && val == lovelace ((fee * percentage) `div` 100)
                        _ -> testBool False
                    )
                    initialDistributionBalancing
                    (template1 6_000_000 (setFixedFee 80_000_000)),
                testCase "Successful merging of outputs to the balancing wallet" $
                  testSucceedsFrom'
                    def
                    ( \((TxSkel {..}, fee, refs, wal), nb) _ ->
                        testBool $
                          fee == 2_000_000 && length txSkelIns == 1 && length txSkelOuts == 2 && length refs == 1 && wal == alice && nb == 3
                    )
                    initialDistributionBalancing
                    (template2 6_000_000 (setFixedFee 2_000_000)),
                testCase "Successful creation of a new output to the balancing wallet" $
                  testSucceedsFrom'
                    def
                    ( \((TxSkel {..}, fee, refs, wal), nb) _ ->
                        testBool $
                          fee == 2_000_000 && length txSkelIns == 1 && length txSkelOuts == 3 && length refs == 1 && wal == alice && nb == 3
                    )
                    initialDistributionBalancing
                    (template2 6_000_000 (setFixedFee 2_000_000 . setDontAdjustOutput)),
                testCase "Successful balancing with oranges" $
                  testSucceedsFrom'
                    def
                    ( \((TxSkel {..}, fee, refs, wal), nb) _ ->
                        testBool $
                          fee == 1_000_000 && length txSkelIns == 1 && length txSkelOuts == 2 && length refs == 1 && wal == alice && nb == 3
                    )
                    initialDistributionBalancing
                    (template3 0 5 0 (setFixedFee 1_000_000)),
                testCase "Successful balancing with oranges and apples" $
                  testSucceedsFrom'
                    def
                    ( \((TxSkel {..}, fee, refs, wal), nb) _ ->
                        testBool $
                          fee == 1_000_000 && length txSkelIns == 2 && length txSkelOuts == 2 && length refs == 1 && wal == alice && nb == 3
                    )
                    initialDistributionBalancing
                    (template3 2 5 0 (setFixedFee 1_000_000)),
                testCase "Unsuccessful balancing with oranges, apples and bananas" $
                  testFailsFrom
                    def
                    ( \case
                        (MCEUnbalanceable wal val _) -> testBool $ wal == alice && val `Api.geq` (apple 2 <> orange 5 <> banana 4)
                        _ -> testBool False
                    )
                    initialDistributionBalancing
                    (template3 2 5 4 (setFixedFee 1_000_000)),
                testCase "Successful balancing with oranges, apples and bananas but losing a reference script" $
                  testSucceedsFrom'
                    def
                    ( \((TxSkel {..}, fee, refs, wal), nb) _ ->
                        testBool $
                          fee == 1_000_000 && length txSkelIns == 3 && length txSkelOuts == 2 && length refs == 1 && wal == alice && nb == 0
                    )
                    initialDistributionBalancing
                    (template4 2 5 4 (setFixedFee 1_000_000))
              ]
    ]
