module Cooked.BalancingSpec where

import Control.Monad
import Cooked
import Data.Default
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe
import Data.Set
import Data.Set qualified as Set
import Data.Text (isInfixOf)
import Ledger.Index qualified as Ledger
import ListT
import Optics.Core
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

testingBalancingTemplate ::
  (MonadBlockChain m) =>
  -- Value to pay to bob
  Api.Value ->
  -- Value to pay back to alice
  Api.Value ->
  -- Search for utxos to be spent
  UtxoSearch m a ->
  -- Search for utxos to be used for balancing
  UtxoSearch m b ->
  -- Search for utxos to be returned through their size
  UtxoSearch m c ->
  -- Option modifications
  (TxOpts -> TxOpts) ->
  m (TxSkel, Integer, Set Api.TxOutRef, Wallet, Integer)
testingBalancingTemplate toBobValue toAliceValue spendSearch balanceSearch returnSearch optionsMod = do
  ((fst <$>) -> toSpendUtxos) <- runUtxoSearch spendSearch
  ((fst <$>) -> toBalanceUtxos) <- runUtxoSearch balanceSearch
  let txSkel =
        txSkelTemplate
          { txSkelOuts = List.filter ((/= mempty) . (^. txSkelOutValueL)) [paysPK bob toBobValue, paysPK alice toAliceValue],
            txSkelIns = Map.fromList $ (,TxSkelNoRedeemerForPK) <$> toSpendUtxos,
            txSkelOpts =
              optionsMod
                def
                  { txOptBalancingUtxos =
                      if List.null toBalanceUtxos
                        then BalancingUtxosAutomatic
                        else BalancingUtxosWith $ Set.fromList toBalanceUtxos
                  },
            txSkelSigners = [alice]
          }
  (skel', fee, cols, wal) <- balanceTxSkel txSkel
  void $ validateTxSkel txSkel
  (toInteger . length -> lengthRes) <- runUtxoSearch returnSearch
  return (skel', fee, cols, wal, lengthRes)

aliceNonOnlyValueUtxos :: (MonadBlockChain m) => UtxoSearch m Api.TxOut
aliceNonOnlyValueUtxos = utxosAtSearch alice `filterWithPred` \o -> isJust (Api.txOutReferenceScript o) || (Api.txOutDatum o /= Api.NoOutputDatum)

aliceRefScriptUtxos :: (MonadBlockChain m) => UtxoSearch m Api.TxOut
aliceRefScriptUtxos = utxosAtSearch alice `filterWithPred` \o -> isJust (Api.txOutReferenceScript o)

emptySearch :: (MonadBlockChain m) => UtxoSearch m Api.TxOut
emptySearch = ListT.fromFoldable []

simplePaymentToBob :: (MonadBlockChain m) => Integer -> (TxOpts -> TxOpts) -> m (TxSkel, Integer, Set Api.TxOutRef, Wallet, Integer)
simplePaymentToBob val =
  testingBalancingTemplate
    (lovelace val)
    mempty
    emptySearch
    emptySearch
    aliceNonOnlyValueUtxos

bothPaymentsToBobAndAlice :: (MonadBlockChain m) => Integer -> (TxOpts -> TxOpts) -> m (TxSkel, Integer, Set Api.TxOutRef, Wallet, Integer)
bothPaymentsToBobAndAlice val =
  testingBalancingTemplate
    (lovelace val)
    (lovelace val)
    emptySearch
    emptySearch
    aliceNonOnlyValueUtxos

fruitsPaymentToBob :: (MonadBlockChain m) => Integer -> Integer -> Integer -> (TxOpts -> TxOpts) -> m (TxSkel, Integer, Set Api.TxOutRef, Wallet, Integer)
fruitsPaymentToBob apples oranges bananas f =
  testingBalancingTemplate
    (apple apples <> orange oranges <> banana bananas)
    mempty
    emptySearch
    emptySearch
    aliceNonOnlyValueUtxos
    (f . (\txOpts -> txOpts {txOptEnsureMinAda = True}))

fruitsPaymentToBobWithBalancingUtxos :: (MonadBlockChain m) => Integer -> Integer -> Integer -> (TxOpts -> TxOpts) -> m (TxSkel, Integer, Set Api.TxOutRef, Wallet, Integer)
fruitsPaymentToBobWithBalancingUtxos apples oranges bananas f =
  testingBalancingTemplate
    (apple apples <> orange oranges <> banana bananas)
    mempty
    emptySearch
    (utxosAtSearch alice)
    aliceRefScriptUtxos
    (f . (\txOpts -> txOpts {txOptEnsureMinAda = True}))

spendingAliceUtxos :: (MonadBlockChain m) => (TxOpts -> TxOpts) -> m (TxSkel, Integer, Set Api.TxOutRef, Wallet, Integer)
spendingAliceUtxos =
  testingBalancingTemplate
    mempty
    mempty
    (onlyValueOutputsAtSearch alice)
    emptySearch
    aliceNonOnlyValueUtxos

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
                    ( \(TxSkel {..}, fee, refs, wal, nb) _ ->
                        testBool $
                          fee == 1_000_000 && length txSkelIns == 1 && length txSkelOuts == 2 && length refs == 1 && wal == alice && nb == 3
                    )
                    initialDistributionBalancing
                    (simplePaymentToBob 20_000_000 (setFixedFee 1_000_000)),
                testCase "Successful 3-utxo balancing with ridiculously high fee" $
                  testSucceedsFrom'
                    def
                    ( \(TxSkel {..}, fee, refs, wal, nb) _ ->
                        testBool $
                          fee == 40_000_000 && length txSkelIns == 3 && length txSkelOuts == 2 && length refs == 3 && wal == alice && nb == 3
                    )
                    initialDistributionBalancing
                    (simplePaymentToBob 20_000_000 (setFixedFee 40_000_000)),
                testCase "Unsuccessful 1-utxo balancing with too little fee" $
                  testFailsFrom
                    def
                    ( \case
                        (MCEValidationError Ledger.Phase1 (Ledger.CardanoLedgerValidationError text)) -> testBool $ isInfixOf "FeeTooSmallUTxO" text
                        _ -> testBool False
                    )
                    initialDistributionBalancing
                    (simplePaymentToBob 20_000_000 (setFixedFee 150_000)),
                testCase "Unsuccessful balancing with too much fee" $
                  testFailsFrom
                    def
                    ( \case
                        (MCEUnbalanceable wal val _) -> testBool $ wal == alice && val == ada 1
                        _ -> testBool False
                    )
                    initialDistributionBalancing
                    (simplePaymentToBob 100_000_000 (setFixedFee 6_000_000)),
                testCase "Unsuccessful balancing with too high collaterals" $
                  testFailsFrom
                    def
                    ( \case
                        (MCENoSuitableCollateral fee percentage val) -> testBool $ fee == 80_000_000 && val == lovelace ((fee * percentage) `div` 100)
                        _ -> testBool False
                    )
                    initialDistributionBalancing
                    (simplePaymentToBob 6_000_000 (setFixedFee 80_000_000)),
                testCase "Successful merging of outputs to the balancing wallet" $
                  testSucceedsFrom'
                    def
                    ( \(TxSkel {..}, fee, refs, wal, nb) _ ->
                        testBool $
                          fee == 2_000_000 && length txSkelIns == 1 && length txSkelOuts == 2 && length refs == 1 && wal == alice && nb == 3
                    )
                    initialDistributionBalancing
                    (bothPaymentsToBobAndAlice 6_000_000 (setFixedFee 2_000_000)),
                testCase "Successful creation of a new output to the balancing wallet" $
                  testSucceedsFrom'
                    def
                    ( \(TxSkel {..}, fee, refs, wal, nb) _ ->
                        testBool $
                          fee == 2_000_000 && length txSkelIns == 1 && length txSkelOuts == 3 && length refs == 1 && wal == alice && nb == 3
                    )
                    initialDistributionBalancing
                    (bothPaymentsToBobAndAlice 6_000_000 (setFixedFee 2_000_000 . setDontAdjustOutput)),
                testCase "Successful balancing with non-ada asset" $
                  testSucceedsFrom'
                    def
                    ( \(TxSkel {..}, fee, refs, wal, nb) _ ->
                        testBool $
                          fee == 1_000_000 && length txSkelIns == 1 && length txSkelOuts == 2 && length refs == 1 && wal == alice && nb == 3
                    )
                    initialDistributionBalancing
                    (fruitsPaymentToBob 0 5 0 (setFixedFee 1_000_000)),
                testCase "Successful balancing with multiple assets" $
                  testSucceedsFrom'
                    def
                    ( \(TxSkel {..}, fee, refs, wal, nb) _ ->
                        testBool $
                          fee == 1_000_000 && length txSkelIns == 2 && length txSkelOuts == 2 && length refs == 1 && wal == alice && nb == 3
                    )
                    initialDistributionBalancing
                    (fruitsPaymentToBob 2 5 0 (setFixedFee 1_000_000)),
                testCase "Unsuccessful balancing with multiple assets in protected utxos" $
                  testFailsFrom
                    def
                    ( \case
                        (MCEUnbalanceable wal val _) -> testBool $ wal == alice && val == banana 4
                        _ -> testBool False
                    )
                    initialDistributionBalancing
                    (fruitsPaymentToBob 2 5 4 (setFixedFee 1_000_000)),
                testCase "Successful balancing with multiple assets and explicit utxo set" $
                  testSucceedsFrom'
                    def
                    ( \(TxSkel {..}, fee, refs, wal, nb) _ ->
                        testBool $
                          fee == 1_000_000 && length txSkelIns == 3 && length txSkelOuts == 2 && length refs == 1 && wal == alice && nb == 0
                    )
                    initialDistributionBalancing
                    (fruitsPaymentToBobWithBalancingUtxos 2 5 4 (setFixedFee 1_000_000)),
                testCase "Successful balancing with excess consumption" $
                  testSucceedsFrom'
                    def
                    ( \(TxSkel {..}, fee, refs, wal, nb) _ ->
                        testBool $
                          fee == 1_000_000 && length txSkelIns == 5 && length txSkelOuts == 1 && length refs == 1 && wal == alice && nb == 3
                    )
                    initialDistributionBalancing
                    (spendingAliceUtxos (setFixedFee 1_000_000))
              ]
    ]
