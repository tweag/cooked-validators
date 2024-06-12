module Cooked.BalancingSpec where

import Cardano.Api qualified as Cardano
import Control.Monad
import Cooked
import Cooked.MockChain.GenerateTx
import Cooked.MockChain.Staged
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

type TestBalancingOutcome = (TxSkel, TxSkel, Integer, Set Api.TxOutRef, Wallet, [Api.TxOutRef])

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
  -- Option modifications
  (TxOpts -> TxOpts) ->
  m TestBalancingOutcome
testingBalancingTemplate toBobValue toAliceValue spendSearch balanceSearch optionsMod = do
  ((fst <$>) -> toSpendUtxos) <- runUtxoSearch spendSearch
  ((fst <$>) -> toBalanceUtxos) <- runUtxoSearch balanceSearch
  let skel =
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
  (skel', fee, cols, wal) <- balanceTxSkel skel
  void $ validateTxSkel skel
  nonOnlyValueUtxos <- runUtxoSearch $ utxosAtSearch alice `filterWithPred` \o -> isJust (Api.txOutReferenceScript o) || (Api.txOutDatum o /= Api.NoOutputDatum)
  return (skel, skel', fee, cols, wal, fst <$> nonOnlyValueUtxos)

aliceNonOnlyValueUtxos :: (MonadBlockChain m) => UtxoSearch m Api.TxOut
aliceNonOnlyValueUtxos = utxosAtSearch alice `filterWithPred` \o -> isJust (Api.txOutReferenceScript o) || (Api.txOutDatum o /= Api.NoOutputDatum)

aliceEightAdaUtxos :: (MonadBlockChain m) => UtxoSearch m Api.TxOut
aliceEightAdaUtxos = utxosAtSearch alice `filterWithPred` ((== ada 8) . Api.txOutValue)

aliceRefScriptUtxos :: (MonadBlockChain m) => UtxoSearch m Api.TxOut
aliceRefScriptUtxos = utxosAtSearch alice `filterWithPred` \o -> isJust (Api.txOutReferenceScript o)

emptySearch :: (MonadBlockChain m) => UtxoSearch m Api.TxOut
emptySearch = ListT.fromFoldable []

simplePaymentToBob :: (MonadBlockChain m) => Integer -> Integer -> Integer -> Integer -> (TxOpts -> TxOpts) -> m TestBalancingOutcome
simplePaymentToBob lv apples oranges bananas =
  testingBalancingTemplate
    (lovelace lv <> apple apples <> orange oranges <> banana bananas)
    mempty
    emptySearch
    emptySearch

bothPaymentsToBobAndAlice :: (MonadBlockChain m) => Integer -> (TxOpts -> TxOpts) -> m TestBalancingOutcome
bothPaymentsToBobAndAlice val =
  testingBalancingTemplate
    (lovelace val)
    (lovelace val)
    emptySearch
    emptySearch

type ResProp prop = TestBalancingOutcome -> prop

hasFee :: (IsProp prop) => Integer -> ResProp prop
hasFee fee (_, _, fee', _, _, _) = testBool $ fee == fee'

additionalOutsNb :: (IsProp prop) => Int -> ResProp prop
additionalOutsNb ao (txSkel1, txSkel2, _, _, _, _) = testBool $ length (txSkelOuts txSkel2) - length (txSkelOuts txSkel1) == ao

insNb :: (IsProp prop) => Int -> ResProp prop
insNb is (_, TxSkel {..}, _, _, _, _) = testBool $ length txSkelIns == is

colInsNb :: (IsProp prop) => Int -> ResProp prop
colInsNb cis (_, _, _, refs, _, _) = testBool $ cis == length refs

balancedBy :: (IsProp prop) => Wallet -> ResProp prop
balancedBy wal (_, _, _, _, wal', _) = testBool $ wal == wal'

retOutsNb :: (IsProp prop) => Int -> ResProp prop
retOutsNb ros (_, _, _, _, _, refs) = testBool $ ros == length refs

testBalancingSucceedsWith :: String -> [ResProp Assertion] -> StagedMockChain TestBalancingOutcome -> TestTree
testBalancingSucceedsWith msg props smc = testCase msg $ testSucceedsFrom' def (\res _ -> testConjoin $ ($ res) <$> props) initialDistributionBalancing smc

failsAtBalancingWith :: (IsProp prop) => Api.Value -> Wallet -> MockChainError -> prop
failsAtBalancingWith val' wal' (MCEUnbalanceable wal val _) = testBool $ val' == val && wal' == wal
failsAtBalancingWith _ _ _ = testBool False

failsWithTooLittleFee :: (IsProp prop) => MockChainError -> prop
failsWithTooLittleFee (MCEValidationError Ledger.Phase1 (Ledger.CardanoLedgerValidationError text)) = testBool $ isInfixOf "FeeTooSmallUTxO" text
failsWithTooLittleFee _ = testBool False

failsWithValueNotConserved :: (IsProp prop) => MockChainError -> prop
failsWithValueNotConserved (MCEValidationError Ledger.Phase1 (Ledger.CardanoLedgerValidationError text)) = testBool $ isInfixOf "ValueNotConserved" text
failsWithValueNotConserved _ = testBool False

failsWithEmptyTxIns :: (IsProp prop) => MockChainError -> prop
failsWithEmptyTxIns (MCEGenerationError (TxBodyError _ Cardano.TxBodyEmptyTxIns)) = testBool True
failsWithEmptyTxIns _ = testBool False

failsAtCollateralsWith :: (IsProp prop) => Integer -> MockChainError -> prop
failsAtCollateralsWith fee' (MCENoSuitableCollateral fee percentage val) = testBool $ fee == fee' && val == lovelace ((fee * percentage) `div` 100)
failsAtCollateralsWith _ _ = testBool False

testBalancingFailsWith :: (Show a) => String -> (MockChainError -> Assertion) -> StagedMockChain a -> TestTree
testBalancingFailsWith msg p smc = testCase msg $ testFailsFrom def p initialDistributionBalancing smc

tests :: TestTree
tests =
  let setFixedFee fee txOpts = txOpts {txOptFeePolicy = ManualFee fee}
      setDontAdjustOutput txOpts = txOpts {txOptBalanceOutputPolicy = DontAdjustExistingOutput}
      setEnsureMinAda txOpts = txOpts {txOptEnsureMinAda = True}
      setDontBalance txOpts = txOpts {txOptBalancingPolicy = DoNotBalance}
      setCollateralWallet wallet' txOpts = txOpts {txOptCollateralUtxos = CollateralUtxosFromWallet wallet'}
   in testGroup
        "Balancing"
        [ testGroup
            "Manual balancing with manual fee"
            [ testBalancingFailsWith
                "Balancing does not occur when not requested, fails with empty inputs"
                failsWithEmptyTxIns
                (simplePaymentToBob 20_000_000 0 0 0 (setCollateralWallet alice . setDontBalance . setFixedFee 1_000_000)),
              testBalancingFailsWith
                "Balancing does not occur when not requested, fails with too small inputs"
                failsWithValueNotConserved
                (testingBalancingTemplate (ada 50) mempty aliceEightAdaUtxos emptySearch (setCollateralWallet alice . setDontBalance . setFixedFee 1_000_000)),
              testBalancingSucceedsWith
                "It is still possible to balance the transaction by hand"
                [hasFee 1_000_000, insNb 1, additionalOutsNb 0, colInsNb 1, balancedBy alice, retOutsNb 3]
                (testingBalancingTemplate (ada 7) mempty aliceEightAdaUtxos emptySearch (setCollateralWallet alice . setDontBalance . setFixedFee 1_000_000))
            ],
          testGroup
            "Auto balancing with manual fee"
            [ testBalancingSucceedsWith
                "We can use a single utxo for balancing purpose"
                [hasFee 1_000_000, insNb 1, additionalOutsNb 1, colInsNb 1, balancedBy alice, retOutsNb 3]
                (simplePaymentToBob 20_000_000 0 0 0 (setFixedFee 1_000_000)),
              testBalancingSucceedsWith
                "We can use several utxos for balancing with ridiculously high fee"
                [hasFee 40_000_000, insNb 3, additionalOutsNb 1, colInsNb 3, balancedBy alice, retOutsNb 3]
                (simplePaymentToBob 20_000_000 0 0 0 (setFixedFee 40_000_000)),
              testBalancingFailsWith
                "We cannot balance with too little fee"
                failsWithTooLittleFee
                (simplePaymentToBob 20_000_000 0 0 0 (setFixedFee 150_000)),
              testBalancingFailsWith
                "Fee are rightfully included in the balancing process, which fails when they are too high"
                (failsAtBalancingWith (ada 1) alice)
                (simplePaymentToBob 100_000_000 0 0 0 (setFixedFee 6_000_000)),
              testBalancingFailsWith
                "Collaterals are rightfully included in the balancing process, which fails when they are too high"
                (failsAtCollateralsWith 80_000_000)
                (simplePaymentToBob 6_000_000 0 0 0 (setFixedFee 80_000_000)),
              testBalancingSucceedsWith
                "Exactly the right amount leads to no output change"
                [hasFee 2_000_000, insNb 3, additionalOutsNb 0, colInsNb 1, balancedBy alice, retOutsNb 3]
                (simplePaymentToBob 65_000_000 3 6 0 (setFixedFee 2_000_000)),
              testBalancingSucceedsWith
                "It still leads to no output change when requesting a new output"
                [hasFee 2_000_000, insNb 3, additionalOutsNb 0, colInsNb 1, balancedBy alice, retOutsNb 3]
                (simplePaymentToBob 65_000_000 3 6 0 (setDontAdjustOutput . setFixedFee 2_000_000)),
              testBalancingSucceedsWith
                "1 lovelace more than the exact right amount leads to an additional output"
                [hasFee 2_000_000, insNb 3, additionalOutsNb 1, colInsNb 1, balancedBy alice, retOutsNb 3]
                (simplePaymentToBob 65_000_001 3 6 0 (setFixedFee 2_000_000)),
              testBalancingSucceedsWith
                "1 lovelace less than the exact right amount leads to an additional output to account for minAda"
                [hasFee 2_000_000, insNb 3, additionalOutsNb 1, colInsNb 1, balancedBy alice, retOutsNb 3]
                (simplePaymentToBob 65_000_001 3 6 0 (setFixedFee 2_000_000)),
              testBalancingSucceedsWith
                "We can merge assets to an existing outputs at the balancing wallet address"
                [hasFee 2_000_000, insNb 1, additionalOutsNb 0, colInsNb 1, balancedBy alice, retOutsNb 3]
                (bothPaymentsToBobAndAlice 6_000_000 (setFixedFee 2_000_000)),
              testBalancingSucceedsWith
                "We can create a new output at the balancing wallet address even if one already exists"
                [hasFee 2_000_000, insNb 1, additionalOutsNb 1, colInsNb 1, balancedBy alice, retOutsNb 3]
                (bothPaymentsToBobAndAlice 6_000_000 (setFixedFee 2_000_000 . setDontAdjustOutput)),
              testBalancingSucceedsWith
                "We can balance transactions with non-ada assets"
                [hasFee 2_000_000, insNb 1, additionalOutsNb 1, colInsNb 1, balancedBy alice, retOutsNb 3]
                (simplePaymentToBob 0 0 5 0 (setFixedFee 2_000_000 . setEnsureMinAda)),
              testBalancingSucceedsWith
                "Successful balancing with multiple assets"
                [hasFee 1_000_000, insNb 2, additionalOutsNb 1, colInsNb 1, balancedBy alice, retOutsNb 3]
                (simplePaymentToBob 0 2 5 0 (setEnsureMinAda . setFixedFee 1_000_000)),
              testBalancingFailsWith
                "Unsuccessful balancing with multiple assets in non value only utxos"
                (failsAtBalancingWith (banana 4) alice)
                (simplePaymentToBob 0 2 5 4 (setEnsureMinAda . setFixedFee 1_000_000)),
              testBalancingSucceedsWith
                "Successful balancing with multiple assets and explicit utxo set, reference script is lost"
                [hasFee 1_000_000, insNb 3, additionalOutsNb 1, colInsNb 1, balancedBy alice, retOutsNb 2]
                (testingBalancingTemplate (apple 2 <> orange 5 <> banana 4) mempty emptySearch (utxosAtSearch alice) (setEnsureMinAda . setFixedFee 1_000_000)),
              testBalancingSucceedsWith
                "Successful balancing with excess initial consumption"
                [hasFee 1_000_000, insNb 5, additionalOutsNb 1, colInsNb 1, balancedBy alice, retOutsNb 3]
                (testingBalancingTemplate mempty mempty (onlyValueOutputsAtSearch alice) emptySearch (setFixedFee 1_000_000))
            ]
        ]
