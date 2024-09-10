module Cooked.BalancingSpec where

import Cardano.Api qualified as Cardano
import Control.Monad
import Cooked
import Cooked.MockChain.GenerateTx
import Cooked.MockChain.Staged
import Data.Default
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set
import Data.Set qualified as Set
import Data.Text (isInfixOf)
import Ledger.Index qualified as Ledger
import ListT
import Optics.Core
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Value qualified as Script
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
    [ paysScript (alwaysTrueValidator @MockContract) () (Script.ada 42),
      paysPK alice (Script.ada 2 <> apple 3),
      paysPK alice (Script.ada 25),
      paysPK alice (Script.ada 40 <> orange 6),
      paysPK alice (Script.ada 8),
      paysPK alice (Script.ada 30),
      paysPK alice (Script.lovelace 1280229 <> banana 3) `withDatum` (10 :: Integer),
      paysPK alice (Script.ada 1 <> banana 7) `withReferenceScript` (alwaysTrueValidator @MockContract),
      paysPK alice (Script.ada 105 <> banana 2) `withUnresolvedDatumHash` ()
    ]

type TestBalancingOutcome = (TxSkel, TxSkel, Integer, Maybe (Set Api.TxOutRef, Wallet), [Api.TxOutRef])

spendsScriptUtxo :: (MonadBlockChain m) => Bool -> m (Map Api.TxOutRef TxSkelRedeemer)
spendsScriptUtxo False = return Map.empty
spendsScriptUtxo True = do
  (scriptOutRef, _) : _ <- runUtxoSearch $ utxosAtSearch $ alwaysTrueValidator @MockContract
  return $ Map.singleton scriptOutRef txSkelEmptyRedeemer

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
  -- Search for utxos to be used for collaterals
  UtxoSearch m c ->
  -- Whether to consum the script utxo
  Bool ->
  -- Option modifications
  (TxOpts -> TxOpts) ->
  m TestBalancingOutcome
testingBalancingTemplate toBobValue toAliceValue spendSearch balanceSearch collateralSearch consumeScriptUtxo optionsMod = do
  ((fst <$>) -> toSpendUtxos) <- runUtxoSearch spendSearch
  ((fst <$>) -> toBalanceUtxos) <- runUtxoSearch balanceSearch
  ((fst <$>) -> toCollateralUtxos) <- runUtxoSearch collateralSearch
  additionalSpend <- spendsScriptUtxo consumeScriptUtxo
  let skel =
        txSkelTemplate
          { txSkelOuts = List.filter ((/= mempty) . (^. txSkelOutValueL)) [paysPK bob toBobValue, paysPK alice toAliceValue],
            txSkelIns = additionalSpend <> Map.fromList ((,txSkelEmptyRedeemer) <$> toSpendUtxos),
            txSkelOpts =
              optionsMod
                def
                  { txOptBalancingUtxos =
                      if List.null toBalanceUtxos
                        then BalancingUtxosFromBalancingWallet
                        else BalancingUtxosFromSet $ Set.fromList toBalanceUtxos,
                    txOptCollateralUtxos =
                      if List.null toCollateralUtxos
                        then CollateralUtxosFromBalancingWallet
                        else CollateralUtxosFromSet (Set.fromList toCollateralUtxos) alice
                  },
            txSkelSigners = [alice]
          }
  (skel', fee, mCols) <- balanceTxSkel skel
  void $ validateTxSkel skel
  nonOnlyValueUtxos <- runUtxoSearch $ utxosAtSearch alice `filterWithPred` \o -> isJust (Api.txOutReferenceScript o) || (Api.txOutDatum o /= Api.NoOutputDatum)
  return (skel, skel', fee, mCols, fst <$> nonOnlyValueUtxos)

aliceNonOnlyValueUtxos :: (MonadBlockChain m) => UtxoSearch m Api.TxOut
aliceNonOnlyValueUtxos = utxosAtSearch alice `filterWithPred` \o -> isJust (Api.txOutReferenceScript o) || (Api.txOutDatum o /= Api.NoOutputDatum)

aliceNAdaUtxos :: (MonadBlockChain m) => Integer -> UtxoSearch m Api.TxOut
aliceNAdaUtxos n = utxosAtSearch alice `filterWithPred` ((== Script.Lovelace (n * 1_000_000)) . Script.fromValue . Api.txOutValue)

aliceRefScriptUtxos :: (MonadBlockChain m) => UtxoSearch m Api.TxOut
aliceRefScriptUtxos = utxosAtSearch alice `filterWithPred` \o -> isJust (Api.txOutReferenceScript o)

emptySearch :: (MonadBlockChain m) => UtxoSearch m Api.TxOut
emptySearch = ListT.fromFoldable []

simplePaymentToBob :: (MonadBlockChain m) => Integer -> Integer -> Integer -> Integer -> Bool -> (TxOpts -> TxOpts) -> m TestBalancingOutcome
simplePaymentToBob lv apples oranges bananas =
  testingBalancingTemplate
    (Script.lovelace lv <> apple apples <> orange oranges <> banana bananas)
    mempty
    emptySearch
    emptySearch
    emptySearch

bothPaymentsToBobAndAlice :: (MonadBlockChain m) => Integer -> Bool -> (TxOpts -> TxOpts) -> m TestBalancingOutcome
bothPaymentsToBobAndAlice val =
  testingBalancingTemplate
    (Script.lovelace val)
    (Script.lovelace val)
    emptySearch
    emptySearch
    emptySearch

noBalanceMaxFee :: (MonadBlockChain m) => m ()
noBalanceMaxFee = do
  maxFee <- snd <$> getMinAndMaxFee
  ((txOutRef, _) : _) <- runUtxoSearch $ utxosAtSearch alice `filterWithPred` ((== Script.ada 30) . Api.txOutValue)
  void $
    validateTxSkel $
      txSkelTemplate
        { txSkelOuts = [paysPK bob (Script.lovelace (30_000_000 - maxFee))],
          txSkelIns = Map.singleton txOutRef txSkelEmptyRedeemer,
          txSkelOpts =
            def
              { txOptBalancingPolicy = DoNotBalance,
                txOptFeePolicy = AutoFeeComputation
              },
          txSkelSigners = [alice]
        }

balanceReduceFee :: (MonadBlockChain m) => m (Integer, Integer, Integer, Integer)
balanceReduceFee = do
  let skelAutoFee =
        txSkelTemplate
          { txSkelOuts = [paysPK bob (Script.ada 50)],
            txSkelSigners = [alice]
          }
  (skelBalanced, feeBalanced, mCols) <- balanceTxSkel skelAutoFee
  feeBalanced' <- estimateTxSkelFee skelBalanced feeBalanced mCols
  let skelManualFee =
        skelAutoFee
          { txSkelOpts =
              def
                { txOptFeePolicy = ManualFee (feeBalanced - 1)
                }
          }
  (skelBalancedManual, feeBalancedManual, mColsManual) <- balanceTxSkel skelManualFee
  feeBalancedManual' <- estimateTxSkelFee skelBalancedManual feeBalancedManual mColsManual
  return (feeBalanced, feeBalanced', feeBalancedManual, feeBalancedManual')

reachingMagic :: (MonadBlockChain m) => m ()
reachingMagic = do
  bananaOutRefs <- (fst <$>) <$> runUtxoSearch (utxosAtSearch alice `filterWithPred` \o -> banana 1 `Api.leq` Api.txOutValue o)
  void $
    validateTxSkel $
      txSkelTemplate
        { txSkelOuts = [paysPK bob (Script.ada 106 <> banana 12)],
          txSkelSigners = [alice],
          txSkelOpts =
            def
              { txOptBalancingUtxos = BalancingUtxosFromSet (Set.fromList bananaOutRefs)
              }
        }

type ResProp = TestBalancingOutcome -> Assertion

hasFee :: Integer -> ResProp
hasFee fee (_, _, fee', _, _) = testBool $ fee == fee'

additionalOutsNb :: Int -> ResProp
additionalOutsNb ao (txSkel1, txSkel2, _, _, _) = testBool $ length (txSkelOuts txSkel2) - length (txSkelOuts txSkel1) == ao

insNb :: Int -> ResProp
insNb is (_, TxSkel {..}, _, _, _) = testBool $ length txSkelIns == is

colInsNb :: Int -> ResProp
colInsNb cis (_, _, _, Nothing, _) = testBool $ cis == 0
colInsNb cis (_, _, _, Just (refs, _), _) = testBool $ cis == length refs

retOutsNb :: Int -> ResProp
retOutsNb ros (_, _, _, _, refs) = testBool $ ros == length refs

testBalancingSucceedsWith :: String -> [ResProp] -> StagedMockChain TestBalancingOutcome -> TestTree
testBalancingSucceedsWith msg props run =
  testCase msg $
    testProp $
      mustSucceedTest run
        `withInitDist` initialDistributionBalancing
        `withValuePred` \res -> testConjoin (($ res) <$> props)

failsAtBalancingWith :: Api.Value -> Wallet -> MockChainError -> Assertion
failsAtBalancingWith val' wal' (MCEUnbalanceable wal val _) = testBool $ val' == val && wal' == wal
failsAtBalancingWith _ _ _ = testBool False

failsAtBalancing :: MockChainError -> Assertion
failsAtBalancing MCEUnbalanceable {} = testBool True
failsAtBalancing _ = testBool False

failsWithTooLittleFee :: MockChainError -> Assertion
failsWithTooLittleFee (MCEValidationError Ledger.Phase1 (Ledger.CardanoLedgerValidationError text)) = testBool $ isInfixOf "FeeTooSmallUTxO" text
failsWithTooLittleFee _ = testBool False

failsWithValueNotConserved :: MockChainError -> Assertion
failsWithValueNotConserved (MCEValidationError Ledger.Phase1 (Ledger.CardanoLedgerValidationError text)) = testBool $ isInfixOf "ValueNotConserved" text
failsWithValueNotConserved _ = testBool False

failsWithEmptyTxIns :: MockChainError -> Assertion
failsWithEmptyTxIns (MCEGenerationError (TxBodyError _ Cardano.TxBodyEmptyTxIns)) = testBool True
failsWithEmptyTxIns _ = testBool False

failsAtCollateralsWith :: Integer -> MockChainError -> Assertion
failsAtCollateralsWith fee' (MCENoSuitableCollateral fee percentage val) = testBool $ fee == fee' && val == Script.lovelace (1 + (fee * percentage) `div` 100)
failsAtCollateralsWith _ _ = testBool False

failsAtCollaterals :: MockChainError -> Assertion
failsAtCollaterals MCENoSuitableCollateral {} = testBool True
failsAtCollaterals _ = testBool False

failsLackOfCollateralWallet :: MockChainError -> Assertion
failsLackOfCollateralWallet (FailWith msg) = testBool $ "Can't select collateral utxos from a balancing wallet because it does not exist." == msg
failsLackOfCollateralWallet _ = testBool False

testBalancingFailsWith :: (Show a) => String -> (MockChainError -> Assertion) -> StagedMockChain a -> TestTree
testBalancingFailsWith msg p smc =
  testCase msg $
    testProp $
      mustFailTest smc
        `withInitDist` initialDistributionBalancing
        `withErrorPred` p

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
                ( simplePaymentToBob
                    20_000_000
                    0
                    0
                    0
                    False
                    (setCollateralWallet alice . setDontBalance . setFixedFee 1_000_000)
                ),
              testBalancingFailsWith
                "Balancing does not occur when not requested, fails with too small inputs"
                failsWithValueNotConserved
                ( testingBalancingTemplate
                    (Script.ada 50)
                    mempty
                    (aliceNAdaUtxos 8)
                    emptySearch
                    emptySearch
                    False
                    (setCollateralWallet alice . setDontBalance . setFixedFee 1_000_000)
                ),
              testBalancingSucceedsWith
                "It is possible to balance the transaction by hand without collaterals"
                [hasFee 1_000_000, insNb 1, additionalOutsNb 0, colInsNb 0, retOutsNb 3]
                ( testingBalancingTemplate
                    (Script.ada 7)
                    mempty
                    (aliceNAdaUtxos 8)
                    emptySearch
                    emptySearch
                    False
                    (setCollateralWallet alice . setDontBalance . setFixedFee 1_000_000)
                ),
              testBalancingSucceedsWith
                "It is also possible to balance the transaction by hand with collaterals"
                [hasFee 1_000_000, insNb 2, additionalOutsNb 0, colInsNb 1, retOutsNb 3]
                ( testingBalancingTemplate
                    (Script.ada 49)
                    mempty
                    (aliceNAdaUtxos 8)
                    emptySearch
                    emptySearch
                    True
                    (setCollateralWallet alice . setDontBalance . setFixedFee 1_000_000)
                ),
              testBalancingFailsWith
                "A collateral wallet needs to be provided when auto balancing is enabled and script are involved..."
                failsLackOfCollateralWallet
                ( testingBalancingTemplate
                    (Script.ada 49)
                    mempty
                    (aliceNAdaUtxos 8)
                    emptySearch
                    emptySearch
                    True
                    (setDontBalance . setFixedFee 1_000_000)
                ),
              testBalancingSucceedsWith
                "... but is not necessary otherwise."
                [hasFee 1_000_000, insNb 1, additionalOutsNb 0, colInsNb 0, retOutsNb 3]
                ( testingBalancingTemplate
                    (Script.ada 7)
                    mempty
                    (aliceNAdaUtxos 8)
                    emptySearch
                    emptySearch
                    False
                    (setDontBalance . setFixedFee 1_000_000)
                ),
              testBalancingSucceedsWith
                "We can also directly give a set of collateral utxos..."
                [hasFee 1_000_000, insNb 2, additionalOutsNb 0, colInsNb 1, retOutsNb 3]
                ( testingBalancingTemplate
                    (Script.ada 49)
                    mempty
                    (aliceNAdaUtxos 8)
                    emptySearch
                    (aliceNAdaUtxos 8)
                    True
                    (setDontBalance . setFixedFee 1_000_000)
                ),
              testBalancingSucceedsWith
                "... which will be ignored when no script is involved"
                [hasFee 1_000_000, insNb 1, additionalOutsNb 0, colInsNb 0, retOutsNb 3]
                ( testingBalancingTemplate
                    (Script.ada 7)
                    mempty
                    (aliceNAdaUtxos 8)
                    emptySearch
                    (aliceNAdaUtxos 8)
                    False
                    (setDontBalance . setFixedFee 1_000_000)
                )
            ],
          testGroup
            "Manual balancing with auto fee"
            [ testCase "Auto fee with manual balancing yields maximum fee" $
                testProp $
                  mustSucceedTest noBalanceMaxFee `withInitDist` initialDistributionBalancing
            ],
          testGroup
            "Auto balancing with auto fee"
            [ testBalancingSucceedsWith
                "We can auto balance a transaction with auto fee, collaterals and no pk inputs"
                [insNb 1, additionalOutsNb 1, colInsNb 1, retOutsNb 3]
                ( simplePaymentToBob
                    20_000_000
                    0
                    0
                    0
                    True
                    id
                ),
              testBalancingSucceedsWith
                "We can auto balance a transaction with auto fee, no collateral and no script inputs"
                [insNb 1, additionalOutsNb 1, colInsNb 0, retOutsNb 3]
                ( simplePaymentToBob
                    20_000_000
                    0
                    0
                    0
                    False
                    id
                ),
              testBalancingSucceedsWith
                "We can auto balance a transaction with auto fee, collaterals, script and pk inputs"
                [insNb 2, additionalOutsNb 1, colInsNb 1, retOutsNb 3]
                ( simplePaymentToBob
                    60_000_000
                    0
                    0
                    0
                    True
                    id
                ),
              testCase "Auto fee are minimal: less fee will lead to strictly smaller fee than Cardano's estimate" $
                testProp $
                  mustSucceedTest balanceReduceFee
                    `withInitDist` initialDistributionBalancing
                    `withValuePred` \(feeBalanced, feeBalanced', feeBalancedManual, feeBalancedManual') ->
                      testBool $ feeBalanced' <= feeBalanced && feeBalancedManual' > feeBalancedManual,
              testCase "The auto-fee process can sometimes recover from a temporary balancing error..." $
                testProp $
                  mustSucceedTest
                    ( simplePaymentToBob
                        103_650_000
                        0
                        0
                        0
                        False
                        id
                    )
                    `withInitDist` initialDistributionBalancing,
              testCase "... but not always" $
                testProp $
                  mustFailTest
                    ( simplePaymentToBob
                        104_000_000
                        0
                        0
                        0
                        False
                        id
                    )
                    `withInitDist` initialDistributionBalancing
                    `withErrorPred` failsAtBalancing,
              testCase "The auto-fee process can recover from a temporary collateral error..." $
                testProp $
                  mustSucceedTest
                    ( testingBalancingTemplate
                        (Script.ada 142)
                        mempty
                        emptySearch
                        emptySearch
                        (aliceNAdaUtxos 2)
                        True
                        id
                    )
                    `withInitDist` initialDistributionBalancing,
              testCase "... but not always" $
                testProp $
                  mustFailTest
                    ( testingBalancingTemplate
                        (Script.ada 142)
                        mempty
                        (utxosAtSearch alice)
                        emptySearch
                        (aliceNAdaUtxos 1)
                        True
                        id
                    )
                    `withInitDist` initialDistributionBalancing
                    `withErrorPred` failsAtCollaterals,
              testCase "Reaching magical spot with the exact balance during auto fee computation" $
                testProp $
                  mustSucceedTest reachingMagic
                    `withInitDist` initialDistributionBalancing
            ],
          testGroup
            "Auto balancing with manual fee"
            [ testBalancingSucceedsWith
                "We can use a single utxo for balancing purpose"
                [hasFee 1_000_000, insNb 1, additionalOutsNb 1, colInsNb 0, retOutsNb 3]
                ( simplePaymentToBob
                    20_000_000
                    0
                    0
                    0
                    False
                    (setFixedFee 1_000_000)
                ),
              testBalancingSucceedsWith
                "We can use several utxos for balancing with ridiculously high fee"
                [hasFee 40_000_000, insNb 3, additionalOutsNb 1, colInsNb 0, retOutsNb 3]
                ( simplePaymentToBob
                    20_000_000
                    0
                    0
                    0
                    False
                    (setFixedFee 40_000_000)
                ),
              testBalancingFailsWith
                "We cannot balance with too little fee"
                failsWithTooLittleFee
                ( simplePaymentToBob
                    20_000_000
                    0
                    0
                    0
                    False
                    (setFixedFee 150_000)
                ),
              testBalancingFailsWith
                "Fee are rightfully included in the balancing process, which fails when they are too high"
                (failsAtBalancingWith (Script.ada 1) alice)
                ( simplePaymentToBob
                    100_000_000
                    0
                    0
                    0
                    False
                    (setFixedFee 6_000_000)
                ),
              testBalancingFailsWith
                "Collaterals are rightfully included in the balancing process, which fails when they are too high"
                (failsAtCollateralsWith 80_000_000)
                ( simplePaymentToBob
                    48_000_000
                    0
                    0
                    0
                    True
                    (setFixedFee 80_000_000)
                ),
              testBalancingSucceedsWith
                "Exactly the right amount leads to no output change"
                [hasFee 2_000_000, insNb 3, additionalOutsNb 0, colInsNb 0, retOutsNb 3]
                ( simplePaymentToBob
                    65_000_000
                    3
                    6
                    0
                    False
                    (setFixedFee 2_000_000)
                ),
              testBalancingSucceedsWith
                "It still leads to no output change when requesting a new output"
                [hasFee 2_000_000, insNb 3, additionalOutsNb 0, colInsNb 0, retOutsNb 3]
                ( simplePaymentToBob
                    65_000_000
                    3
                    6
                    0
                    False
                    (setDontAdjustOutput . setFixedFee 2_000_000)
                ),
              testBalancingSucceedsWith
                "1 lovelace more than the exact right amount leads to an additional output"
                [hasFee 2_000_000, insNb 3, additionalOutsNb 1, colInsNb 0, retOutsNb 3]
                ( simplePaymentToBob
                    65_000_001
                    3
                    6
                    0
                    False
                    (setFixedFee 2_000_000)
                ),
              testBalancingSucceedsWith
                "1 lovelace less than the exact right amount leads to an additional output to account for minAda"
                [hasFee 2_000_000, insNb 3, additionalOutsNb 1, colInsNb 0, retOutsNb 3]
                ( simplePaymentToBob
                    64_999_999
                    3
                    6
                    0
                    False
                    (setFixedFee 2_000_000)
                ),
              testBalancingSucceedsWith
                "We can merge assets to an existing outputs at the balancing wallet address"
                [hasFee 2_000_000, insNb 1, additionalOutsNb 0, colInsNb 0, retOutsNb 3]
                ( bothPaymentsToBobAndAlice
                    6_000_000
                    False
                    (setFixedFee 2_000_000)
                ),
              testBalancingSucceedsWith
                "We can create a new output at the balancing wallet address even if one already exists"
                [hasFee 2_000_000, insNb 1, additionalOutsNb 1, colInsNb 0, retOutsNb 3]
                ( bothPaymentsToBobAndAlice
                    6_000_000
                    False
                    (setFixedFee 2_000_000 . setDontAdjustOutput)
                ),
              testBalancingSucceedsWith
                "We can balance transactions with non-ada assets"
                [hasFee 2_000_000, insNb 1, additionalOutsNb 1, colInsNb 0, retOutsNb 3]
                ( simplePaymentToBob
                    0
                    0
                    5
                    0
                    False
                    (setFixedFee 2_000_000 . setEnsureMinAda)
                ),
              testBalancingSucceedsWith
                "Successful balancing with multiple assets"
                [hasFee 1_000_000, insNb 2, additionalOutsNb 1, colInsNb 0, retOutsNb 3]
                ( simplePaymentToBob
                    0
                    2
                    5
                    0
                    False
                    (setEnsureMinAda . setFixedFee 1_000_000)
                ),
              testBalancingFailsWith
                "Unsuccessful balancing with multiple assets in non value only utxos"
                (failsAtBalancingWith (banana 4) alice)
                ( simplePaymentToBob
                    0
                    2
                    5
                    4
                    False
                    (setEnsureMinAda . setFixedFee 1_000_000)
                ),
              testBalancingSucceedsWith
                "Successful balancing with multiple assets and explicit utxo set, reference script is lost"
                [hasFee 1_000_000, insNb 3, additionalOutsNb 1, colInsNb 0, retOutsNb 2]
                ( testingBalancingTemplate
                    (apple 2 <> orange 5 <> banana 4)
                    mempty
                    emptySearch
                    (utxosAtSearch alice)
                    emptySearch
                    False
                    (setEnsureMinAda . setFixedFee 1_000_000)
                ),
              testBalancingSucceedsWith
                "Successful balancing with excess initial consumption"
                [hasFee 1_000_000, insNb 5, additionalOutsNb 1, colInsNb 0, retOutsNb 3]
                ( testingBalancingTemplate
                    mempty
                    mempty
                    (onlyValueOutputsAtSearch alice)
                    emptySearch
                    emptySearch
                    False
                    (setFixedFee 1_000_000)
                )
            ]
        ]
