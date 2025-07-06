module Spec.Balancing where

import Cooked
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
import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import Test.Tasty
import Test.Tasty.HUnit

alice, bob :: Wallet
(alice, bob) = (wallet 1, wallet 2)

apple, orange, banana :: Integer -> Api.Value
apple = Script.multiPurposeScriptValue Script.trueMintingMPScript $ Api.TokenName "apple"
orange = Script.multiPurposeScriptValue Script.trueMintingMPScript $ Api.TokenName "orange"
banana = Script.multiPurposeScriptValue Script.trueMintingMPScript $ Api.TokenName "banana"

initialDistributionBalancing :: InitialDistribution
initialDistributionBalancing =
  InitialDistribution
    [ Script.trueSpendingMPScript @() `receives` (FixedValue (Script.ada 42) <&&> VisibleHashedDatum ()),
      alice `receives` FixedValue (Script.ada 2 <> apple 3),
      alice `receives` FixedValue (Script.ada 25),
      alice `receives` FixedValue (Script.ada 40 <> orange 6),
      alice `receives` FixedValue (Script.ada 8),
      alice `receives` FixedValue (Script.ada 30),
      alice `receives` (FixedValue (Script.lovelace 1280229 <> banana 3) <&&> VisibleHashedDatum (10 :: Integer)),
      alice `receives` (FixedValue (Script.ada 1 <> banana 7) <&&> ReferenceScript (Script.trueSpendingMPScript @())),
      alice `receives` (FixedValue (Script.ada 105 <> banana 2) <&&> VisibleHashedDatum ())
    ]

type TestBalancingOutcome = (TxSkel, TxSkel, Integer, Maybe (Set Api.TxOutRef, Wallet), [Api.TxOutRef])

spendsScriptUtxo :: (MonadBlockChain m) => Bool -> m (Map Api.TxOutRef TxSkelRedeemer)
spendsScriptUtxo False = return Map.empty
spendsScriptUtxo True = do
  (scriptOutRef, _) : _ <- runUtxoSearch $ utxosOwnedBySearch $ Script.trueSpendingMPScript @()
  return $ Map.singleton scriptOutRef emptyTxSkelRedeemerNoAutoFill

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
  (TxSkelOpts -> TxSkelOpts) ->
  -- Wether to adjust the output with min ada
  Bool ->
  m TestBalancingOutcome
testingBalancingTemplate toBobValue toAliceValue spendSearch balanceSearch collateralSearch consumeScriptUtxo optionsMod adjust = do
  ((fst <$>) -> toSpendUtxos) <- runUtxoSearch spendSearch
  ((fst <$>) -> toBalanceUtxos) <- runUtxoSearch balanceSearch
  ((fst <$>) -> toCollateralUtxos) <- runUtxoSearch collateralSearch
  additionalSpend <- spendsScriptUtxo consumeScriptUtxo
  let valueConstr = if adjust then Value else FixedValue
      skel =
        txSkelTemplate
          { txSkelOuts =
              List.filter
                ((/= mempty) . (^. txSkelOutValueL % txSkelOutValueContentL))
                [ bob `receives` valueConstr toBobValue,
                  alice `receives` valueConstr toAliceValue
                ],
            txSkelIns = additionalSpend <> Map.fromList ((,emptyTxSkelRedeemer) <$> toSpendUtxos),
            txSkelOpts =
              optionsMod
                def
                  { txSkelOptBalancingUtxos =
                      if List.null toBalanceUtxos
                        then BalancingUtxosFromBalancingWallet
                        else BalancingUtxosFromSet $ Set.fromList toBalanceUtxos,
                    txSkelOptCollateralUtxos =
                      if List.null toCollateralUtxos
                        then CollateralUtxosFromBalancingWallet
                        else CollateralUtxosFromSet (Set.fromList toCollateralUtxos) alice
                  },
            txSkelSigners = [alice]
          }
  (skel', fee, mCols) <- balanceTxSkel skel
  validateTxSkel_ skel
  nonOnlyValueUtxos <- runUtxoSearch aliceNonOnlyValueUtxos
  return (skel, skel', fee, mCols, fst <$> nonOnlyValueUtxos)

aliceNonOnlyValueUtxos :: (MonadBlockChain m) => UtxoSearch m TxSkelOut
aliceNonOnlyValueUtxos =
  utxosOwnedBySearch alice `filterWithPred` \o ->
    isJust (txSkelOutReferenceScript o)
      || isJust (preview (txSkelOutDatumL % txSkelOutDatumContentAT) o)

aliceNAdaUtxos :: (MonadBlockChain m) => Integer -> UtxoSearch m TxSkelOut
aliceNAdaUtxos n = utxosOwnedBySearch alice `filterWithValuePred` (\v -> Api.lovelaceValueOf v == Api.Lovelace (n * 1_000_000))

aliceRefScriptUtxos :: (MonadBlockChain m) => UtxoSearch m TxSkelOut
aliceRefScriptUtxos = utxosOwnedBySearch alice `filterWithPred` \o -> isJust (txSkelOutReferenceScript o)

emptySearch :: (MonadBlockChain m) => UtxoSearch m TxSkelOut
emptySearch = ListT.fromFoldable []

simplePaymentToBob :: (MonadBlockChain m) => Integer -> Integer -> Integer -> Integer -> Bool -> (TxSkelOpts -> TxSkelOpts) -> Bool -> m TestBalancingOutcome
simplePaymentToBob lv apples oranges bananas =
  testingBalancingTemplate
    (Script.lovelace lv <> apple apples <> orange oranges <> banana bananas)
    mempty
    emptySearch
    emptySearch
    emptySearch

bothPaymentsToBobAndAlice :: (MonadBlockChain m) => Integer -> Bool -> (TxSkelOpts -> TxSkelOpts) -> Bool -> m TestBalancingOutcome
bothPaymentsToBobAndAlice val =
  testingBalancingTemplate
    (Script.lovelace val)
    (Script.lovelace val)
    emptySearch
    emptySearch
    emptySearch

noBalanceMaxFee :: (MonadBlockChain m) => m ()
noBalanceMaxFee = do
  maxFee <- snd <$> getMinAndMaxFee 0
  ((txOutRef, _) : _) <- runUtxoSearch $ utxosOwnedBySearch alice `filterWithValuePred` (== Script.ada 30)
  validateTxSkel_ $
    txSkelTemplate
      { txSkelOuts = [bob `receives` Value (Script.lovelace (30_000_000 - maxFee))],
        txSkelIns = Map.singleton txOutRef emptyTxSkelRedeemer,
        txSkelOpts =
          def
            { txSkelOptBalancingPolicy = DoNotBalance,
              txSkelOptFeePolicy = AutoFeeComputation
            },
        txSkelSigners = [alice]
      }

balanceReduceFee :: (MonadBlockChain m) => m (Integer, Integer, Integer, Integer)
balanceReduceFee = do
  let skelAutoFee =
        txSkelTemplate
          { txSkelOuts = [bob `receives` Value (Script.ada 50)],
            txSkelSigners = [alice]
          }
  (skelBalanced, feeBalanced, mCols) <- balanceTxSkel skelAutoFee
  feeBalanced' <- estimateTxSkelFee skelBalanced feeBalanced mCols
  let skelManualFee =
        skelAutoFee
          { txSkelOpts =
              def
                { txSkelOptFeePolicy = ManualFee (feeBalanced - 1)
                }
          }
  (skelBalancedManual, feeBalancedManual, mColsManual) <- balanceTxSkel skelManualFee
  feeBalancedManual' <- estimateTxSkelFee skelBalancedManual feeBalancedManual mColsManual
  return (feeBalanced, feeBalanced', feeBalancedManual, feeBalancedManual')

reachingMagic :: (MonadBlockChain m) => m ()
reachingMagic = do
  bananaOutRefs <- (fst <$>) <$> runUtxoSearch (utxosOwnedBySearch alice `filterWithValuePred` (banana 1 `Api.leq`))
  validateTxSkel_ $
    txSkelTemplate
      { txSkelOuts = [bob `receives` Value (Script.ada 106 <> banana 12)],
        txSkelSigners = [alice],
        txSkelOpts =
          def
            { txSkelOptBalancingUtxos = BalancingUtxosFromSet (Set.fromList bananaOutRefs)
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
  testCooked msg $
    mustSucceedTest run
      `withInitDist` initialDistributionBalancing
      `withResultProp` \res -> testConjoin (($ res) <$> props)

failsAtBalancingWith :: Api.Value -> Wallet -> MockChainError -> Assertion
failsAtBalancingWith val' wal' (MCEUnbalanceable wal val) = testBool $ val' == val && wal' == wal
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
failsWithEmptyTxIns (MCEValidationError Ledger.Phase1 (Ledger.CardanoLedgerValidationError text)) = testBool $ isInfixOf "InputSetEmptyUTxO" text
failsWithEmptyTxIns _ = testBool False

failsAtCollateralsWith :: Integer -> MockChainError -> Assertion
failsAtCollateralsWith fee' (MCENoSuitableCollateral fee percentage val) = testBool $ fee == fee' && val == Script.lovelace (1 + (fee * percentage) `div` 100)
failsAtCollateralsWith _ _ = testBool False

failsAtCollaterals :: MockChainError -> Assertion
failsAtCollaterals MCENoSuitableCollateral {} = testBool True
failsAtCollaterals _ = testBool False

failsLackOfCollateralWallet :: MockChainError -> Assertion
failsLackOfCollateralWallet (MCEMissingBalancingWallet msg) = "Collateral utxos should be taken from the balancing wallet, but it does not exist." .==. msg
failsLackOfCollateralWallet _ = testBool False

testBalancingFailsWith :: (Show a) => String -> (MockChainError -> Assertion) -> StagedMockChain a -> TestTree
testBalancingFailsWith msg p smc =
  testCooked msg $
    mustFailTest smc
      `withInitDist` initialDistributionBalancing
      `withErrorProp` p

tests :: TestTree
tests =
  let setFixedFee fee txSkelOpts = txSkelOpts {txSkelOptFeePolicy = ManualFee fee}
      setDontAdjustOutput txSkelOpts = txSkelOpts {txSkelOptBalanceOutputPolicy = DontAdjustExistingOutput}
      setDontBalance txSkelOpts = txSkelOpts {txSkelOptBalancingPolicy = DoNotBalance}
      setCollateralWallet wallet' txSkelOpts = txSkelOpts {txSkelOptCollateralUtxos = CollateralUtxosFromWallet wallet'}
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
                    False
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
                    False
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
                    False
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
                    False
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
                    False
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
                    False
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
                    False
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
                    False
                )
            ],
          testGroup
            "Manual balancing with auto fee"
            [ testCooked "Auto fee with manual balancing yields maximum fee" $
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
                    False
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
                    False
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
                    False
                ),
              testCooked "Auto fee are minimal: less fee will lead to strictly smaller fee than Cardano's estimate" $
                mustSucceedTest balanceReduceFee
                  `withInitDist` initialDistributionBalancing
                  `withResultProp` \(feeBalanced, feeBalanced', feeBalancedManual, feeBalancedManual') ->
                    testBool $ feeBalanced' <= feeBalanced && feeBalancedManual' > feeBalancedManual,
              testCooked "The auto-fee process can sometimes recover from a temporary balancing error..." $
                mustSucceedTest
                  ( simplePaymentToBob
                      103_650_000
                      0
                      0
                      0
                      False
                      id
                      False
                  )
                  `withInitDist` initialDistributionBalancing,
              testCooked "... but not always" $
                mustFailTest
                  ( simplePaymentToBob
                      104_000_000
                      0
                      0
                      0
                      False
                      id
                      False
                  )
                  `withInitDist` initialDistributionBalancing
                  `withErrorProp` failsAtBalancing,
              testCooked "The auto-fee process can recover from a temporary collateral error..." $
                mustSucceedTest
                  ( testingBalancingTemplate
                      (Script.ada 142)
                      mempty
                      emptySearch
                      emptySearch
                      (aliceNAdaUtxos 2)
                      True
                      id
                      False
                  )
                  `withInitDist` initialDistributionBalancing,
              testCooked "... but not always" $
                mustFailTest
                  ( testingBalancingTemplate
                      (Script.ada 142)
                      mempty
                      (utxosOwnedBySearch alice)
                      emptySearch
                      (aliceNAdaUtxos 1)
                      True
                      id
                      False
                  )
                  `withInitDist` initialDistributionBalancing
                  `withErrorProp` failsAtCollaterals,
              testCooked "Reaching magical spot with the exact balance during auto fee computation" $
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
                    False
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
                    False
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
                    False
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
                    False
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
                    False
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
                    False
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
                    False
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
                    False
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
                    False
                ),
              testBalancingSucceedsWith
                "We can merge assets to an existing outputs at the balancing wallet address"
                [hasFee 2_000_000, insNb 1, additionalOutsNb 0, colInsNb 0, retOutsNb 3]
                ( bothPaymentsToBobAndAlice
                    6_000_000
                    False
                    (setFixedFee 2_000_000)
                    False
                ),
              testBalancingSucceedsWith
                "We can create a new output at the balancing wallet address even if one already exists"
                [hasFee 2_000_000, insNb 1, additionalOutsNb 1, colInsNb 0, retOutsNb 3]
                ( bothPaymentsToBobAndAlice
                    6_000_000
                    False
                    (setFixedFee 2_000_000 . setDontAdjustOutput)
                    False
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
                    (setFixedFee 2_000_000)
                    True
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
                    (setFixedFee 1_000_000)
                    True
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
                    (setFixedFee 1_000_000)
                    True
                ),
              testBalancingSucceedsWith
                "Successful balancing with multiple assets and explicit utxo set, reference script is lost"
                [hasFee 1_000_000, insNb 3, additionalOutsNb 1, colInsNb 0, retOutsNb 2]
                ( testingBalancingTemplate
                    (apple 2 <> orange 5 <> banana 4)
                    mempty
                    emptySearch
                    (utxosOwnedBySearch alice)
                    emptySearch
                    False
                    (setFixedFee 1_000_000)
                    True
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
                    False
                )
            ]
        ]
