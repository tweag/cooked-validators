{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.AttackSpec (tests) where

import Control.Monad
import Cooked.Attack
import Cooked.Currencies
import Cooked.Ltl
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
import qualified Ledger as Pl hiding (singleton, validatorHash)
import qualified Ledger.Ada as Pl
import qualified Ledger.Scripts as Pl
import qualified Ledger.Typed.Scripts as Pl
import qualified Ledger.Value as Pl
import Optics.Core
import qualified Plutus.Script.Utils.V1.Scripts as Pl hiding (validatorHash)
import qualified PlutusTx as Pl
import qualified PlutusTx.Prelude as Pl
import Test.Tasty
import Test.Tasty.HUnit
import Type.Reflection

-- * General helpers

assertTxSkelEqual :: Maybe TxSkel -> Maybe TxSkel -> Assertion
assertTxSkelEqual expected actual =
  assertBool
    ( "non-equal 'TxSkel's:\nexpected:\n\n"
        ++ show (prettyTxSkel [] <$> expected)
        ++ "\n\nactual:\n\n"
        ++ show (prettyTxSkel [] <$> actual)
    )
    $ actual == expected

-- * Tests for the token duplication attack

{-# INLINEABLE mkCarefulPolicy #-}
mkCarefulPolicy :: Pl.TokenName -> Integer -> () -> Pl.ScriptContext -> Bool
mkCarefulPolicy tName allowedAmount _ ctx
  | amnt Pl.== Just allowedAmount = True
  | otherwise = Pl.trace "tried to mint wrong amount" False
  where
    txi = Pl.scriptContextTxInfo ctx

    amnt :: Maybe Integer
    amnt = case Pl.flattenValue (Pl.txInfoMint txi) of
      [(cs, tn, a)] | cs Pl.== Pl.ownCurrencySymbol ctx && tn Pl.== tName -> Just a
      _ -> Nothing

carefulPolicy :: Pl.TokenName -> Integer -> Pl.MintingPolicy
carefulPolicy tName allowedAmount =
  Pl.mkMintingPolicyScript $
    $$(Pl.compile [||\n x -> Pl.mkUntypedMintingPolicy (mkCarefulPolicy n x)||])
      `Pl.applyCode` Pl.liftCode tName
      `Pl.applyCode` Pl.liftCode allowedAmount

{-# INLINEABLE mkCarelessPolicy #-}
mkCarelessPolicy :: () -> Pl.ScriptContext -> Bool
mkCarelessPolicy _ _ = True

carelessPolicy :: Pl.MintingPolicy
carelessPolicy =
  Pl.mkMintingPolicyScript
    $$(Pl.compile [||Pl.mkUntypedMintingPolicy mkCarelessPolicy||])

dupTokenTrace :: MonadBlockChain m => Pl.MintingPolicy -> Pl.TokenName -> Integer -> Wallet -> m ()
dupTokenTrace pol tName amount recipient = void $ validateTxSkel skel
  where
    skel =
      txSkelOpts (def {adjustUnbalTx = True}) $
        let minted = Pl.singleton (Pl.scriptCurrencySymbol pol) tName amount
         in [Mints (Nothing @()) [pol] minted]
              :=>: [paysPK (walletPKHash recipient) minted]

dupTokenAttackTests :: TestTree
dupTokenAttackTests =
  testGroup
    "token duplication attack"
    [ testCase "unit test on a 'TxSkel'" $
        let attacker = wallet 6
            tName1 = Pl.tokenName "MockToken1"
            tName2 = Pl.tokenName "MockToken2"
            pol1 = carefulPolicy tName1 1
            pol2 = carelessPolicy
            ac1 = Pl.assetClass (Pl.scriptCurrencySymbol pol1) tName1
            ac2 = Pl.assetClass (Pl.scriptCurrencySymbol pol2) tName2
            skelIn =
              txSkel
                ( [ Mints (Nothing @()) [pol1, pol2] (Pl.assetClassValue ac1 1 <> Pl.assetClassValue ac2 1),
                    Mints (Nothing @()) [pol2] (Pl.assetClassValue ac2 3),
                    Mints (Nothing @()) [pol1] (Pl.assetClassValue ac1 7)
                  ]
                    :=>: [ paysPK (walletPKHash (wallet 1)) (Pl.assetClassValue ac1 1 <> Pl.lovelaceValueOf 1234),
                           paysPK (walletPKHash (wallet 2)) (Pl.assetClassValue ac2 2)
                         ]
                )
            skelOut select =
              dupTokenAttack
                select
                attacker
                skelIn
            skelExpected v1 v2 v3 v4 =
              txSkelLbl
                DupTokenLbl
                ( [ Mints (Nothing @()) [pol1, pol2] (Pl.assetClassValue ac1 v1 <> Pl.assetClassValue ac2 v2),
                    Mints (Nothing @()) [pol2] (Pl.assetClassValue ac2 v3),
                    Mints (Nothing @()) [pol1] (Pl.assetClassValue ac1 v4)
                  ]
                    :=>: [ paysPK (walletPKHash (wallet 1)) (Pl.assetClassValue ac1 1 <> Pl.lovelaceValueOf 1234),
                           paysPK (walletPKHash (wallet 2)) (Pl.assetClassValue ac2 2),
                           paysPK
                             (walletPKHash attacker)
                             (Pl.assetClassValue ac1 ((v1 - 1) + (v4 - 7)) <> Pl.assetClassValue ac2 ((v2 - 1) + (v3 - 3)))
                         ]
                )
         in assertTxSkelEqual (Just $ skelExpected 2 2 4 8) (skelOut (\_ n -> Just $ n + 1))
              .&&. assertTxSkelEqual Nothing (skelOut (\_ n -> Just n))
              .&&. assertTxSkelEqual
                (Just $ skelExpected 6 1 3 12)
                (skelOut (\ac n -> if ac == ac1 then Just (n + 5) else Nothing)),
      testCase "careful minting policy" $
        let tName = Pl.tokenName "MockToken"
            pol = carefulPolicy tName 1
         in testFailsFrom'
              isCekEvaluationFailure
              def
              ( somewhere
                  (dupTokenAttack (\_ n -> Just $ n + 1) (wallet 6))
                  (dupTokenTrace pol tName 1 (wallet 1))
              ),
      testCase "careless minting policy" $
        let tName = Pl.tokenName "MockToken"
            pol = carelessPolicy
         in testSucceeds $
              somewhere
                (dupTokenAttack (\_ n -> Just $ n + 1) (wallet 6))
                (dupTokenTrace pol tName 1 (wallet 1)),
      testCase "pre-existing tokens are left alone" $
        let attacker = wallet 6
            pol = carelessPolicy
            ac1 = Pl.assetClass (Pl.scriptCurrencySymbol pol) (Pl.tokenName "mintedToken")
            ac2 = quickAssetClass "preExistingToken"
            skelIn =
              txSkel
                ( [Mints (Nothing @()) [pol] (Pl.assetClassValue ac1 1)]
                    :=>: [ paysPK
                             (walletPKHash (wallet 1))
                             (Pl.assetClassValue ac1 1 <> Pl.assetClassValue ac2 2)
                         ]
                )
            skelExpected =
              txSkelLbl
                DupTokenLbl
                ( [Mints (Nothing @()) [pol] (Pl.assetClassValue ac1 2)]
                    :=>: [ paysPK
                             (walletPKHash (wallet 1))
                             (Pl.assetClassValue ac1 1 <> Pl.assetClassValue ac2 2),
                           paysPK
                             (walletPKHash attacker)
                             (Pl.assetClassValue ac1 1)
                         ]
                )
            skelOut = dupTokenAttack (\_ i -> Just $ i + 1) attacker skelIn
         in assertTxSkelEqual (Just skelExpected) skelOut
    ]

-- * Tests for the datum hijacking attack

-- ** Mock contract for the datum hijacking attack

-- This is a very simple contract: The first transaction locks some Ada to the
-- validator, using the datum 'FirstLock', the second transaction then re-locks
-- the same amount to the same validator, using the datum 'SecondLock'. The
-- datum hijacking attack should target the secon transaction, and substitute a
-- different recipient.

data MockDatum = FirstLock | SecondLock deriving (Show)

instance Pl.Eq MockDatum where
  {-# INLINEABLE (==) #-}
  FirstLock == FirstLock = True
  SecondLock == SecondLock = True
  _ == _ = False

Pl.makeLift ''MockDatum
Pl.unstableMakeIsData ''MockDatum

data MockContract

instance Pl.ValidatorTypes MockContract where
  type DatumType MockContract = MockDatum
  type RedeemerType MockContract = ()

-- ** Transactions (and 'TxSkels') for the datum hijacking attack

lockValue :: Pl.Value
lockValue = Pl.lovelaceValueOf 12345678

lockTxSkel :: SpendableOut -> Pl.TypedValidator MockContract -> TxSkel
lockTxSkel o v =
  txSkelOpts
    (def {adjustUnbalTx = True})
    ([SpendsPK o] :=>: [paysScript v FirstLock lockValue])

txLock :: MonadBlockChain m => Pl.TypedValidator MockContract -> m ()
txLock v = do
  me <- ownPaymentPubKeyHash
  utxo : _ <- pkUtxosSuchThatValue me (`Pl.geq` lockValue)
  void $ validateTxSkel $ lockTxSkel utxo v

relockTxSkel :: Pl.TypedValidator MockContract -> SpendableOut -> TxSkel
relockTxSkel v o =
  txSkelOpts
    (def {adjustUnbalTx = True})
    ( [SpendsScript v () (o, FirstLock)]
        :=>: [paysScript v SecondLock lockValue]
    )

txRelock ::
  MonadBlockChain m =>
  Pl.TypedValidator MockContract ->
  m ()
txRelock v = do
  utxo : _ <- scriptUtxosSuchThat v (\d _ -> FirstLock Pl.== d)
  void $ validateTxSkel $ relockTxSkel v (fst utxo)

-- ** Validators for the datum hijacking attack

-- | Try to extract a datum from an output.
{-# INLINEABLE outputDatum #-}
outputDatum :: Pl.TxInfo -> Pl.TxOut -> Maybe MockDatum
outputDatum txi o = do
  h <- Pl.txOutDatum o
  Pl.Datum d <- Pl.findDatum h txi
  Pl.fromBuiltinData d

{-# INLINEABLE mkMockValidator #-}
mkMockValidator :: (Pl.ScriptContext -> [Pl.TxOut]) -> MockDatum -> () -> Pl.ScriptContext -> Bool
mkMockValidator getOutputs datum _ ctx =
  let txi = Pl.scriptContextTxInfo ctx
   in case datum of
        FirstLock ->
          case getOutputs ctx of
            o : _ ->
              Pl.traceIfFalse
                "not in 'SecondLock'-state after re-locking"
                (outputDatum txi o Pl.== Just SecondLock)
                && Pl.traceIfFalse
                  "not re-locking the right amout"
                  (Pl.txOutValue o == lockValue)
            _ -> Pl.trace "there must be a output re-locked" False
        SecondLock -> False

{-# INLINEABLE mkCarefulValidator #-}
mkCarefulValidator :: MockDatum -> () -> Pl.ScriptContext -> Bool
mkCarefulValidator = mkMockValidator Pl.getContinuingOutputs

carefulValidator :: Pl.TypedValidator MockContract
carefulValidator =
  Pl.mkTypedValidator @MockContract
    $$(Pl.compile [||mkCarefulValidator||])
    $$(Pl.compile [||wrap||])
  where
    wrap = Pl.mkUntypedValidator @MockDatum @()

{-# INLINEABLE mkCarelessValidator #-}
mkCarelessValidator :: MockDatum -> () -> Pl.ScriptContext -> Bool
mkCarelessValidator = mkMockValidator (Pl.txInfoOutputs . Pl.scriptContextTxInfo)

carelessValidator :: Pl.TypedValidator MockContract
carelessValidator =
  Pl.mkTypedValidator @MockContract
    $$(Pl.compile [||mkCarelessValidator||])
    $$(Pl.compile [||wrap||])
  where
    wrap = Pl.mkUntypedValidator @MockDatum @()

datumHijackingTrace :: MonadBlockChain m => Pl.TypedValidator MockContract -> m ()
datumHijackingTrace v = do
  txLock v
  txRelock v

datumHijackingAttackTests :: TestTree
datumHijackingAttackTests =
  testGroup
    "datum hijacking attack"
    [ testCase "unit test on a 'TxSkel'" $
        let val1 = carelessValidator
            val2 = carefulValidator
            thief = datumHijackingTarget @MockContract
            x1 = Pl.lovelaceValueOf 10001
            x2 = Pl.lovelaceValueOf 10000
            x3 = Pl.lovelaceValueOf 9999
            skelIn =
              txSkel
                [ paysScript val1 SecondLock x1,
                  paysScript val1 SecondLock x3,
                  paysScript val2 SecondLock x1,
                  paysScript val1 FirstLock x2,
                  paysScript val1 SecondLock x2
                ]
            skelOut select =
              datumHijackingAttack @MockContract
                ( \v d x ->
                    Pl.validatorHash val1 == Pl.validatorHash v
                      && SecondLock Pl.== d
                      && x2 `Pl.geq` x
                )
                select
                skelIn
            skelExpected a b =
              txSkelLbl
                (DatumHijackingLbl $ Pl.validatorHash thief)
                [ paysScript val1 SecondLock x1,
                  paysScript a SecondLock x3,
                  paysScript val2 SecondLock x1,
                  paysScript val1 FirstLock x2,
                  paysScript b SecondLock x2
                ]
         in assertTxSkelEqual (Just $ skelExpected thief val1) (skelOut (0 ==))
              .&&. assertTxSkelEqual (Just $ skelExpected val1 thief) (skelOut (1 ==))
              .&&. assertTxSkelEqual (Just $ skelExpected thief thief) (skelOut (const True)),
      testCase "careful validator" $
        testFailsFrom'
          isCekEvaluationFailure
          def
          ( somewhere
              ( datumHijackingAttack @MockContract
                  ( \v d _ ->
                      Pl.validatorHash v == Pl.validatorHash carefulValidator
                        && SecondLock Pl.== d
                  )
                  (const True)
              )
              (datumHijackingTrace carefulValidator)
          ),
      testCase "careless validator" $
        testSucceeds
          ( somewhere
              ( datumHijackingAttack @MockContract
                  ( \v d _ ->
                      Pl.validatorHash v == Pl.validatorHash carelessValidator
                        && SecondLock Pl.== d
                  )
                  (const True)
              )
              (datumHijackingTrace carelessValidator)
          )
    ]

-- * Collecting all tests in this module

tests :: [TestTree]
tests =
  [ testGroup
      "Attack DSL"
      [ dupTokenAttackTests,
        datumHijackingAttackTests
      ]
  ]
