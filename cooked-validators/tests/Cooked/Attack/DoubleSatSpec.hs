{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.Attack.DoubleSatSpec (tests) where

import Control.Arrow
import Cooked.Attack
import Cooked.Attack.DoubleSat
import Cooked.MockChain
import Cooked.TestUtils
import Cooked.Tx.Constraints.Optics
import Cooked.Tx.Constraints.Type
import Data.Default
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Ledger.Ada as L
import Ledger.Typed.Scripts
import qualified Ledger.Typed.Scripts as L
import qualified Ledger.Value as L
import Optics.Core
import qualified Plutus.Script.Utils.V1.Scripts as L
import qualified Plutus.V1.Ledger.Contexts as L
import qualified Plutus.V1.Ledger.Interval as Pl
import qualified PlutusTx as Pl
import qualified PlutusTx.Eq as Pl
import qualified PlutusTx.Prelude as Pl
import Test.Tasty
import Test.Tasty.HUnit

-- * Mock contracts for the double satisfaction attack

-- Scenario: There are two validators, one of type A, one of type B. We want to
-- add an input belonging to the B validator to a transaction that spends from
-- the A validator.

data ADatum = ADatum deriving (Show)

instance Pl.Eq ADatum where
  ADatum == ADatum = True

Pl.makeLift ''ADatum
Pl.unstableMakeIsData ''ADatum

data ARedeemer = ARedeemer deriving (Show)

instance Pl.Eq ARedeemer where
  ARedeemer == ARedeemer = True

Pl.makeLift ''ARedeemer
Pl.unstableMakeIsData ''ARedeemer

data AContract

instance L.ValidatorTypes AContract where
  type DatumType AContract = ADatum
  type RedeemerType AContract = ARedeemer

{-# INLINEABLE mkAValidator #-}
mkAValidator :: ADatum -> ARedeemer -> L.ScriptContext -> Bool
mkAValidator _ _ _ = True

aValidator :: L.TypedValidator AContract
aValidator =
  L.mkTypedValidator @AContract
    $$(Pl.compile [||mkAValidator||])
    $$(Pl.compile [||wrap||])
  where
    wrap = L.mkUntypedValidator @ADatum @ARedeemer

data BDatum = BDatum deriving (Show)

instance Pl.Eq BDatum where
  BDatum == BDatum = True

Pl.makeLift ''BDatum
Pl.unstableMakeIsData ''BDatum

data BRedeemer = BRedeemer1 | BRedeemer2 deriving (Show)

instance Pl.Eq BRedeemer where
  BRedeemer1 == BRedeemer1 = True
  BRedeemer2 == BRedeemer2 = True
  _ == _ = False

Pl.makeLift ''BRedeemer
Pl.unstableMakeIsData ''BRedeemer

data BContract

instance L.ValidatorTypes BContract where
  type DatumType BContract = BDatum
  type RedeemerType BContract = BRedeemer

{-# INLINEABLE mkBValidator #-}
mkBValidator :: BDatum -> BRedeemer -> L.ScriptContext -> Bool
mkBValidator _ _ _ = True

bValidator :: L.TypedValidator BContract
bValidator =
  L.mkTypedValidator @BContract
    $$(Pl.compile [||mkBValidator||])
    $$(Pl.compile [||wrap||])
  where
    wrap = L.mkUntypedValidator @BDatum @BRedeemer

-- | In the initial state of the Mockchain, the A and B validators each own
-- a few UTxOs, with different values
dsTestMockChainSt :: MockChainSt
dsTestMockChainSt = case runMockChainRaw def def setup of
  Left _ -> def -- this branch will not be reached
  Right (_, mcst) -> mcst
  where
    setup = do
      validateTxSkel' $ mempty {txSkelOuts = [paysScript aValidator (Right ADatum) (L.lovelaceValueOf 2_000_000)]}
      validateTxSkel' $ mempty {txSkelOuts = [paysScript aValidator (Right ADatum) (L.lovelaceValueOf 3_000_000)]}
      validateTxSkel' $ mempty {txSkelOuts = [paysScript aValidator (Right ADatum) (L.lovelaceValueOf 4_000_000)]}
      validateTxSkel' $ mempty {txSkelOuts = [paysScript aValidator (Right ADatum) (L.lovelaceValueOf 5_000_000)]}
      validateTxSkel' $ mempty {txSkelOuts = [paysScript bValidator (Right BDatum) (L.lovelaceValueOf 6_000_000)]}
      validateTxSkel' $ mempty {txSkelOuts = [paysScript bValidator (Right BDatum) (L.lovelaceValueOf 7_000_000)]}
      validateTxSkel' $ mempty {txSkelOuts = [paysScript bValidator (Right BDatum) (L.lovelaceValueOf 8_000_000)]}

tests :: TestTree
tests =
  testGroup
    "double satisfaction attack"
    $ let [[(aUtxo1, _)], [(aUtxo2, _)], [(aUtxo3, _)], [(aUtxo4, _)]] =
            map
              ( \n ->
                  scriptUtxosSuchThatMcst
                    dsTestMockChainSt
                    aValidator
                    (\_ v -> v == L.lovelaceValueOf n)
              )
              [ 2_000_000,
                3_000_000,
                4_000_000,
                5_000_000
              ]
          [[(bUtxo1, _)], [(bUtxo2, _)], [(bUtxo3, _)]] =
            map
              ( \n ->
                  scriptUtxosSuchThatMcst
                    dsTestMockChainSt
                    bValidator
                    (\_ v -> v == L.lovelaceValueOf n)
              )
              [ 6_000_000,
                7_000_000,
                8_000_000
              ]
       in [ testCase "the two test validators have different addresses" $
              assertBool "the addresses are the same" $
                L.validatorAddress aValidator /= L.validatorAddress bValidator,
            testGroup "unit tests on a 'TxSkel'" $
              -- The following tests make sure that, depending on some
              -- 'SpendsScript' constraints for UTxOs belonging to the
              -- 'aValidator' on the input 'TxSkel', the correct additional
              -- 'SpendsScript' constraints for UTxOs of the 'bValidator' are on
              -- the output 'TxSkel's. Both 'DoubleSatSplitMode's are tested.
              let -- generate an input skeleton from some 'aValidator' UTxOs to
                  -- be spent.
                  skelIn :: [SpendableOut] -> TxSkel
                  skelIn aUtxos =
                    mempty
                      { txSkelIns = Map.fromSet (const (SpendsScript aValidator ARedeemer)) $ Set.fromList aUtxos,
                        txSkelOuts = [paysPK (walletPKHash (wallet 2)) (L.lovelaceValueOf 2_500_000)]
                      }

                  -- apply the 'doubleSatAttack' to the input skeleton to
                  -- generate a list of output skeleta. The multiway-if
                  -- statement is what decides which UTxOs belonging to the
                  -- 'bValidator' to add, depending on the focused input
                  -- 'aValidator' UTxO.
                  skelsOut :: DoubleSatSplitMode -> [SpendableOut] -> [TxSkel]
                  skelsOut splitMode aUtxos =
                    fst
                      <$> getTweak
                        ( doubleSatAttack
                            (spendsScriptTypeF @AContract)
                            ( \mcst (aOut, _, _) ->
                                let bUtxos = fst <$> scriptUtxosSuchThatMcst mcst bValidator (\_ _ -> True)
                                    aValue = sOutValue aOut
                                 in if
                                        | aValue == L.lovelaceValueOf 2_000_000 ->
                                          [ toDelta bOut $ SpendsScript bValidator BRedeemer1
                                            | bOut <- bUtxos,
                                              sOutValue bOut == L.lovelaceValueOf 123 -- not satisfied by any UTxO in 'dsTestMockChain'
                                          ]
                                        | aValue == L.lovelaceValueOf 3_000_000 ->
                                          [ toDelta bOut $ SpendsScript bValidator BRedeemer1
                                            | bOut <- bUtxos,
                                              sOutValue bOut == L.lovelaceValueOf 6_000_000 -- satisfied by exactly one UTxO in 'dsTestMockChain'
                                          ]
                                        | aValue == L.lovelaceValueOf 4_000_000 ->
                                          concatMap
                                            ( \bOut ->
                                                let bValue = sOutValue bOut
                                                 in if
                                                        | bValue == L.lovelaceValueOf 6_000_000 ->
                                                          [toDelta bOut $ SpendsScript bValidator BRedeemer1]
                                                        | bValue == L.lovelaceValueOf 7_000_000 ->
                                                          [ toDelta bOut $ SpendsScript bValidator BRedeemer1,
                                                            toDelta bOut $ SpendsScript bValidator BRedeemer2
                                                          ]
                                                        | otherwise -> []
                                            )
                                            bUtxos
                                        | otherwise -> []
                            )
                            (wallet 6)
                            splitMode
                        )
                        dsTestMockChainSt
                        (skelIn aUtxos)
                    where
                      toDelta :: SpendableOut -> TxSkelIn -> DoubleSatDelta
                      toDelta sOut howSpent = (Map.singleton sOut howSpent, [], mempty)

                  -- generate a transaction that spends the given 'aValidator'
                  -- UTxOs (all with 'ARedeemer') and the 'bValidator' UTxOs
                  -- with the specified redeemers.
                  skelExpected :: [SpendableOut] -> [(BRedeemer, SpendableOut)] -> TxSkel
                  skelExpected aUtxos bUtxos =
                    mempty
                      { txSkelLabel = Set.singleton $ TxLabel DoubleSatLbl,
                        txSkelIns =
                          Map.fromList ((\(bRedeemer, bUtxo) -> (bUtxo, SpendsScript bValidator bRedeemer)) <$> bUtxos)
                            <> Map.fromSet (const (SpendsScript aValidator ARedeemer)) (Set.fromList aUtxos),
                        txSkelOuts =
                          [ paysPK (walletPKHash (wallet 2)) (L.lovelaceValueOf 2_500_000),
                            paysPK (walletPKHash (wallet 6)) (foldMap (sOutValue . snd) bUtxos)
                          ]
                      }
               in [ testGroup "with 'AllSeparate'" $
                      let thePredicate :: [SpendableOut] -> [[(BRedeemer, SpendableOut)]] -> Assertion
                          thePredicate aUtxos bUtxos =
                            assertSameSets
                              (skelExpected aUtxos <$> bUtxos)
                              (skelsOut AllSeparate aUtxos)
                       in [ testCase "no modified transactions if there's no suitable UTxO" $ thePredicate [aUtxo1] [],
                            testCase "exactly one modified transaction if there's one suitable UTxO" $ thePredicate [aUtxo2] [[(BRedeemer1, bUtxo1)]],
                            testCase "three modified transactions from 1+2 redeemers" $
                              thePredicate [aUtxo3] [[(BRedeemer1, bUtxo1)], [(BRedeemer1, bUtxo2)], [(BRedeemer2, bUtxo2)]],
                            testCase "no modified transactions if no redeemer is specified" $ thePredicate [aUtxo4] [],
                            testCase "with two foci, the correct combinations are returned" $
                              testConjoin $
                                map
                                  (uncurry thePredicate)
                                  [ ([aUtxo1, aUtxo4], []),
                                    ([aUtxo4, aUtxo1], []),
                                    ([aUtxo1, aUtxo2], [[(BRedeemer1, bUtxo1)]]),
                                    ([aUtxo2, aUtxo4], [[(BRedeemer1, bUtxo1)]]),
                                    ( [aUtxo2, aUtxo3],
                                      [[(BRedeemer1, bUtxo1)], [(BRedeemer1, bUtxo2)], [(BRedeemer2, bUtxo2)]]
                                    )
                                  ],
                            testCase "with all possible foci, no additional transactions are generated" $
                              thePredicate
                                [aUtxo1, aUtxo2, aUtxo3, aUtxo4]
                                -- the same list as in the last example
                                [[(BRedeemer1, bUtxo1)], [(BRedeemer1, bUtxo2)], [(BRedeemer2, bUtxo2)]]
                          ],
                    testGroup "with 'TryCombinations'" $
                      let thePredicate :: [SpendableOut] -> [[(BRedeemer, SpendableOut)]] -> Assertion
                          thePredicate aUtxos bUtxos =
                            assertSameSets
                              (skelExpected aUtxos <$> bUtxos)
                              (skelsOut TryCombinations aUtxos)
                       in [ testCase "no modified transactions if there's no suitable UTxO" $ thePredicate [aUtxo1] [],
                            testCase "exactly one modified transaction if there's one suitable UTxO" $ thePredicate [aUtxo2] [[(BRedeemer1, bUtxo1)]],
                            testCase "three modified transactions from 1+2 redeemers" $
                              thePredicate [aUtxo3] [[(BRedeemer1, bUtxo1)], [(BRedeemer1, bUtxo2)], [(BRedeemer2, bUtxo2)]],
                            testCase "no modified transactions if no redeemer is specified" $ thePredicate [aUtxo4] [],
                            testCase "with two foci, the correct combinations are returned" $
                              testConjoin $
                                map
                                  (uncurry thePredicate)
                                  [ ([aUtxo1, aUtxo4], []),
                                    ([aUtxo4, aUtxo1], []),
                                    ([aUtxo1, aUtxo2], [[(BRedeemer1, bUtxo1)]]),
                                    ([aUtxo2, aUtxo4], [[(BRedeemer1, bUtxo1)]]),
                                    ( [aUtxo2, aUtxo3],
                                      [ -- one extra input
                                        [(BRedeemer1, bUtxo1)],
                                        [(BRedeemer1, bUtxo2)],
                                        [(BRedeemer2, bUtxo2)],
                                        -- two extra inputs
                                        [(BRedeemer1, bUtxo1), (BRedeemer1, bUtxo2)],
                                        [(BRedeemer1, bUtxo1), (BRedeemer2, bUtxo2)]
                                      ]
                                    )
                                  ],
                            testCase "with all possible foci, no additional transactions are generated" $
                              thePredicate
                                [aUtxo1, aUtxo2, aUtxo3, aUtxo4]
                                -- the same list  as in the last example
                                [ [(BRedeemer1, bUtxo1)],
                                  [(BRedeemer1, bUtxo2)],
                                  [(BRedeemer2, bUtxo2)],
                                  [(BRedeemer1, bUtxo1), (BRedeemer1, bUtxo2)],
                                  [(BRedeemer1, bUtxo1), (BRedeemer2, bUtxo2)]
                                ]
                          ]
                  ]
          ]
