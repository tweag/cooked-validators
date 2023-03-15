{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.Unit.Attack.DoubleSatSpec (tests) where

import Cooked
import Cooked.MockChain.Staged
import Cooked.TestUtils
import qualified Cooked.Validators.Other as Validators
import qualified Cooked.Validators.Unit as Validators
import Data.Default
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Optics.Core
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Plutus.Script.Utils.Typed as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import Test.Tasty
import Test.Tasty.HUnit

-- * Mock contracts for the double satisfaction attack

-- Scenario: There are two validators, one of type A, one of type B. We want to
-- add an input belonging to the B validator to a transaction that spends from
-- the A validator.

-- | In the initial state of the Mockchain, the A and B validators each own
-- a few UTxOs, with different values
dsTestMockChainSt :: MockChainSt
dsTestMockChainSt = case runMockChainRaw def def setup of
  Left _ -> def -- this branch will not be reached
  Right (_, mcst) -> mcst
  where
    setup = do
      validateTxSkel $ txSkelTemplate {txSkelSigners = [wallet 1], txSkelOuts = [paysScript Validators.yes () (Pl.lovelaceValueOf 2_000_000)]}
      validateTxSkel $ txSkelTemplate {txSkelSigners = [wallet 1], txSkelOuts = [paysScript Validators.yes () (Pl.lovelaceValueOf 3_000_000)]}
      validateTxSkel $ txSkelTemplate {txSkelSigners = [wallet 1], txSkelOuts = [paysScript Validators.yes () (Pl.lovelaceValueOf 4_000_000)]}
      validateTxSkel $ txSkelTemplate {txSkelSigners = [wallet 1], txSkelOuts = [paysScript Validators.yes () (Pl.lovelaceValueOf 5_000_000)]}
      validateTxSkel $ txSkelTemplate {txSkelSigners = [wallet 1], txSkelOuts = [paysScript Validators.yesBoolR () (Pl.lovelaceValueOf 6_000_000)]}
      validateTxSkel $ txSkelTemplate {txSkelSigners = [wallet 1], txSkelOuts = [paysScript Validators.yesBoolR () (Pl.lovelaceValueOf 7_000_000)]}

tests :: TestTree
tests =
  testGroup
    "double satisfaction attack"
    $ let Right ([aUtxo1], _) =
            runMockChainRaw def dsTestMockChainSt $
              runUtxoSearch $
                allUtxosSearch `filterWithPred` ((== Pl.lovelaceValueOf 2_000_000) . outputValue)
          Right ([aUtxo2], _) =
            runMockChainRaw def dsTestMockChainSt $
              runUtxoSearch $
                allUtxosSearch `filterWithPred` ((== Pl.lovelaceValueOf 3_000_000) . outputValue)
          Right ([aUtxo3], _) =
            runMockChainRaw def dsTestMockChainSt $
              runUtxoSearch $
                allUtxosSearch `filterWithPred` ((== Pl.lovelaceValueOf 4_000_000) . outputValue)
          Right ([aUtxo4], _) =
            runMockChainRaw def dsTestMockChainSt $
              runUtxoSearch $
                allUtxosSearch `filterWithPred` ((== Pl.lovelaceValueOf 5_000_000) . outputValue)
          Right ([bUtxo1], _) =
            runMockChainRaw def dsTestMockChainSt $
              runUtxoSearch $
                allUtxosSearch `filterWithPred` ((== Pl.lovelaceValueOf 6_000_000) . outputValue)
          Right ([bUtxo2], _) =
            runMockChainRaw def dsTestMockChainSt $
              runUtxoSearch $
                allUtxosSearch `filterWithPred` ((== Pl.lovelaceValueOf 7_000_000) . outputValue)
       in [ testCase "the two test validators have different addresses" $
              assertBool "no, the addresses are the same" $
                Pl.validatorAddress Validators.yes /= Pl.validatorAddress Validators.yesBoolR,
            testGroup "unit tests on a 'TxSkel'" $
              -- The following tests make sure that, depending on some
              -- 'TxSkelRedeemerForScript' constraints for UTxOs belonging to the
              -- 'Validators.yes' on the input 'TxSkel', the correct additional
              -- 'TxSkelRedeemerForScript' constraints for UTxOs of the 'Validators.yesBoolR' are on
              -- the output 'TxSkel's. Both 'DoubleSatSplitMode's are tested.
              let -- generate an input skeleton from some 'Validators.yes' UTxOs to
                  -- be spent.
                  skelIn :: [Pl.TxOutRef] -> TxSkel
                  skelIn aOrefs =
                    txSkelTemplate
                      { txSkelIns = Map.fromSet (const (TxSkelRedeemerForScript ())) $ Set.fromList aOrefs,
                        txSkelOuts = [paysPK (walletPKHash (wallet 2)) (Pl.lovelaceValueOf 2_500_000)],
                        txSkelSigners = [wallet 1]
                      }

                  -- apply the 'doubleSatAttack' to the input skeleton to
                  -- generate a list of output skeleta. The multiway-if
                  -- statement is what decides which UTxOs belonging to the
                  -- 'Validators.yesBoolR' to add, depending on the focused input
                  -- 'Validators.yes' UTxO.
                  skelsOut :: DoubleSatSplitMode -> [Pl.TxOutRef] -> [TxSkel]
                  skelsOut splitMode aUtxos =
                    mapMaybe (\case Right (_, skel') -> Just skel'; _ -> Nothing) $
                      runTweakFrom
                        def
                        dsTestMockChainSt
                        ( doubleSatAttack
                            (txSkelInsL % to Map.keys % folded) -- We know that all of these TxOutRefs point to something that the 'Validators.yes' owns
                            ( \aOref -> do
                                bUtxos <- runUtxoSearch $ allUtxosSearch `filterWithPure` isScriptOutputFrom Validators.yesBoolR
                                Just aValue <- valueFromTxOutRef aOref
                                if
                                    | aValue == Pl.lovelaceValueOf 2_000_000 ->
                                        return
                                          [ toDelta bOref $ TxSkelRedeemerForScript True
                                            | (bOref, bOut) <- bUtxos,
                                              outputValue bOut == Pl.lovelaceValueOf 123 -- not satisfied by any UTxO in 'dsTestMockChain'
                                          ]
                                    | aValue == Pl.lovelaceValueOf 3_000_000 ->
                                        return
                                          [ toDelta bOref $ TxSkelRedeemerForScript True
                                            | (bOref, bOut) <- bUtxos,
                                              outputValue bOut == Pl.lovelaceValueOf 6_000_000 -- satisfied by exactly one UTxO in 'dsTestMockChain'
                                          ]
                                    | aValue == Pl.lovelaceValueOf 4_000_000 ->
                                        return $
                                          concatMap
                                            ( \(bOref, bOut) ->
                                                let bValue = outputValue bOut
                                                 in if
                                                        | bValue == Pl.lovelaceValueOf 6_000_000 ->
                                                            [toDelta bOref $ TxSkelRedeemerForScript True]
                                                        | bValue == Pl.lovelaceValueOf 7_000_000 ->
                                                            [ toDelta bOref $ TxSkelRedeemerForScript True,
                                                              toDelta bOref $ TxSkelRedeemerForScript False
                                                            ]
                                                        | otherwise -> []
                                            )
                                            bUtxos
                                    | otherwise -> return []
                            )
                            (wallet 6)
                            splitMode
                        )
                        (skelIn aUtxos)
                    where
                      toDelta :: Pl.TxOutRef -> TxSkelRedeemer -> DoubleSatDelta
                      toDelta oref howSpent = (Map.singleton oref howSpent, [], mempty)

                  -- generate a transaction that spends the given 'Validators.yes'
                  -- UTxOs (all with 'ARedeemer') and the 'Validators.yesBoolR' UTxOs
                  -- with the specified redeemers.
                  skelExpected :: [Pl.TxOutRef] -> [(Bool, (Pl.TxOutRef, Pl.TxOut))] -> TxSkel
                  skelExpected aOrefs bUtxos =
                    txSkelTemplate
                      { txSkelLabel = Set.singleton $ TxLabel DoubleSatLbl,
                        txSkelIns =
                          Map.fromList
                            ( ( \(bRedeemer, (bOref, _)) ->
                                  (bOref, TxSkelRedeemerForScript bRedeemer)
                              )
                                <$> bUtxos
                            )
                            <> Map.fromSet (const (TxSkelRedeemerForScript ())) (Set.fromList aOrefs),
                        txSkelOuts =
                          [ paysPK (walletPKHash (wallet 2)) (Pl.lovelaceValueOf 2_500_000),
                            paysPK (walletPKHash (wallet 6)) (foldMap (outputValue . snd . snd) bUtxos)
                          ],
                        txSkelSigners = [wallet 1]
                      }
               in [ testGroup "with 'AllSeparate'" $
                      let thePredicate ::
                            [(Pl.TxOutRef, Pl.TxOut)] ->
                            [[(Bool, (Pl.TxOutRef, Pl.TxOut))]] ->
                            Assertion
                          thePredicate aUtxos bUtxos =
                            assertSameSets
                              (skelExpected (fst <$> aUtxos) <$> bUtxos)
                              (skelsOut AllSeparate $ fst <$> aUtxos)
                       in [ testCase "no modified transactions if there's no suitable UTxO" $ thePredicate [aUtxo1] [],
                            testCase "exactly one modified transaction if there's one suitable UTxO" $
                              thePredicate [aUtxo2] [[(True, bUtxo1)]],
                            testCase "three modified transactions from 1+2 redeemers" $
                              thePredicate [aUtxo3] [[(True, bUtxo1)], [(True, bUtxo2)], [(False, bUtxo2)]],
                            testCase "no modified transactions if no redeemer is specified" $ thePredicate [aUtxo4] [],
                            testCase "with two foci, the correct combinations are returned" $
                              testConjoin $
                                map
                                  (uncurry thePredicate)
                                  [ ([aUtxo1, aUtxo4], []),
                                    ([aUtxo4, aUtxo1], []),
                                    ([aUtxo1, aUtxo2], [[(True, bUtxo1)]]),
                                    ([aUtxo2, aUtxo4], [[(True, bUtxo1)]]),
                                    ( [aUtxo2, aUtxo3],
                                      [[(True, bUtxo1)], [(True, bUtxo2)], [(False, bUtxo2)]]
                                    )
                                  ],
                            testCase "with all possible foci, no additional transactions are generated" $
                              thePredicate
                                [aUtxo1, aUtxo2, aUtxo3, aUtxo4]
                                -- the same list as in the last example
                                [[(True, bUtxo1)], [(True, bUtxo2)], [(False, bUtxo2)]]
                          ],
                    testGroup "with 'TryCombinations'" $
                      let thePredicate :: [(Pl.TxOutRef, Pl.TxOut)] -> [[(Bool, (Pl.TxOutRef, Pl.TxOut))]] -> Assertion
                          thePredicate aUtxos bUtxos =
                            assertSameSets
                              (skelExpected (fst <$> aUtxos) <$> bUtxos)
                              (skelsOut TryCombinations $ fst <$> aUtxos)
                       in [ testCase "no modified transactions if there's no suitable UTxO" $ thePredicate [aUtxo1] [],
                            testCase "exactly one modified transaction if there's one suitable UTxO" $ thePredicate [aUtxo2] [[(True, bUtxo1)]],
                            testCase "three modified transactions from 1+2 redeemers" $
                              thePredicate [aUtxo3] [[(True, bUtxo1)], [(True, bUtxo2)], [(False, bUtxo2)]],
                            testCase "no modified transactions if no redeemer is specified" $ thePredicate [aUtxo4] [],
                            testCase "with two foci, the correct combinations are returned" $
                              testConjoin $
                                map
                                  (uncurry thePredicate)
                                  [ ([aUtxo1, aUtxo4], []),
                                    ([aUtxo4, aUtxo1], []),
                                    ([aUtxo1, aUtxo2], [[(True, bUtxo1)]]),
                                    ([aUtxo2, aUtxo4], [[(True, bUtxo1)]]),
                                    ( [aUtxo2, aUtxo3],
                                      [ -- one extra input
                                        [(True, bUtxo1)],
                                        [(True, bUtxo2)],
                                        [(False, bUtxo2)],
                                        -- two extra inputs
                                        [(True, bUtxo1), (True, bUtxo2)],
                                        [(True, bUtxo1), (False, bUtxo2)]
                                      ]
                                    )
                                  ],
                            testCase "with all possible foci, no additional transactions are generated" $
                              thePredicate
                                [aUtxo1, aUtxo2, aUtxo3, aUtxo4]
                                -- the same list  as in the last example
                                [ [(True, bUtxo1)],
                                  [(True, bUtxo2)],
                                  [(False, bUtxo2)],
                                  [(True, bUtxo1), (True, bUtxo2)],
                                  [(True, bUtxo1), (False, bUtxo2)]
                                ]
                          ]
                  ]
          ]
