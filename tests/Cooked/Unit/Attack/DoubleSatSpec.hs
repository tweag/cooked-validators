{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.Unit.Attack.DoubleSatSpec where

import Control.Arrow
import Cooked
import Cooked.MockChain.Staged
import Cooked.TestUtils
import qualified Cooked.Validators.Other as Validators
import Data.Default
import Data.List (subsequences)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Tuple (swap)
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
      validateTxSkel $ txSkelTemplate {txSkelSigners = [wallet 1], txSkelOuts = [paysScript Validators.yesThreeR () (Pl.lovelaceValueOf 2_000_000)]}
      validateTxSkel $ txSkelTemplate {txSkelSigners = [wallet 1], txSkelOuts = [paysScript Validators.yesThreeR () (Pl.lovelaceValueOf 3_000_000)]}
      validateTxSkel $ txSkelTemplate {txSkelSigners = [wallet 1], txSkelOuts = [paysScript Validators.yesThreeR () (Pl.lovelaceValueOf 4_000_000)]}
      validateTxSkel $ txSkelTemplate {txSkelSigners = [wallet 1], txSkelOuts = [paysScript Validators.yesThreeR () (Pl.lovelaceValueOf 5_000_000)]}
      validateTxSkel $ txSkelTemplate {txSkelSigners = [wallet 1], txSkelOuts = [paysScript Validators.yesBoolR () (Pl.lovelaceValueOf 6_000_000)]}
      validateTxSkel $ txSkelTemplate {txSkelSigners = [wallet 1], txSkelOuts = [paysScript Validators.yesBoolR () (Pl.lovelaceValueOf 7_000_000)]}

tests :: TestTree
tests =
  testGroup
    "double satisfaction attack"
    $ let Right ([aUtxo1, aUtxo2, aUtxo3, aUtxo4], _) =
            runMockChainRaw def dsTestMockChainSt $
              runUtxoSearch $
                utxosAtSearch (Pl.validatorAddress Validators.yesThreeR)
          Right ([bUtxo1, bUtxo2], _) =
            runMockChainRaw def dsTestMockChainSt $
              runUtxoSearch $
                utxosAtSearch (Pl.validatorAddress Validators.yesBoolR)
       in [ testCase "the two test validators have different addresses" $
              assertBool "no, the addresses are the same" $
                Pl.validatorAddress Validators.yesThreeR /= Pl.validatorAddress Validators.yesBoolR,
            testGroup "unit tests on a 'TxSkel'" $
              -- The following tests make sure that, depending on some
              -- 'TxSkelRedeemerForScript' constraints for UTxOs belonging to the
              -- 'Validators.yesThreeR on the input 'TxSkel', the correct additional
              -- 'TxSkelRedeemerForScript' constraints for UTxOs of the 'Validators.yesBoolR are on
              -- the output 'TxSkel's. Both 'DoubleSatSplitMode's are tested.
              let -- generate an input skeleton from some 'Validators.yesThreeR UTxOs to
                  -- be spent.
                  skelIn :: [(Validators.Gearbox, Pl.TxOutRef)] -> TxSkel
                  skelIn aInputs =
                    txSkelTemplate
                      { txSkelIns = Map.fromList $ map (second TxSkelRedeemerForScript . swap) aInputs,
                        txSkelOuts = [paysPK (walletPKHash (wallet 2)) (Pl.lovelaceValueOf 2_500_000)],
                        txSkelSigners = [wallet 1]
                      }

                  -- apply the 'doubleSatAttack' to the input skeleton to
                  -- generate a list of output skeleta. The multiway-if
                  -- statement is what decides which UTxOs belonging to the
                  -- 'Validators.yesBoolR to add, depending on the focused input
                  -- 'Validators.yesThreeR UTxO.
                  skelsOut :: ([Pl.TxOutRef] -> [[Pl.TxOutRef]]) -> [(Validators.Gearbox, Pl.TxOutRef)] -> [TxSkel]
                  skelsOut splitMode aInputs =
                    mapMaybe (\case Right (_, skel') -> Just skel'; _ -> Nothing) $
                      runTweakFrom
                        def
                        dsTestMockChainSt
                        ( doubleSatAttack
                            splitMode
                            (txSkelInsL % itraversed) -- we know that every 'TxOutRef' in the inputs points to a UTxO that the 'Validators.yesThreeR owns
                            ( \aOref _aRedeemer -> do
                                bUtxos <- runUtxoSearch $ allUtxosSearch `filterWithPure` isScriptOutputFrom Validators.yesBoolR
                                if
                                    | aOref == fst aUtxo1 ->
                                        return
                                          [ (TxSkelRedeemerForScript Validators.GbNeutral, toDelta bOref $ TxSkelRedeemerForScript True)
                                            | (bOref, bOut) <- bUtxos,
                                              outputValue bOut == Pl.lovelaceValueOf 123 -- not satisfied by any UTxO in 'dsTestMockChain'
                                          ]
                                    | aOref == fst aUtxo2 ->
                                        return
                                          [ (TxSkelRedeemerForScript Validators.GbNeutral, toDelta bOref $ TxSkelRedeemerForScript True)
                                            | (bOref, _) <- bUtxos,
                                              bOref == fst bUtxo1
                                          ]
                                    | aOref == fst aUtxo3 ->
                                        return $
                                          concatMap
                                            ( \(bOref, _) ->
                                                if
                                                    | bOref == fst bUtxo1 ->
                                                        [(TxSkelRedeemerForScript Validators.GbNeutral, toDelta bOref $ TxSkelRedeemerForScript True)]
                                                    | bOref == fst bUtxo2 ->
                                                        [ (TxSkelRedeemerForScript Validators.GbNeutral, toDelta bOref $ TxSkelRedeemerForScript True),
                                                          (TxSkelRedeemerForScript Validators.GbForward, toDelta bOref $ TxSkelRedeemerForScript False)
                                                        ]
                                                    | otherwise -> []
                                            )
                                            bUtxos
                                    | otherwise -> return []
                            )
                            (wallet 6)
                        )
                        (skelIn aInputs)
                    where
                      toDelta :: Pl.TxOutRef -> TxSkelRedeemer -> DoubleSatDelta
                      toDelta oref howSpent = (Map.singleton oref howSpent, [], mempty)

                  -- generate a transaction that spends the given
                  -- 'Validators.yesThreeR' UTxOs (all with
                  -- 'Validators.Gearbox') and the 'Validators.yesBoolR' UTxOs
                  -- with the specified redeemers, while redirecting the value
                  -- of the inputs from the 'Validators.yesBoolR' to wallet 6
                  skelExpected :: [(Validators.Gearbox, Pl.TxOutRef)] -> [(Bool, (Pl.TxOutRef, Pl.TxOut))] -> TxSkel
                  skelExpected aInputs bInputs =
                    txSkelTemplate
                      { txSkelLabel = Set.singleton $ TxLabel DoubleSatLbl,
                        txSkelIns =
                          Map.fromList
                            ( ( \(bRedeemer, (bOref, _)) ->
                                  (bOref, TxSkelRedeemerForScript bRedeemer)
                              )
                                <$> bInputs
                            )
                            <> Map.fromList
                              ( ( \(aRedeemer, aOref) ->
                                    (aOref, TxSkelRedeemerForScript aRedeemer)
                                )
                                  <$> aInputs
                              ),
                        txSkelOuts =
                          [ paysPK (walletPKHash (wallet 2)) (Pl.lovelaceValueOf 2_500_000),
                            paysPK (walletPKHash (wallet 6)) (foldMap (outputValue . snd . snd) bInputs)
                          ],
                        txSkelSigners = [wallet 1]
                      }
               in [ testGroup "with separate skeletons for each modification" $
                      let thePredicate ::
                            [(Validators.Gearbox, (Pl.TxOutRef, Pl.TxOut))] ->
                            [ ( [(Validators.Gearbox, (Pl.TxOutRef, Pl.TxOut))],
                                [(Bool, (Pl.TxOutRef, Pl.TxOut))]
                              )
                            ] ->
                            Assertion
                          thePredicate oldInputs newInputs =
                            assertSameSets
                              ( map
                                  ( \(newAInputs, newBInputs) ->
                                      skelExpected (second fst <$> newAInputs) newBInputs
                                  )
                                  newInputs
                              )
                              (skelsOut (map (: [])) $ second fst <$> oldInputs)
                       in [ testCase "no modified transactions if there's no suitable UTxO" $
                              thePredicate
                                [(Validators.GbRear, aUtxo1)]
                                [],
                            testCase "exactly one modified transaction if there's one suitable UTxO" $
                              thePredicate
                                [(Validators.GbRear, aUtxo2)]
                                [([(Validators.GbNeutral, aUtxo2)], [(True, bUtxo1)])],
                            testCase "three modified transactions from 1+2 redeemers" $
                              thePredicate
                                [(Validators.GbRear, aUtxo3)]
                                [ ([(Validators.GbNeutral, aUtxo3)], [(True, bUtxo1)]),
                                  ([(Validators.GbNeutral, aUtxo3)], [(True, bUtxo2)]),
                                  ([(Validators.GbForward, aUtxo3)], [(False, bUtxo2)])
                                ],
                            testCase "no modified transactions if no redeemer is specified" $
                              thePredicate
                                [(Validators.GbRear, aUtxo4)]
                                [],
                            testCase "with two foci, the correct combinations are returned" $
                              testConjoin $
                                map
                                  (uncurry thePredicate)
                                  [ ( [(Validators.GbRear, aUtxo1), (Validators.GbRear, aUtxo4)],
                                      []
                                    ),
                                    ( [(Validators.GbRear, aUtxo4), (Validators.GbRear, aUtxo1)],
                                      []
                                    ),
                                    ( [(Validators.GbRear, aUtxo1), (Validators.GbRear, aUtxo2)],
                                      [([(Validators.GbRear, aUtxo1), (Validators.GbNeutral, aUtxo2)], [(True, bUtxo1)])]
                                    ),
                                    ( [(Validators.GbRear, aUtxo4), (Validators.GbRear, aUtxo2)],
                                      [([(Validators.GbRear, aUtxo4), (Validators.GbNeutral, aUtxo2)], [(True, bUtxo1)])]
                                    ),
                                    ( [(Validators.GbRear, aUtxo2), (Validators.GbRear, aUtxo3)],
                                      [ ([(Validators.GbNeutral, aUtxo2), (Validators.GbRear, aUtxo3)], [(True, bUtxo1)]),
                                        ([(Validators.GbRear, aUtxo2), (Validators.GbNeutral, aUtxo3)], [(True, bUtxo1)]),
                                        ([(Validators.GbRear, aUtxo2), (Validators.GbNeutral, aUtxo3)], [(True, bUtxo2)]),
                                        ([(Validators.GbRear, aUtxo2), (Validators.GbForward, aUtxo3)], [(False, bUtxo2)])
                                      ]
                                    )
                                  ],
                            testCase "with all possible foci, no additional transactions are generated" $
                              thePredicate
                                [(Validators.GbRear, aUtxo1), (Validators.GbRear, aUtxo2), (Validators.GbRear, aUtxo3), (Validators.GbRear, aUtxo4)]
                                [ ([(Validators.GbRear, aUtxo1), (Validators.GbRear, aUtxo4), (Validators.GbNeutral, aUtxo2), (Validators.GbRear, aUtxo3)], [(True, bUtxo1)]),
                                  ([(Validators.GbRear, aUtxo1), (Validators.GbRear, aUtxo4), (Validators.GbRear, aUtxo2), (Validators.GbNeutral, aUtxo3)], [(True, bUtxo1)]),
                                  ([(Validators.GbRear, aUtxo1), (Validators.GbRear, aUtxo4), (Validators.GbRear, aUtxo2), (Validators.GbNeutral, aUtxo3)], [(True, bUtxo2)]),
                                  ([(Validators.GbRear, aUtxo1), (Validators.GbRear, aUtxo4), (Validators.GbRear, aUtxo2), (Validators.GbForward, aUtxo3)], [(False, bUtxo2)])
                                ]
                          ],
                    testGroup "trying all combinations of modifications" $
                      let thePredicate ::
                            [(Validators.Gearbox, (Pl.TxOutRef, Pl.TxOut))] ->
                            [ ( [(Validators.Gearbox, (Pl.TxOutRef, Pl.TxOut))],
                                [(Bool, (Pl.TxOutRef, Pl.TxOut))]
                              )
                            ] ->
                            Assertion
                          thePredicate oldInputs newInputs =
                            assertSameSets
                              ( map
                                  ( \(newAInputs, newBInputs) ->
                                      skelExpected (second fst <$> newAInputs) newBInputs
                                  )
                                  newInputs
                              )
                              (skelsOut (tail . subsequences) $ second fst <$> oldInputs)
                       in [ testCase "no modified transactions if there's no suitable UTxO" $
                              thePredicate
                                [(Validators.GbRear, aUtxo1)]
                                [],
                            testCase "exactly one modified transaction if there's one suitable UTxO" $
                              thePredicate
                                [(Validators.GbRear, aUtxo2)]
                                [([(Validators.GbNeutral, aUtxo2)], [(True, bUtxo1)])],
                            testCase "three modified transactions from 1+2 redeemers" $
                              thePredicate
                                [(Validators.GbRear, aUtxo3)]
                                [ ([(Validators.GbNeutral, aUtxo3)], [(True, bUtxo1)]),
                                  ([(Validators.GbNeutral, aUtxo3)], [(True, bUtxo2)]),
                                  ([(Validators.GbForward, aUtxo3)], [(False, bUtxo2)])
                                ],
                            testCase "no modified transactions if no redeemer is specified" $
                              thePredicate
                                [(Validators.GbRear, aUtxo4)]
                                [],
                            testCase "with two foci, the correct combinations are returned" $
                              testConjoin $
                                map
                                  (uncurry thePredicate)
                                  [ ( [(Validators.GbRear, aUtxo1), (Validators.GbRear, aUtxo4)],
                                      []
                                    ),
                                    ( [(Validators.GbRear, aUtxo4), (Validators.GbRear, aUtxo1)],
                                      []
                                    ),
                                    ( [(Validators.GbRear, aUtxo1), (Validators.GbRear, aUtxo2)],
                                      [([(Validators.GbRear, aUtxo1), (Validators.GbNeutral, aUtxo2)], [(True, bUtxo1)])]
                                    ),
                                    ( [(Validators.GbRear, aUtxo4), (Validators.GbRear, aUtxo2)],
                                      [([(Validators.GbRear, aUtxo4), (Validators.GbNeutral, aUtxo2)], [(True, bUtxo1)])]
                                    ),
                                    ( [(Validators.GbRear, aUtxo2), (Validators.GbRear, aUtxo3)],
                                      [ -- one modified focus
                                        ([(Validators.GbNeutral, aUtxo2), (Validators.GbRear, aUtxo3)], [(True, bUtxo1)]),
                                        ([(Validators.GbRear, aUtxo2), (Validators.GbNeutral, aUtxo3)], [(True, bUtxo1)]),
                                        ([(Validators.GbRear, aUtxo2), (Validators.GbNeutral, aUtxo3)], [(True, bUtxo2)]),
                                        ([(Validators.GbRear, aUtxo2), (Validators.GbForward, aUtxo3)], [(False, bUtxo2)]),
                                        -- both foci modified
                                        ([(Validators.GbNeutral, aUtxo2), (Validators.GbNeutral, aUtxo3)], [(True, bUtxo1)]),
                                        ([(Validators.GbNeutral, aUtxo2), (Validators.GbNeutral, aUtxo3)], [(True, bUtxo1), (True, bUtxo2)]),
                                        ([(Validators.GbNeutral, aUtxo2), (Validators.GbForward, aUtxo3)], [(True, bUtxo1), (False, bUtxo2)])
                                      ]
                                    )
                                  ],
                            testCase "with all possible foci, no additional transactions are generated" $
                              thePredicate
                                [(Validators.GbRear, aUtxo1), (Validators.GbRear, aUtxo2), (Validators.GbRear, aUtxo3), (Validators.GbRear, aUtxo4)]
                                [ ([(Validators.GbRear, aUtxo1), (Validators.GbRear, aUtxo4), (Validators.GbNeutral, aUtxo2), (Validators.GbRear, aUtxo3)], [(True, bUtxo1)]),
                                  ([(Validators.GbRear, aUtxo1), (Validators.GbRear, aUtxo4), (Validators.GbRear, aUtxo2), (Validators.GbNeutral, aUtxo3)], [(True, bUtxo1)]),
                                  ([(Validators.GbRear, aUtxo1), (Validators.GbRear, aUtxo4), (Validators.GbRear, aUtxo2), (Validators.GbNeutral, aUtxo3)], [(True, bUtxo2)]),
                                  ([(Validators.GbRear, aUtxo1), (Validators.GbRear, aUtxo4), (Validators.GbRear, aUtxo2), (Validators.GbForward, aUtxo3)], [(False, bUtxo2)]),
                                  ([(Validators.GbRear, aUtxo1), (Validators.GbRear, aUtxo4), (Validators.GbNeutral, aUtxo2), (Validators.GbNeutral, aUtxo3)], [(True, bUtxo1)]),
                                  ([(Validators.GbRear, aUtxo1), (Validators.GbRear, aUtxo4), (Validators.GbNeutral, aUtxo2), (Validators.GbNeutral, aUtxo3)], [(True, bUtxo1), (True, bUtxo2)]),
                                  ([(Validators.GbRear, aUtxo1), (Validators.GbRear, aUtxo4), (Validators.GbNeutral, aUtxo2), (Validators.GbForward, aUtxo3)], [(True, bUtxo1), (False, bUtxo2)])
                                ]
                          ]
                  ]
          ]
