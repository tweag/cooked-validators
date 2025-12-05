{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Attack.DoubleSat (tests) where

import Control.Arrow
import Cooked
import Data.Default
import Data.List (subsequences)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Tuple (swap)
import Optics.Core
import Plutus.Attack.DoubleSat
import Plutus.Script.Utils.V2 qualified as Script
import PlutusLedgerApi.V3 qualified as V3
import Prettyprinter
import Test.Tasty
import Test.Tasty.HUnit

instance PrettyCooked ADatum where
  prettyCooked = viaShow

instance PrettyCooked ARedeemer where
  prettyCooked = viaShow

instance PrettyCooked BDatum where
  prettyCooked = viaShow

instance PrettyCooked BRedeemer where
  prettyCooked = viaShow

-- | In the initial state of the Mockchain, the A and B validators
-- each own a few UTxOs, with different values
customInitDist :: InitialDistribution
customInitDist =
  def
    <> InitialDistribution ((\n -> aValidator `receives` (VisibleHashedDatum ADatum <&&> Value (Script.ada n))) <$> [2, 3, 4, 5])
    <> InitialDistribution ((\n -> bValidator `receives` (VisibleHashedDatum BDatum <&&> Value (Script.ada n))) <$> [6, 7])

-- | Utxos generated from the initial distribution
aUtxo1, aUtxo2, aUtxo3, aUtxo4, bUtxo1, bUtxo2 :: (V3.TxOutRef, TxSkelOut)
(aUtxo1, aUtxo2, aUtxo3, aUtxo4, bUtxo1, bUtxo2) =
  case mcrValue $ runMockChainFrom customInitDist $ do
    [a1, a2, a3, a4] <- runUtxoSearch $ utxosOwnedBySearch aValidator
    [b1, b2] <- runUtxoSearch $ utxosOwnedBySearch bValidator
    return (a1, a2, a3, a4, b1, b2) of
    Left _ -> error "Initial distribution error"
    Right a -> a

tests :: TestTree
tests =
  testGroup
    "double satisfaction attack"
    [ testCase "the two test validators have different addresses" $
        assertBool "no, the addresses are the same" $
          Script.toAddress aValidator /= Script.toAddress bValidator,
      testGroup "unit tests on a 'TxSkel'" $
        -- The following tests make sure that, depending on some
        -- 'someRedeemer' constraints for UTxOs
        -- belonging to the 'aValidator' on the input 'TxSkel',
        -- the correct additional 'someRedeemer'
        -- constraints for UTxOs of the 'bValidator' are on the
        -- output 'TxSkel's. Both 'DoubleSatSplitMode's are
        -- tested.
        let -- generate an input skeleton from some 'aValidator'
            -- UTxOs to be spent.
            skelIn :: [(ARedeemer, V3.TxOutRef)] -> TxSkel
            skelIn aInputs =
              txSkelTemplate
                { txSkelIns = Map.fromList $ map (second someTxSkelRedeemer . swap) aInputs,
                  txSkelOuts = [wallet 2 `receives` Value (Script.lovelace 2_500_000)],
                  txSkelSigners = [wallet 1]
                }

            -- apply the 'doubleSatAttack' to the input skeleton
            -- to generate a list of output skeleta. The
            -- multiway-if statement is what decides which UTxOs
            -- belonging to the 'bValidator' to add, depending
            -- on the focused input 'aValidator' UTxO.
            skelsOut :: ([V3.TxOutRef] -> [[V3.TxOutRef]]) -> [(ARedeemer, V3.TxOutRef)] -> [TxSkel]
            skelsOut splitMode aInputs =
              mapMaybe
                ((\case Right (_, skel') -> Just skel'; _ -> Nothing) . mcrValue)
                ( runTweakFrom
                    customInitDist
                    ( doubleSatAttack
                        splitMode
                        (txSkelInsL % itraversed) -- we know that every 'TxOutRef' in the inputs points to a UTxO that the 'aValidator' owns
                        ( \aOref _aRedeemer -> do
                            bUtxos <- runUtxoSearch $ utxosOwnedBySearch bValidator
                            if
                              | aOref == fst aUtxo1 ->
                                  return
                                    [ (someTxSkelRedeemer ARedeemer2, toDelta bOref $ someTxSkelRedeemer BRedeemer1)
                                    | (bOref, bOut) <- bUtxos,
                                      view txSkelOutValueL bOut == Script.lovelace 123 -- not satisfied by any UTxO in 'dsTestMockChain'
                                    ]
                              | aOref == fst aUtxo2 ->
                                  return
                                    [ (someTxSkelRedeemer ARedeemer2, toDelta bOref $ someTxSkelRedeemer BRedeemer1)
                                    | (bOref, _) <- bUtxos,
                                      bOref == fst bUtxo1
                                    ]
                              | aOref == fst aUtxo3 ->
                                  return $
                                    concatMap
                                      ( \(bOref, _) ->
                                          if
                                            | bOref == fst bUtxo1 ->
                                                [(someTxSkelRedeemer ARedeemer2, toDelta bOref $ someTxSkelRedeemer BRedeemer1)]
                                            | bOref == fst bUtxo2 ->
                                                [ (someTxSkelRedeemer ARedeemer2, toDelta bOref $ someTxSkelRedeemer BRedeemer1),
                                                  (someTxSkelRedeemer ARedeemer3, toDelta bOref $ someTxSkelRedeemer BRedeemer2)
                                                ]
                                            | otherwise -> []
                                      )
                                      bUtxos
                              | otherwise -> return []
                        )
                        (wallet 6)
                    )
                    (skelIn aInputs)
                )
              where
                toDelta :: V3.TxOutRef -> TxSkelRedeemer -> DoubleSatDelta
                toDelta oref howSpent = (Map.singleton oref howSpent, [], mempty)

            -- generate a transaction that spends the given
            -- 'aValidator' UTxOs (all with 'ARedeemer') and the
            -- 'bValidator' UTxOs with the specified redeemers,
            -- while redirecting the value of the inputs from
            -- the 'bValidator' to wallet 6
            skelExpected :: [(ARedeemer, V3.TxOutRef)] -> [(BRedeemer, (V3.TxOutRef, TxSkelOut))] -> TxSkel
            skelExpected aInputs bInputs =
              txSkelTemplate
                { txSkelLabel = Set.singleton $ TxSkelLabel DoubleSatLbl,
                  txSkelIns =
                    Map.fromList
                      ( ( \(bRedeemer, (bOref, _)) ->
                            (bOref, someTxSkelRedeemer bRedeemer)
                        )
                          <$> bInputs
                      )
                      <> Map.fromList
                        ( ( \(aRedeemer, aOref) ->
                              (aOref, someTxSkelRedeemer aRedeemer)
                          )
                            <$> aInputs
                        ),
                  txSkelOuts =
                    [ wallet 2 `receives` Value (Script.lovelace 2_500_000),
                      wallet 6 `receives` Value (foldMap (view txSkelOutValueL . snd . snd) bInputs)
                    ],
                  txSkelSigners = [wallet 1]
                }
         in [ testGroup "with separate skeletons for each modification" $
                let thePredicate ::
                      [(ARedeemer, (V3.TxOutRef, TxSkelOut))] ->
                      [ ( [(ARedeemer, (V3.TxOutRef, TxSkelOut))],
                          [(BRedeemer, (V3.TxOutRef, TxSkelOut))]
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
                          [(ARedeemer1, aUtxo1)]
                          [],
                      testCase "exactly one modified transaction if there's one suitable UTxO" $
                        thePredicate
                          [(ARedeemer1, aUtxo2)]
                          [([(ARedeemer2, aUtxo2)], [(BRedeemer1, bUtxo1)])],
                      testCase "three modified transactions from 1+2 redeemers" $
                        thePredicate
                          [(ARedeemer1, aUtxo3)]
                          [ ([(ARedeemer2, aUtxo3)], [(BRedeemer1, bUtxo1)]),
                            ([(ARedeemer2, aUtxo3)], [(BRedeemer1, bUtxo2)]),
                            ([(ARedeemer3, aUtxo3)], [(BRedeemer2, bUtxo2)])
                          ],
                      testCase "no modified transactions if no redeemer is specified" $
                        thePredicate
                          [(ARedeemer1, aUtxo4)]
                          [],
                      testCase "with two foci, the correct combinations are returned" $
                        testConjoin $
                          map
                            (uncurry thePredicate)
                            [ ( [(ARedeemer1, aUtxo1), (ARedeemer1, aUtxo4)],
                                []
                              ),
                              ( [(ARedeemer1, aUtxo4), (ARedeemer1, aUtxo1)],
                                []
                              ),
                              ( [(ARedeemer1, aUtxo1), (ARedeemer1, aUtxo2)],
                                [([(ARedeemer1, aUtxo1), (ARedeemer2, aUtxo2)], [(BRedeemer1, bUtxo1)])]
                              ),
                              ( [(ARedeemer1, aUtxo4), (ARedeemer1, aUtxo2)],
                                [([(ARedeemer1, aUtxo4), (ARedeemer2, aUtxo2)], [(BRedeemer1, bUtxo1)])]
                              ),
                              ( [(ARedeemer1, aUtxo2), (ARedeemer1, aUtxo3)],
                                [ ([(ARedeemer2, aUtxo2), (ARedeemer1, aUtxo3)], [(BRedeemer1, bUtxo1)]),
                                  ([(ARedeemer1, aUtxo2), (ARedeemer2, aUtxo3)], [(BRedeemer1, bUtxo1)]),
                                  ([(ARedeemer1, aUtxo2), (ARedeemer2, aUtxo3)], [(BRedeemer1, bUtxo2)]),
                                  ([(ARedeemer1, aUtxo2), (ARedeemer3, aUtxo3)], [(BRedeemer2, bUtxo2)])
                                ]
                              )
                            ],
                      testCase "with all possible foci, no additional transactions are generated" $
                        thePredicate
                          [(ARedeemer1, aUtxo1), (ARedeemer1, aUtxo2), (ARedeemer1, aUtxo3), (ARedeemer1, aUtxo4)]
                          [ ([(ARedeemer1, aUtxo1), (ARedeemer1, aUtxo4), (ARedeemer2, aUtxo2), (ARedeemer1, aUtxo3)], [(BRedeemer1, bUtxo1)]),
                            ([(ARedeemer1, aUtxo1), (ARedeemer1, aUtxo4), (ARedeemer1, aUtxo2), (ARedeemer2, aUtxo3)], [(BRedeemer1, bUtxo1)]),
                            ([(ARedeemer1, aUtxo1), (ARedeemer1, aUtxo4), (ARedeemer1, aUtxo2), (ARedeemer2, aUtxo3)], [(BRedeemer1, bUtxo2)]),
                            ([(ARedeemer1, aUtxo1), (ARedeemer1, aUtxo4), (ARedeemer1, aUtxo2), (ARedeemer3, aUtxo3)], [(BRedeemer2, bUtxo2)])
                          ]
                    ],
              testGroup "trying all combinations of modifications" $
                let thePredicate ::
                      [(ARedeemer, (V3.TxOutRef, TxSkelOut))] ->
                      [ ( [(ARedeemer, (V3.TxOutRef, TxSkelOut))],
                          [(BRedeemer, (V3.TxOutRef, TxSkelOut))]
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
                          [(ARedeemer1, aUtxo1)]
                          [],
                      testCase "exactly one modified transaction if there's one suitable UTxO" $
                        thePredicate
                          [(ARedeemer1, aUtxo2)]
                          [([(ARedeemer2, aUtxo2)], [(BRedeemer1, bUtxo1)])],
                      testCase "three modified transactions from 1+2 redeemers" $
                        thePredicate
                          [(ARedeemer1, aUtxo3)]
                          [ ([(ARedeemer2, aUtxo3)], [(BRedeemer1, bUtxo1)]),
                            ([(ARedeemer2, aUtxo3)], [(BRedeemer1, bUtxo2)]),
                            ([(ARedeemer3, aUtxo3)], [(BRedeemer2, bUtxo2)])
                          ],
                      testCase "no modified transactions if no redeemer is specified" $
                        thePredicate
                          [(ARedeemer1, aUtxo4)]
                          [],
                      testCase "with two foci, the correct combinations are returned" $
                        testConjoin $
                          map
                            (uncurry thePredicate)
                            [ ( [(ARedeemer1, aUtxo1), (ARedeemer1, aUtxo4)],
                                []
                              ),
                              ( [(ARedeemer1, aUtxo4), (ARedeemer1, aUtxo1)],
                                []
                              ),
                              ( [(ARedeemer1, aUtxo1), (ARedeemer1, aUtxo2)],
                                [([(ARedeemer1, aUtxo1), (ARedeemer2, aUtxo2)], [(BRedeemer1, bUtxo1)])]
                              ),
                              ( [(ARedeemer1, aUtxo4), (ARedeemer1, aUtxo2)],
                                [([(ARedeemer1, aUtxo4), (ARedeemer2, aUtxo2)], [(BRedeemer1, bUtxo1)])]
                              ),
                              ( [(ARedeemer1, aUtxo2), (ARedeemer1, aUtxo3)],
                                [ -- one modified focus
                                  ([(ARedeemer2, aUtxo2), (ARedeemer1, aUtxo3)], [(BRedeemer1, bUtxo1)]),
                                  ([(ARedeemer1, aUtxo2), (ARedeemer2, aUtxo3)], [(BRedeemer1, bUtxo1)]),
                                  ([(ARedeemer1, aUtxo2), (ARedeemer2, aUtxo3)], [(BRedeemer1, bUtxo2)]),
                                  ([(ARedeemer1, aUtxo2), (ARedeemer3, aUtxo3)], [(BRedeemer2, bUtxo2)]),
                                  -- both foci modified
                                  ([(ARedeemer2, aUtxo2), (ARedeemer2, aUtxo3)], [(BRedeemer1, bUtxo1)]),
                                  ([(ARedeemer2, aUtxo2), (ARedeemer2, aUtxo3)], [(BRedeemer1, bUtxo1), (BRedeemer1, bUtxo2)]),
                                  ([(ARedeemer2, aUtxo2), (ARedeemer3, aUtxo3)], [(BRedeemer1, bUtxo1), (BRedeemer2, bUtxo2)])
                                ]
                              )
                            ],
                      testCase "with all possible foci, no additional transactions are generated" $
                        thePredicate
                          [(ARedeemer1, aUtxo1), (ARedeemer1, aUtxo2), (ARedeemer1, aUtxo3), (ARedeemer1, aUtxo4)]
                          [ ([(ARedeemer1, aUtxo1), (ARedeemer1, aUtxo4), (ARedeemer2, aUtxo2), (ARedeemer1, aUtxo3)], [(BRedeemer1, bUtxo1)]),
                            ([(ARedeemer1, aUtxo1), (ARedeemer1, aUtxo4), (ARedeemer1, aUtxo2), (ARedeemer2, aUtxo3)], [(BRedeemer1, bUtxo1)]),
                            ([(ARedeemer1, aUtxo1), (ARedeemer1, aUtxo4), (ARedeemer1, aUtxo2), (ARedeemer2, aUtxo3)], [(BRedeemer1, bUtxo2)]),
                            ([(ARedeemer1, aUtxo1), (ARedeemer1, aUtxo4), (ARedeemer1, aUtxo2), (ARedeemer3, aUtxo3)], [(BRedeemer2, bUtxo2)]),
                            ([(ARedeemer1, aUtxo1), (ARedeemer1, aUtxo4), (ARedeemer2, aUtxo2), (ARedeemer2, aUtxo3)], [(BRedeemer1, bUtxo1)]),
                            ([(ARedeemer1, aUtxo1), (ARedeemer1, aUtxo4), (ARedeemer2, aUtxo2), (ARedeemer2, aUtxo3)], [(BRedeemer1, bUtxo1), (BRedeemer1, bUtxo2)]),
                            ([(ARedeemer1, aUtxo1), (ARedeemer1, aUtxo4), (ARedeemer2, aUtxo2), (ARedeemer3, aUtxo3)], [(BRedeemer1, bUtxo1), (BRedeemer2, bUtxo2)])
                          ]
                    ]
            ]
    ]
