module Cooked.Attack.DoubleSatSpec where

import Control.Arrow
import Cooked
import Data.Default
import Data.List (subsequences)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Tuple (swap)
import Optics.Core
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Typed qualified as Script
import Plutus.Script.Utils.V3.Typed.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter
import Test.Tasty
import Test.Tasty.HUnit

-- * Mock contracts for the double satisfaction attack

-- Scenario: There are two validators, one of type A, one of type B. We want to
-- add an input belonging to the B validator to a transaction that spends from
-- the A validator.

data ADatum = ADatum deriving (Show)

instance PrettyCooked ADatum where
  prettyCooked = viaShow

instance PlutusTx.Eq ADatum where
  ADatum == ADatum = True

PlutusTx.makeLift ''ADatum
PlutusTx.unstableMakeIsData ''ADatum

data ARedeemer = ARedeemer1 | ARedeemer2 | ARedeemer3 deriving (Show)

instance PlutusTx.Eq ARedeemer where
  ARedeemer1 == ARedeemer1 = True
  ARedeemer2 == ARedeemer2 = True
  ARedeemer3 == ARedeemer3 = True
  _ == _ = False

instance PrettyCooked ARedeemer where
  prettyCooked = viaShow

PlutusTx.makeLift ''ARedeemer
PlutusTx.unstableMakeIsData ''ARedeemer

data AContract

instance Script.ValidatorTypes AContract where
  type DatumType AContract = ADatum
  type RedeemerType AContract = ARedeemer

{-# INLINEABLE mkAValidator #-}
mkAValidator :: ADatum -> ARedeemer -> Api.ScriptContext -> Bool
mkAValidator _ _ _ = True

aValidator :: Script.TypedValidator AContract
aValidator =
  Script.mkTypedValidator @AContract
    $$(PlutusTx.compile [||mkAValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Script.mkUntypedValidator

data BDatum = BDatum deriving (Show)

instance PrettyCooked BDatum where
  prettyCooked = viaShow

instance PlutusTx.Eq BDatum where
  BDatum == BDatum = True

PlutusTx.makeLift ''BDatum
PlutusTx.unstableMakeIsData ''BDatum

data BRedeemer = BRedeemer1 | BRedeemer2 deriving (Show)

instance PrettyCooked BRedeemer where
  prettyCooked = viaShow

instance PlutusTx.Eq BRedeemer where
  BRedeemer1 == BRedeemer1 = True
  BRedeemer2 == BRedeemer2 = True
  _ == _ = False

PlutusTx.makeLift ''BRedeemer
PlutusTx.unstableMakeIsData ''BRedeemer

data BContract

instance Script.ValidatorTypes BContract where
  type DatumType BContract = BDatum
  type RedeemerType BContract = BRedeemer

{-# INLINEABLE mkBValidator #-}
mkBValidator :: BDatum -> BRedeemer -> Api.ScriptContext -> Bool
mkBValidator _ _ _ = True

bValidator :: Script.TypedValidator BContract
bValidator =
  Script.mkTypedValidator @BContract
    $$(PlutusTx.compile [||mkBValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Script.mkUntypedValidator

-- | In the initial state of the Mockchain, the A and B validators
-- each own a few UTxOs, with different values
customInitDist :: InitialDistribution
customInitDist =
  def
    <> InitialDistribution (paysScript aValidator ADatum . ada <$> [2, 3, 4, 5]) True
    <> InitialDistribution (paysScript bValidator BDatum . ada <$> [6, 7]) True

-- | Utxos generated from the initial distribution
aUtxo1, aUtxo2, aUtxo3, aUtxo4, bUtxo1, bUtxo2 :: (Api.TxOutRef, Api.TxOut)
(aUtxo1, aUtxo2, aUtxo3, aUtxo4, bUtxo1, bUtxo2) =
  case runMockChainFrom customInitDist $ do
    [a1, a2, a3, a4] <- runUtxoSearch $ utxosAtSearch aValidator
    [b1, b2] <- runUtxoSearch $ utxosAtSearch bValidator
    return (a1, a2, a3, a4, b1, b2) of
    Left _ -> error "Initial distribution error"
    Right (a, _) -> a

tests :: TestTree
tests =
  testGroup
    "double satisfaction attack"
    [ testCase "the two test validators have different addresses" $
        assertBool "no, the addresses are the same" $
          toAddress aValidator /= toAddress bValidator,
      testGroup "unit tests on a 'TxSkel'" $
        -- The following tests make sure that, depending on some
        -- 'txSkelSomeRedeemer' constraints for UTxOs
        -- belonging to the 'aValidator' on the input 'TxSkel',
        -- the correct additional 'txSkelSomeRedeemer'
        -- constraints for UTxOs of the 'bValidator' are on the
        -- output 'TxSkel's. Both 'DoubleSatSplitMode's are
        -- tested.
        let -- generate an input skeleton from some 'aValidator'
            -- UTxOs to be spent.
            skelIn :: [(ARedeemer, Api.TxOutRef)] -> TxSkel
            skelIn aInputs =
              txSkelTemplate
                { txSkelIns = Map.fromList $ map (second txSkelSomeRedeemer . swap) aInputs,
                  txSkelOuts = [paysPK (wallet 2) (Script.lovelaceValueOf 2_500_000)],
                  txSkelSigners = [wallet 1]
                }

            -- apply the 'doubleSatAttack' to the input skeleton
            -- to generate a list of output skeleta. The
            -- multiway-if statement is what decides which UTxOs
            -- belonging to the 'bValidator' to add, depending
            -- on the focused input 'aValidator' UTxO.
            skelsOut :: ([Api.TxOutRef] -> [[Api.TxOutRef]]) -> [(ARedeemer, Api.TxOutRef)] -> [TxSkel]
            skelsOut splitMode aInputs =
              mapMaybe (\case Right (_, skel') -> Just skel'; _ -> Nothing) $
                runTweakFrom
                  (mockChainSt0From customInitDist)
                  ( doubleSatAttack
                      splitMode
                      (txSkelInsL % itraversed) -- we know that every 'TxOutRef' in the inputs points to a UTxO that the 'aValidator' owns
                      ( \aOref _aRedeemer -> do
                          bUtxos <- runUtxoSearch $ scriptOutputsSearch bValidator
                          if
                            | aOref == fst aUtxo1 ->
                                return
                                  [ (txSkelSomeRedeemer ARedeemer2, toDelta bOref $ txSkelSomeRedeemer BRedeemer1)
                                    | (bOref, bOut) <- bUtxos,
                                      outputValue bOut == Script.lovelaceValueOf 123 -- not satisfied by any UTxO in 'dsTestMockChain'
                                  ]
                            | aOref == fst aUtxo2 ->
                                return
                                  [ (txSkelSomeRedeemer ARedeemer2, toDelta bOref $ txSkelSomeRedeemer BRedeemer1)
                                    | (bOref, _) <- bUtxos,
                                      bOref == fst bUtxo1
                                  ]
                            | aOref == fst aUtxo3 ->
                                return $
                                  concatMap
                                    ( \(bOref, _) ->
                                        if
                                          | bOref == fst bUtxo1 ->
                                              [(txSkelSomeRedeemer ARedeemer2, toDelta bOref $ txSkelSomeRedeemer BRedeemer1)]
                                          | bOref == fst bUtxo2 ->
                                              [ (txSkelSomeRedeemer ARedeemer2, toDelta bOref $ txSkelSomeRedeemer BRedeemer1),
                                                (txSkelSomeRedeemer ARedeemer3, toDelta bOref $ txSkelSomeRedeemer BRedeemer2)
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
                toDelta :: Api.TxOutRef -> TxSkelRedeemer -> DoubleSatDelta
                toDelta oref howSpent = (Map.singleton oref howSpent, [], mempty)

            -- generate a transaction that spends the given
            -- 'aValidator' UTxOs (all with 'ARedeemer') and the
            -- 'bValidator' UTxOs with the specified redeemers,
            -- while redirecting the value of the inputs from
            -- the 'bValidator' to wallet 6
            skelExpected :: [(ARedeemer, Api.TxOutRef)] -> [(BRedeemer, (Api.TxOutRef, Api.TxOut))] -> TxSkel
            skelExpected aInputs bInputs =
              txSkelTemplate
                { txSkelLabel = Set.singleton $ TxLabel DoubleSatLbl,
                  txSkelIns =
                    Map.fromList
                      ( ( \(bRedeemer, (bOref, _)) ->
                            (bOref, txSkelSomeRedeemer bRedeemer)
                        )
                          <$> bInputs
                      )
                      <> Map.fromList
                        ( ( \(aRedeemer, aOref) ->
                              (aOref, txSkelSomeRedeemer aRedeemer)
                          )
                            <$> aInputs
                        ),
                  txSkelOuts =
                    [ paysPK (wallet 2) (Script.lovelaceValueOf 2_500_000),
                      paysPK (wallet 6) (foldMap (outputValue . snd . snd) bInputs)
                    ],
                  txSkelSigners = [wallet 1]
                }
         in [ testGroup "with separate skeletons for each modification" $
                let thePredicate ::
                      [(ARedeemer, (Api.TxOutRef, Api.TxOut))] ->
                      [ ( [(ARedeemer, (Api.TxOutRef, Api.TxOut))],
                          [(BRedeemer, (Api.TxOutRef, Api.TxOut))]
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
                      [(ARedeemer, (Api.TxOutRef, Api.TxOut))] ->
                      [ ( [(ARedeemer, (Api.TxOutRef, Api.TxOut))],
                          [(BRedeemer, (Api.TxOutRef, Api.TxOut))]
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
