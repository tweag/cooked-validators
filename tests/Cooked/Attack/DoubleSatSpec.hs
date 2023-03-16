{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.Attack.DoubleSatSpec where

import Control.Arrow
import Control.Monad
import Cooked
import Cooked.MockChain.Staged
import Cooked.TestUtils
import Data.Default
import Data.List (subsequences)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Tuple (swap)
import qualified Debug.Trace
import Ledger.Typed.Scripts
import Optics.Core
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Plutus.Script.Utils.Typed as Pl
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Pl
import qualified Plutus.V1.Ledger.Interval as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import qualified PlutusTx as Pl
import qualified PlutusTx.Eq as Pl
import qualified PlutusTx.Prelude as Pl
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

instance Pl.Eq ADatum where
  ADatum == ADatum = True

Pl.makeLift ''ADatum
Pl.unstableMakeIsData ''ADatum

data ARedeemer = ARedeemer1 | ARedeemer2 | ARedeemer3 deriving (Show)

instance Pl.Eq ARedeemer where
  ARedeemer1 == ARedeemer1 = True
  ARedeemer2 == ARedeemer2 = True
  ARedeemer3 == ARedeemer3 = True
  _ == _ = False

instance PrettyCooked ARedeemer where
  prettyCooked = viaShow

Pl.makeLift ''ARedeemer
Pl.unstableMakeIsData ''ARedeemer

data AContract

instance Pl.ValidatorTypes AContract where
  type DatumType AContract = ADatum
  type RedeemerType AContract = ARedeemer

{-# INLINEABLE mkAValidator #-}
mkAValidator :: ADatum -> ARedeemer -> Pl.ScriptContext -> Bool
mkAValidator _ _ _ = True

aValidator :: Pl.TypedValidator AContract
aValidator =
  Pl.mkTypedValidator @AContract
    $$(Pl.compile [||mkAValidator||])
    $$(Pl.compile [||wrap||])
  where
    wrap = Pl.mkUntypedValidator

data BDatum = BDatum deriving (Show)

instance PrettyCooked BDatum where
  prettyCooked = viaShow

instance Pl.Eq BDatum where
  BDatum == BDatum = True

Pl.makeLift ''BDatum
Pl.unstableMakeIsData ''BDatum

data BRedeemer = BRedeemer1 | BRedeemer2 deriving (Show)

instance PrettyCooked BRedeemer where
  prettyCooked = viaShow

instance Pl.Eq BRedeemer where
  BRedeemer1 == BRedeemer1 = True
  BRedeemer2 == BRedeemer2 = True
  _ == _ = False

Pl.makeLift ''BRedeemer
Pl.unstableMakeIsData ''BRedeemer

data BContract

instance Pl.ValidatorTypes BContract where
  type DatumType BContract = BDatum
  type RedeemerType BContract = BRedeemer

{-# INLINEABLE mkBValidator #-}
mkBValidator :: BDatum -> BRedeemer -> Pl.ScriptContext -> Bool
mkBValidator _ _ _ = True

bValidator :: Pl.TypedValidator BContract
bValidator =
  Pl.mkTypedValidator @BContract
    $$(Pl.compile [||mkBValidator||])
    $$(Pl.compile [||wrap||])
  where
    wrap = Pl.mkUntypedValidator

-- | In the initial state of the Mockchain, the A and B validators each own
-- a few UTxOs, with different values
dsTestMockChainSt :: MockChainSt
dsTestMockChainSt = case runMockChainRaw def def setup of
  Left _ -> def -- this branch will not be reached
  Right (_, mcst) -> mcst
  where
    setup = do
      validateTxSkel $ txSkelTemplate {txSkelSigners = [wallet 1], txSkelOuts = [paysScript aValidator ADatum (Pl.lovelaceValueOf 2_000_000)]}
      validateTxSkel $ txSkelTemplate {txSkelSigners = [wallet 1], txSkelOuts = [paysScript aValidator ADatum (Pl.lovelaceValueOf 3_000_000)]}
      validateTxSkel $ txSkelTemplate {txSkelSigners = [wallet 1], txSkelOuts = [paysScript aValidator ADatum (Pl.lovelaceValueOf 4_000_000)]}
      validateTxSkel $ txSkelTemplate {txSkelSigners = [wallet 1], txSkelOuts = [paysScript aValidator ADatum (Pl.lovelaceValueOf 5_000_000)]}
      validateTxSkel $ txSkelTemplate {txSkelSigners = [wallet 1], txSkelOuts = [paysScript bValidator BDatum (Pl.lovelaceValueOf 6_000_000)]}
      validateTxSkel $ txSkelTemplate {txSkelSigners = [wallet 1], txSkelOuts = [paysScript bValidator BDatum (Pl.lovelaceValueOf 7_000_000)]}

tests :: TestTree
tests =
  testGroup
    "double satisfaction attack"
    $ let Right ([aUtxo1, aUtxo2, aUtxo3, aUtxo4], _) =
            runMockChainRaw def dsTestMockChainSt $
              runUtxoSearch $
                utxosAtSearch (Pl.validatorAddress aValidator)
          Right ([bUtxo1, bUtxo2], _) =
            runMockChainRaw def dsTestMockChainSt $
              runUtxoSearch $
                utxosAtSearch (Pl.validatorAddress bValidator)
       in [ testCase "the two test validators have different addresses" $
              assertBool "no, the addresses are the same" $
                Pl.validatorAddress aValidator /= Pl.validatorAddress bValidator,
            testGroup "unit tests on a 'TxSkel'" $
              -- The following tests make sure that, depending on some
              -- 'TxSkelRedeemerForScript' constraints for UTxOs belonging to the
              -- 'aValidator' on the input 'TxSkel', the correct additional
              -- 'TxSkelRedeemerForScript' constraints for UTxOs of the 'bValidator' are on
              -- the output 'TxSkel's. Both 'DoubleSatSplitMode's are tested.
              let -- generate an input skeleton from some 'aValidator' UTxOs to
                  -- be spent.
                  skelIn :: [(ARedeemer, Pl.TxOutRef)] -> TxSkel
                  skelIn aInputs =
                    txSkelTemplate
                      { txSkelIns = Map.fromList $ map (second TxSkelRedeemerForScript . swap) aInputs,
                        txSkelOuts = [paysPK (walletPKHash (wallet 2)) (Pl.lovelaceValueOf 2_500_000)],
                        txSkelSigners = [wallet 1]
                      }

                  -- apply the 'doubleSatAttack' to the input skeleton to
                  -- generate a list of output skeleta. The multiway-if
                  -- statement is what decides which UTxOs belonging to the
                  -- 'bValidator' to add, depending on the focused input
                  -- 'aValidator' UTxO.
                  skelsOut :: ([Pl.TxOutRef] -> [[Pl.TxOutRef]]) -> [(ARedeemer, Pl.TxOutRef)] -> [TxSkel]
                  skelsOut splitMode aInputs =
                    mapMaybe (\case Right (_, skel') -> Just skel'; _ -> Nothing) $
                      runTweakFrom
                        def
                        dsTestMockChainSt
                        ( doubleSatAttack
                            splitMode
                            (txSkelInsL % itraversed) -- we know that every 'TxOutRef' in the inputs points to a UTxO that the 'aValidator' owns
                            ( \aOref _aRedeemer -> do
                                bUtxos <- runUtxoSearch $ allUtxosSearch `filterWithPure` isScriptOutputFrom bValidator
                                if
                                    | aOref == fst aUtxo1 ->
                                        return
                                          [ (TxSkelRedeemerForScript ARedeemer2, toDelta bOref $ TxSkelRedeemerForScript BRedeemer1)
                                            | (bOref, bOut) <- bUtxos,
                                              outputValue bOut == Pl.lovelaceValueOf 123 -- not satisfied by any UTxO in 'dsTestMockChain'
                                          ]
                                    | aOref == fst aUtxo2 ->
                                        return
                                          [ (TxSkelRedeemerForScript ARedeemer2, toDelta bOref $ TxSkelRedeemerForScript BRedeemer1)
                                            | (bOref, _) <- bUtxos,
                                              bOref == fst bUtxo1
                                          ]
                                    | aOref == fst aUtxo3 ->
                                        return $
                                          concatMap
                                            ( \(bOref, _) ->
                                                if
                                                    | bOref == fst bUtxo1 ->
                                                        [(TxSkelRedeemerForScript ARedeemer2, toDelta bOref $ TxSkelRedeemerForScript BRedeemer1)]
                                                    | bOref == fst bUtxo2 ->
                                                        [ (TxSkelRedeemerForScript ARedeemer2, toDelta bOref $ TxSkelRedeemerForScript BRedeemer1),
                                                          (TxSkelRedeemerForScript ARedeemer3, toDelta bOref $ TxSkelRedeemerForScript BRedeemer2)
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

                  -- generate a transaction that spends the given 'aValidator'
                  -- UTxOs (all with 'ARedeemer') and the 'bValidator' UTxOs
                  -- with the specified redeemers, while redirecting the value of the inputs from the 'bValidator' to wallet 6
                  skelExpected :: [(ARedeemer, Pl.TxOutRef)] -> [(BRedeemer, (Pl.TxOutRef, Pl.TxOut))] -> TxSkel
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
                            [(ARedeemer, (Pl.TxOutRef, Pl.TxOut))] ->
                            [ ( [(ARedeemer, (Pl.TxOutRef, Pl.TxOut))],
                                [(BRedeemer, (Pl.TxOutRef, Pl.TxOut))]
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
                            [(ARedeemer, (Pl.TxOutRef, Pl.TxOut))] ->
                            [ ( [(ARedeemer, (Pl.TxOutRef, Pl.TxOut))],
                                [(BRedeemer, (Pl.TxOutRef, Pl.TxOut))]
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
