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
import Cooked
import Cooked.MockChain.Staged
import Cooked.TestUtils
import Data.Default
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
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

data ARedeemer = ARedeemer deriving (Show)

instance Pl.Eq ARedeemer where
  ARedeemer == ARedeemer = True

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
    $ let Right ([aUtxo1], _) = runMockChainRaw def dsTestMockChainSt $ runUtxoSearch $ allUtxosSearch `filterWithPred` ((== Pl.lovelaceValueOf 2_000_000) . outputValue)
          Right ([aUtxo2], _) = runMockChainRaw def dsTestMockChainSt $ runUtxoSearch $ allUtxosSearch `filterWithPred` ((== Pl.lovelaceValueOf 3_000_000) . outputValue)
          Right ([aUtxo3], _) = runMockChainRaw def dsTestMockChainSt $ runUtxoSearch $ allUtxosSearch `filterWithPred` ((== Pl.lovelaceValueOf 4_000_000) . outputValue)
          Right ([aUtxo4], _) = runMockChainRaw def dsTestMockChainSt $ runUtxoSearch $ allUtxosSearch `filterWithPred` ((== Pl.lovelaceValueOf 5_000_000) . outputValue)
          Right ([bUtxo1], _) = runMockChainRaw def dsTestMockChainSt $ runUtxoSearch $ allUtxosSearch `filterWithPred` ((== Pl.lovelaceValueOf 6_000_000) . outputValue)
          Right ([bUtxo2], _) = runMockChainRaw def dsTestMockChainSt $ runUtxoSearch $ allUtxosSearch `filterWithPred` ((== Pl.lovelaceValueOf 7_000_000) . outputValue)
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
                  skelIn :: [Pl.TxOutRef] -> TxSkel
                  skelIn aOrefs =
                    txSkelTemplate
                      { txSkelIns = Map.fromSet (const (TxSkelRedeemerForScript ARedeemer)) $ Set.fromList aOrefs,
                        txSkelOuts = [paysPK (walletPKHash (wallet 2)) (Pl.lovelaceValueOf 2_500_000)],
                        txSkelSigners = [wallet 1]
                      }

                  -- apply the 'doubleSatAttack' to the input skeleton to
                  -- generate a list of output skeleta. The multiway-if
                  -- statement is what decides which UTxOs belonging to the
                  -- 'bValidator' to add, depending on the focused input
                  -- 'aValidator' UTxO.
                  skelsOut :: DoubleSatSplitMode -> [Pl.TxOutRef] -> [TxSkel]
                  skelsOut splitMode aUtxos =
                    mapMaybe (\case Right (_, skel') -> Just skel'; _ -> Nothing) $
                      runTweakFrom
                        def
                        dsTestMockChainSt
                        ( doubleSatAttack
                            (txSkelInsL % to Map.keys % folded) -- We know that all of these TxOutRefs point to something that the 'aValidator' owns
                            ( \aOref -> do
                                bUtxos <- runUtxoSearch $ allUtxosSearch `filterWithPure` isScriptOutputFrom bValidator
                                Just aValue <- valueFromTxOutRef aOref
                                if
                                    | aValue == Pl.lovelaceValueOf 2_000_000 ->
                                      return
                                        [ toDelta bOref $ TxSkelRedeemerForScript BRedeemer1
                                          | (bOref, bOut) <- bUtxos,
                                            outputValue bOut == Pl.lovelaceValueOf 123 -- not satisfied by any UTxO in 'dsTestMockChain'
                                        ]
                                    | aValue == Pl.lovelaceValueOf 3_000_000 ->
                                      return
                                        [ toDelta bOref $ TxSkelRedeemerForScript BRedeemer1
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
                                                        [toDelta bOref $ TxSkelRedeemerForScript BRedeemer1]
                                                      | bValue == Pl.lovelaceValueOf 7_000_000 ->
                                                        [ toDelta bOref $ TxSkelRedeemerForScript BRedeemer1,
                                                          toDelta bOref $ TxSkelRedeemerForScript BRedeemer2
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

                  -- generate a transaction that spends the given 'aValidator'
                  -- UTxOs (all with 'ARedeemer') and the 'bValidator' UTxOs
                  -- with the specified redeemers.
                  skelExpected :: [Pl.TxOutRef] -> [(BRedeemer, (Pl.TxOutRef, Pl.TxOut))] -> TxSkel
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
                            <> Map.fromSet (const (TxSkelRedeemerForScript ARedeemer)) (Set.fromList aOrefs),
                        txSkelOuts =
                          [ paysPK (walletPKHash (wallet 2)) (Pl.lovelaceValueOf 2_500_000),
                            paysPK (walletPKHash (wallet 6)) (foldMap (outputValue . snd . snd) bUtxos)
                          ],
                        txSkelSigners = [wallet 1]
                      }
               in [ testGroup "with 'AllSeparate'" $
                      let thePredicate :: [(Pl.TxOutRef, Pl.TxOut)] -> [[(BRedeemer, (Pl.TxOutRef, Pl.TxOut))]] -> Assertion
                          thePredicate aUtxos bUtxos =
                            assertSameSets
                              (skelExpected (fst <$> aUtxos) <$> bUtxos)
                              (skelsOut AllSeparate $ fst <$> aUtxos)
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
                      let thePredicate :: [(Pl.TxOutRef, Pl.TxOut)] -> [[(BRedeemer, (Pl.TxOutRef, Pl.TxOut))]] -> Assertion
                          thePredicate aUtxos bUtxos =
                            assertSameSets
                              (skelExpected (fst <$> aUtxos) <$> bUtxos)
                              (skelsOut TryCombinations $ fst <$> aUtxos)
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
