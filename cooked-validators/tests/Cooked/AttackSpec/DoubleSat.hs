{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.AttackSpec.DoubleSat (tests) where

import Control.Arrow
import Cooked.Attack
import Cooked.AttackSpec.Util
import Cooked.MockChain
import Cooked.TestUtils
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Optics
import Data.Default
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
      validateTxSkel $ txSkel [PaysScript aValidator ADatum (L.lovelaceValueOf 2_000_000)]
      validateTxSkel $ txSkel [PaysScript aValidator ADatum (L.lovelaceValueOf 3_000_000)]
      validateTxSkel $ txSkel [PaysScript aValidator ADatum (L.lovelaceValueOf 4_000_000)]
      validateTxSkel $ txSkel [PaysScript aValidator ADatum (L.lovelaceValueOf 5_000_000)]
      validateTxSkel $ txSkel [PaysScript bValidator BDatum (L.lovelaceValueOf 6_000_000)]
      validateTxSkel $ txSkel [PaysScript bValidator BDatum (L.lovelaceValueOf 7_000_000)]
      validateTxSkel $ txSkel [PaysScript bValidator BDatum (L.lovelaceValueOf 8_000_000)]

tests :: TestTree
tests =
  testGroup
    "double satisfaction attack"
    $ let [[aUtxo1], [aUtxo2], [aUtxo3], [aUtxo4]] =
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
          [[bUtxo1], [bUtxo2], [bUtxo3]] =
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
            testCase "unit test on a 'TxSkel'" $
              let skelIn aUtxos =
                    txSkel $
                      map (SpendsScript aValidator ARedeemer) aUtxos
                        :=>: [paysPK (walletPKHash (wallet 2)) (L.lovelaceValueOf 2_500_000)]
                  skelOut aUtxos =
                    getAttack
                      ( doubleSatAttack
                          (spendsScriptConstraintsT % spendsScriptConstraintTypeP @AContract)
                          ( \mcst (_, _, (aOut, _)) ->
                              let bUtxos = scriptUtxosSuchThatMcst mcst bValidator (\_ _ -> True)
                                  aValue = sOutValue aOut
                               in if
                                      | aValue == L.lovelaceValueOf 2_000_000 ->
                                        [ toConstraints $ SpendsScript bValidator BRedeemer1 (bOut, bDatum)
                                          | (bOut, bDatum) <- bUtxos,
                                            sOutValue bOut == L.lovelaceValueOf 123 -- not satisfied by any UTxO in 'dsTestMockChain'
                                        ]
                                      | aValue == L.lovelaceValueOf 3_000_000 ->
                                        [ toConstraints $ SpendsScript bValidator BRedeemer1 (bOut, bDatum)
                                          | (bOut, bDatum) <- bUtxos,
                                            sOutValue bOut == L.lovelaceValueOf 6_000_000 -- satisfied by exactly one UTxO in 'dsTestMockChain'
                                        ]
                                      | aValue == L.lovelaceValueOf 4_000_000 ->
                                        concatMap
                                          ( \(bOut, bDatum) ->
                                              let bValue = sOutValue bOut
                                               in if
                                                      | bValue == L.lovelaceValueOf 6_000_000 ->
                                                        [toConstraints $ SpendsScript bValidator BRedeemer1 (bOut, bDatum)]
                                                      | bValue == L.lovelaceValueOf 7_000_000 ->
                                                        [ toConstraints $ SpendsScript bValidator BRedeemer1 (bOut, bDatum),
                                                          toConstraints $ SpendsScript bValidator BRedeemer2 (bOut, bDatum)
                                                        ]
                                                      | otherwise -> []
                                          )
                                          bUtxos
                                      | otherwise -> []
                          )
                          (wallet 6)
                          AllSeparate
                      )
                      dsTestMockChainSt
                      (skelIn aUtxos)
                  skelExpected aUtxos bUtxos =
                    map
                      ( \((bOut, bDat), bRed) ->
                          txSkelLbl DoubleSatLbl $
                            ( SpendsScript bValidator bRed (bOut, bDat) :
                              map (SpendsScript aValidator ARedeemer) aUtxos
                            )
                              :=>: [ paysPK (walletPKHash (wallet 2)) (L.lovelaceValueOf 2_500_000),
                                     paysPK (walletPKHash (wallet 6)) (sOutValue bOut)
                                   ]
                      )
                      bUtxos
               in testConjoin $
                    map
                      (\(aUtxos, bUtxos) -> assertSameTxSkels (skelExpected aUtxos bUtxos) (fst <$> skelOut aUtxos))
                      [ ([aUtxo1], []),
                        ([aUtxo2], [(bUtxo1, BRedeemer1)]),
                        ([aUtxo3], [(bUtxo1, BRedeemer1), (bUtxo2, BRedeemer1), (bUtxo2, BRedeemer2)]),
                        ([aUtxo4], []),
                        ([aUtxo1, aUtxo4], []),
                        ([aUtxo4, aUtxo1], []),
                        ([aUtxo1, aUtxo2], [(bUtxo1, BRedeemer1)]),
                        ([aUtxo2, aUtxo4], [(bUtxo1, BRedeemer1)]),
                        ( [aUtxo2, aUtxo3],
                          [(bUtxo1, BRedeemer1), (bUtxo1, BRedeemer1), (bUtxo2, BRedeemer1), (bUtxo2, BRedeemer2)]
                        ),
                        ( [aUtxo1, aUtxo2, aUtxo3, aUtxo4],
                          [(bUtxo1, BRedeemer1), (bUtxo1, BRedeemer1), (bUtxo2, BRedeemer1), (bUtxo2, BRedeemer2)]
                        )
                      ]
          ]
