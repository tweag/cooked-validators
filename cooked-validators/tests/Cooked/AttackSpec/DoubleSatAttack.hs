{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.AttackSpec.DoubleSatAttack (tests) where

import Control.Arrow
import Cooked.Attack
import Cooked.AttackSpec.Common
import Cooked.MockChain
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Optics
import Data.Default
import qualified Ledger.Ada as L
import qualified Ledger.Contexts as L
import qualified Ledger.Typed.Scripts as L
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
    wrap = L.wrapValidator @ADatum @ARedeemer

data BDatum = BDatum1 | BDatum2 deriving (Show)

instance Pl.Eq BDatum where
  BDatum1 == BDatum1 = True
  BDatum2 == BDatum2 = True
  _ == _ = False

Pl.makeLift ''BDatum
Pl.unstableMakeIsData ''BDatum

data BRedeemer = BRedeemer deriving (Show)

instance Pl.Eq BRedeemer where
  BRedeemer == BRedeemer = True

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
    wrap = L.wrapValidator @BDatum @BRedeemer

-- | In the initial state of the Mockchain, the A validator owns two UTxOs, with
-- different values, and the B validator owns two UtxOs, distinguished by their
-- datum.
dsTestMockChainSt :: MockChainSt
dsTestMockChainSt = case runMockChainRaw def def setup of
  Left _ -> def -- this branch will not be reached
  Right (_, mcst) -> mcst
  where
    setup = do
      validateTxSkel $ txSkel [PaysScript aValidator ADatum (L.lovelaceValueOf 2_000_000)]
      validateTxSkel $ txSkel [PaysScript aValidator ADatum (L.lovelaceValueOf 3_000_000)]
      validateTxSkel $ txSkel [PaysScript bValidator BDatum1 (L.lovelaceValueOf 4_000_000)]
      validateTxSkel $ txSkel [PaysScript bValidator BDatum2 (L.lovelaceValueOf 5_000_000)]

tests :: TestTree
tests =
  testGroup
    "double satisfaction attack"
    [ testCase "unit test on a 'TxSkel'" $
        let params =
              DoubleSatParams
                { dsExtraInputOwner = bValidator,
                  dsExtraInputPred = \d _ -> d Pl.== BDatum1,
                  dsExtraInputRedeemer = BRedeemer,
                  dsSelectSpendsScript = \case
                    SpendsScriptConstraint aValidator _ (o, _) -> sOutValue o == L.lovelaceValueOf 2_000_000,
                  dsAttacker = wallet 6
                }
            [aOut1] =
              scriptUtxosSuchThatMcst
                dsTestMockChainSt
                aValidator
                (\_ v -> v == L.lovelaceValueOf 2_000_000)
            [aOut2] =
              scriptUtxosSuchThatMcst
                dsTestMockChainSt
                aValidator
                (\_ v -> v == L.lovelaceValueOf 3_000_000)
            [bOut1] =
              scriptUtxosSuchThatMcst
                dsTestMockChainSt
                bValidator
                (\d _ -> d Pl.== BDatum1)
            skelIn aOuts =
              txSkel $
                map (SpendsScript aValidator ARedeemer) aOuts
                  :=>: [paysPK (walletPKHash (wallet 2)) (L.lovelaceValueOf 2_500_000)]
            skelOut aOuts = doubleSatAttack params dsTestMockChainSt $ skelIn aOuts
            skelExpected aOuts =
              if containsAOut1 aOuts
                then
                  Just $
                    txSkelLbl DoubleSatLbl $
                      ( SpendsScript bValidator BRedeemer bOut1 :
                        map (SpendsScript aValidator ARedeemer) aOuts
                      )
                        :=>: [ paysPK (walletPKHash (wallet 2)) (L.lovelaceValueOf 2_500_000),
                               paysPK (walletPKHash (wallet 6)) (L.lovelaceValueOf 4_000_000)
                             ]
                else Nothing
            containsAOut1 aOuts = any (\(o, _) -> sOutValue o == L.lovelaceValueOf 2_000_000) aOuts
         in testConjoin $
              map
                (\aOuts -> assertTxSkelEqual (skelExpected aOuts) (skelOut aOuts))
                [ [aOut1],
                  [aOut2],
                  [aOut1, aOut2],
                  [aOut2, aOut1]
                ]
    ]
