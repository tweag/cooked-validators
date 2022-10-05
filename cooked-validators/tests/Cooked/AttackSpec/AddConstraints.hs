{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.AttackSpec.AddConstraints (tests) where

import Cooked.Attack.AddConstraints
import Cooked.Attack.Common
import Cooked.MockChain
import Cooked.TestUtils
import Cooked.Tx.Constraints
import Data.Default
import qualified Ledger.Typed.Scripts as L
import qualified Plutus.V1.Ledger.Ada as L
import qualified Plutus.V1.Ledger.Contexts as L
import qualified Plutus.V1.Ledger.Interval as Pl
import qualified PlutusTx as Pl
import qualified PlutusTx.Prelude as Pl
import Test.Tasty
import Test.Tasty.HUnit

data Redeemer = Redeemer1 | Redeemer2 deriving (Show)

instance Pl.Eq Redeemer where
  Redeemer1 == Redeemer1 = True
  Redeemer2 == Redeemer2 = True
  _ == _ = False

Pl.makeLift ''Redeemer
Pl.unstableMakeIsData ''Redeemer

data Contract

instance L.ValidatorTypes Contract where
  type DatumType Contract = ()
  type RedeemerType Contract = Redeemer

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> Redeemer -> L.ScriptContext -> Bool
mkValidator _ _ _ = True

validator :: L.TypedValidator Contract
validator =
  L.mkTypedValidator @Contract
    $$(Pl.compile [||mkValidator||])
    $$(Pl.compile [||wrap||])
  where
    wrap = L.mkUntypedValidator @() @Redeemer

-- | In the initial state of the Mockchain, validator owns a few UTxOs, with
-- different values
testMockChainSt :: MockChainSt
testMockChainSt = case runMockChainRaw def def setup of
  Left _ -> def -- this branch will not be reached
  Right (_, mcst) -> mcst
  where
    setup = do
      validateTxSkel $ txSkel [PaysScript validator () (L.lovelaceValueOf 2_000_000)]
      validateTxSkel $ txSkel [PaysScript validator () (L.lovelaceValueOf 3_000_000)]

assertTxSameConstraints :: TxSkel -> TxSkel -> Assertion
assertTxSameConstraints (TxSkel _ _ actual) (TxSkel _ _ expected) =
  assertSameConstraints actual expected

tests :: TestTree
tests =
  testGroup
    "adding and removing constraints"
    [ testGroup
        "addConstraintsAttack"
        [ testCase "'OutConstraints' are always added" $
            let oneOut = toConstraints $ paysPK (walletPKHash $ wallet 1) $ L.lovelaceValueOf 123
             in case getAttack (addConstraintsAttack oneOut) def (txSkel oneOut) of
                  [(skelOut, x)] -> assertTxSameConstraints skelOut (txSkel $ oneOut <> oneOut) .&&. (x @?= oneOut)
                  _ -> assertFailure "not the right number of attack outputs",
          testCase "time constraints are simplified" $
            let overlyComplicated = toConstraints [Before 10_000, After 1_000]
                interval = toConstraints $ ValidateIn $ Pl.interval 5_000 20_000
                combined = toConstraints $ ValidateIn $ Pl.interval 5_000 10_000
             in case getAttack (addConstraintsAttack interval) def (txSkel overlyComplicated) of
                  [(skelOut, x)] -> (skelOut @?= txSkel combined) .&&. (x @?= combined)
                  _ -> assertFailure "not the right number of attack outputs",
          testCase "conflicting 'SpendsScript'" $
            let utxo1 : _ = scriptUtxosSuchThatMcst testMockChainSt validator (\_ _ -> True)
                c1 = toConstraints $ SpendsScript validator Redeemer1 utxo1
                c2 =
                  [SpendsScript validator Redeemer2 utxo1]
                    :=>: [paysPK (walletPKHash $ wallet 6) $ sOutValue $ fst utxo1]
             in getAttack (addConstraintsAttack c2) def (txSkel c1) @?= [],
          testCase "non-conflicting 'SpendsScript', which is already present" $
            let utxo1 : _ = scriptUtxosSuchThatMcst testMockChainSt validator (\_ _ -> True)
                c1 = toConstraints $ SpendsScript validator Redeemer1 utxo1
                c2 =
                  [SpendsScript validator Redeemer1 utxo1]
                    :=>: [paysPK (walletPKHash $ wallet 6) $ sOutValue $ fst utxo1]
             in case getAttack (addConstraintsAttack c2) def (txSkel c1) of
                  [(skelOut, x)] ->
                    assertTxSameConstraints skelOut (txSkel c2)
                      .&&. (x @?= toConstraints (paysPK (walletPKHash $ wallet 6) $ sOutValue $ fst utxo1))
                  _ -> assertFailure "not the right number of attack outputs",
          testCase "non-conflicting 'SpendsScript', which is new" $
            let utxo1 : utxo2 : _ = scriptUtxosSuchThatMcst testMockChainSt validator (\_ _ -> True)
                c1 =
                  [SpendsScript validator Redeemer1 utxo1]
                    :=>: [paysPK (walletPKHash $ wallet 1) $ sOutValue $ fst utxo1]
                c2 =
                  [SpendsScript validator Redeemer1 utxo2]
                    :=>: [paysPK (walletPKHash $ wallet 6) $ sOutValue $ fst utxo2]
             in case getAttack (addConstraintsAttack c2) def (txSkel c1) of
                  [(skelOut, x)] ->
                    assertTxSameConstraints skelOut (txSkel $ c1 <> c2)
                      .&&. (x @?= c2)
                  _ -> assertFailure "not the right number of attack outputs"
        ]
    ]
