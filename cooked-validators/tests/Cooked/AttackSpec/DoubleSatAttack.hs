module Cooked.AttackSpec.DoubleSatAttack (tests) where

import Cooked.Attack
import Cooked.AttackSpec.Common
import Cooked.MockChain
import Cooked.Tx.Constraints
import Test.Tasty
import Test.Tasty.HUnit

dsTestMcst :: MockChainSt
dsTestMcst = undefined

tests :: TestTree
tests =
  testGroup
    "double satisfaction attack"
    []

-- testCase "unit test on a 'TxSkel'" $
--   let params =
--         DoubleSatParams
--           { dsExtraInputOwner = carelessValidator,
--             dsExtraInputPred = \_ _ -> True,
--             dsExtraInputRedeemer = (),
--             dsSelectSpendsScript = const True,
--             dsAttacker = wallet 6
--           }
--       skelIn = txSkel [SpendsScript carefulValidator undefined undefined]
--       skelOut = doubleSatAttack params dsTestMcst skelIn
--       skelExpected = undefined
--    in assertTxSkelEqual (Just skelExpected) skelOut
