module Cooked.AttackSpec.Common where

import Cooked.Tx.Constraints
import Test.Tasty.HUnit

assertTxSkelEqual :: Maybe TxSkel -> Maybe TxSkel -> Assertion
assertTxSkelEqual expected actual =
  assertBool
    ( "non-equal 'TxSkel's:\nexpected:\n\n"
        ++ show (prettyTxSkel [] <$> expected)
        ++ "\n\nactual:\n\n"
        ++ show (prettyTxSkel [] <$> actual)
    )
    $ actual == expected
