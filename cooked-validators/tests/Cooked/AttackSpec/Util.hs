module Cooked.AttackSpec.Util where

import Cooked.MockChain.Testing ((.&&.))
import Cooked.Tx.Constraints
import Test.Tasty.HUnit

assertSameTxSkels :: [TxSkel] -> [TxSkel] -> Assertion
assertSameTxSkels expected actual =
  -- assertBool
  --   ( "non-equal number of 'TxSkel's.:\nexpected: "
  --       ++ show (length expected)
  --       ++ "\nactual: "
  --       ++ show (length actual)
  --   )
  --   (length expected == length actual)
  --   .&&.
  assertBool
    ( "non-equal 'TxSkel' sets:\nexpected:\n\n"
        ++ show (prettyTxSkel [] <$> expected)
        ++ "\n\nactual:\n\n"
        ++ show (prettyTxSkel [] <$> actual)
    )
    (actual `sameSet` expected)
  where
    sameSet a b = subset a b && subset b a
    subset a b = all (`elem` b) a

instance Show TxSkel where
  show = show . prettyTxSkel []
