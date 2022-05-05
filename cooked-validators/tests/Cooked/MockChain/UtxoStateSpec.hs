{-# LANGUAGE NumericUnderscores #-}

module Cooked.MockChain.UtxoStateSpec where

import Cooked.Currencies
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.UtxoState
import Cooked.MockChain.UtxoState.Testing
import Cooked.MockChain.Wallet
import qualified Ledger.Ada as Ada
import qualified PlutusTx.Numeric as Pl
import Test.Tasty
import Test.Tasty.HUnit

utxoStateFromID :: InitialDistribution -> UtxoState
utxoStateFromID = mcstToUtxoState . mockChainSt0From

stA0 :: UtxoState
stA0 = utxoStateFromID $ initialDistribution' [(wallet 3, [minAda <> quickValue "TOK_A" 42])]

stA :: UtxoState
stA =
  stA0
    <> utxoStateFromID
      ( distributionFromList
          [ (wallet 1, [minAda <> Ada.lovelaceValueOf 123_000]),
            (wallet 2, [minAda <> Ada.lovelaceValueOf 123_000])
          ]
      )

stB :: UtxoState
stB =
  utxoStateFromID $
    initialDistribution'
      [ (wallet 4, [minAda <> Ada.lovelaceValueOf 123_000]),
        (wallet 5, [minAda <> Ada.lovelaceValueOf 123_000]),
        (wallet 3, [minAda <> quickValue "TOK_A" 20]),
        (wallet 1, [minAda <> quickValue "TOK_A" 22]),
        (wallet 2, [minAda <> quickValue "TOK_B" 1])
      ]

tests :: [TestTree]
tests =
  [ testGroup
      "utxoStateDiff"
      [ testCase "utxoStateDiffSrc (utxoStateDiff a b) == a" $
          utxoStateDiffSrc (utxoStateDiff stA stB) @?= stA,
        testCase "utxoStateDiffTgt (utxoStateDiff a b) == b" $
          utxoStateDiffTgt (utxoStateDiff stA stB) @?= stB
      ],
    testGroup
      "utxoStateDiffTotal"
      [ testCase "utxoStateDiffTotal (utxoStateDiff a b) =~= b - a" $
          utxoStateDiffTotal (utxoStateDiff stA stB) @?= (minAda <> minAda <> quickValue "TOK_B" 1),
        testCase "utxoStateDiffTotal (utxoStateDiff b a) =~= a - b" $
          utxoStateDiffTotal (utxoStateDiff stB stA) @?= Pl.negate (minAda <> minAda <> quickValue "TOK_B" 1)
      ],
    testGroup
      "Relations"
      [ testCase "stA `equalModuloAda` stA0" $
          equalModuloAda stA stA0 @?= True,
        testCase "equalModuloAtMost (quickValue \"TOK_B\" 1) stA stB" $
          equivModuloAtMost (minAda <> minAda <> quickValue "TOK_B" 1) stA stB @?= True,
        testCase "not $ equalModuloAtMost (Ada.lovelaceValueOf 100) stA0 stA" $
          equivModuloAtMost (Ada.lovelaceValueOf 100) stA0 stA @?= False,
        testCase "equalModuloAtMost (Ada.lovelaceValueOf -246000) stA stA0" $
          equivModuloAtMost (Ada.lovelaceValueOf (-246000)) stA stA0 @?= True
      ]
  ]
