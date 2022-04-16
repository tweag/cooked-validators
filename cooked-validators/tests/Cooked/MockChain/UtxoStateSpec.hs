{-# LANGUAGE NumericUnderscores #-}

module Cooked.MockChain.UtxoStateSpec where

import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.UtxoState
import Cooked.MockChain.UtxoState.Testing
import Cooked.MockChain.Wallet
import qualified Ledger.Ada as Ada
import qualified PlutusTx.Numeric as Pl
import Test.HUnit (Assertion)
import Test.Hspec

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

spec :: SpecWith ()
spec = do
  describe "utxoStateDiff Unit Tests" $ do
    it "utxoStateDiffSrc (utxoStateDiff a b) == a" $
      utxoStateDiffSrc (utxoStateDiff stA stB) `shouldBe` stA

    it "utxoStateDiffTgt (utxoStateDiff a b) == b" $
      utxoStateDiffTgt (utxoStateDiff stA stB) `shouldBe` stB

  describe "utxoStateDiffTotal" $ do
    it "utxoStateDiffTotal (utxoStateDiff a b) =~= b - a" $
      utxoStateDiffTotal (utxoStateDiff stA stB) `shouldBe` (minAda <> minAda <> quickValue "TOK_B" 1)

    it "utxoStateDiffTotal (utxoStateDiff b a) =~= a - b" $
      utxoStateDiffTotal (utxoStateDiff stB stA) `shouldBe` Pl.negate (minAda <> minAda <> quickValue "TOK_B" 1)

  describe "Relations" $ do
    it "stA `equalModuloAda` stA0" $
      equalModuloAda stA stA0 `shouldBe` True

    it "equalModuloAtMost (quickValue \"TOK_B\" 1) stA stB" $
      equivModuloAtMost (minAda <> minAda <> quickValue "TOK_B" 1) stA stB `shouldBe` True

    it "not $ equalModuloAtMost (Ada.lovelaceValueOf 100) stA0 stA" $
      equivModuloAtMost (Ada.lovelaceValueOf 100) stA0 stA `shouldBe` False

    it "equalModuloAtMost (Ada.lovelaceValueOf -246000) stA stA0" $
      equivModuloAtMost (Ada.lovelaceValueOf (-246000)) stA stA0 `shouldBe` True
