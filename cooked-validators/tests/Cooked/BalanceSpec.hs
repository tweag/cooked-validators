{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Cooked.BalanceSpec (spec) where

import Control.Monad.State
import Cooked.MockChain
import Cooked.Tx.Balance
import Cooked.Tx.Constraints
import qualified Data.Map as M
import Data.String
import qualified Ledger.Crypto as Pl
import qualified Ledger.Index as Pl
import qualified Plutus.V1.Ledger.Ada as Pl
import qualified Plutus.V1.Ledger.Crypto as Pl
import qualified Plutus.V1.Ledger.Tx as Pl
import Test.Hspec
import qualified Wallet.Emulator.Wallet as Pl

spec :: SpecWith ()
spec = do
  describe "spendValueFrom" $ do
    -- In a simple case, where one has only one output, it is simply to balance it.
    it "spends money from the output" $
      let txOut1 = outsOf 1 utxoIndex0
       in shouldBe
            (spendValueFrom (Pl.lovelaceValueOf 10_000) txOut1)
            (map fst txOut1, Pl.lovelaceValueOf 90_000)
    -- It is necessary to spend both outputs of w11 to gather 8_000 Adas.
    it "spends money from the outputs" $
      let Right (st, _) = tracePayWallet11
       in let txOut11 = outsOf 11 $ mcstIndex st
           in shouldBe
                (spendValueFrom (Pl.lovelaceValueOf 8_000) txOut11)
                (map fst txOut11, Pl.lovelaceValueOf 400)

outsOf :: Int -> Pl.UtxoIndex -> [(Pl.TxOutRef, Pl.TxOut)]
outsOf i utxoIndex =
  M.foldlWithKey
    ( \acc k tx ->
        case Pl.txOutPubKey tx of
          Nothing -> acc
          Just pk ->
            if pk == walletPKHash (wallet i)
              then (k, tx) : acc
              else acc
    )
    []
    (Pl.getIndex utxoIndex)

tracePayWallet11 :: Either MockChainError (MockChainSt, UtxoState)
tracePayWallet11 =
  runMockChain $ do
    validateTxFromSkeleton $
      TxSkel
        (wallet 1)
        [PaysPK (walletPKHash $ wallet 11) (Pl.lovelaceValueOf 4200)]
    validateTxFromSkeleton $
      TxSkel
        (wallet 3)
        [PaysPK (walletPKHash $ wallet 11) (Pl.lovelaceValueOf 4200)]
    MockChainT get
