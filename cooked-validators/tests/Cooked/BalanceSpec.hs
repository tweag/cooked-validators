{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cooked.BalanceSpec (spec) where

import Control.Monad.Identity
import Control.Monad.State
import Cooked.MockChain
import Cooked.Tx.Balance
import Cooked.Tx.Constraints
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import Data.Kind
import Data.String
import qualified Ledger.Crypto as Pl
import qualified Ledger.Index as Pl
import qualified Plutus.V1.Ledger.Ada as Pl
import qualified Plutus.V1.Ledger.Crypto as Pl
import qualified Plutus.V1.Ledger.Tx as Pl
import qualified PlutusTx.AssocMap as Map
import qualified PlutusTx.Builtins.Internal as PlI
import Test.Hspec
import qualified Wallet.Emulator.Wallet as Pl
import qualified Ledger.Value as Pl
import Test.QuickCheck
import qualified Plutus.V1.Ledger.Api as Pl

newtype MockBalancable = MockBalancable { getMBValue :: Pl.Value }
  deriving (Eq, Show)

instance BalancableOut MockBalancable where
  type BOutRef MockBalancable = MockBalancable
  outValue = getMBValue

instance Arbitrary Pl.BuiltinByteString where
  arbitrary = PlI.BuiltinByteString . BS.pack . getASCIIString <$> arbitrary

instance Arbitrary Pl.CurrencySymbol where
  arbitrary = Pl.CurrencySymbol <$> arbitrary

instance Arbitrary Pl.TokenName where
  arbitrary = Pl.TokenName <$> arbitrary

instance (Arbitrary k, Arbitrary v) => Arbitrary (Map.Map k v) where
  arbitrary = Map.fromList <$> arbitrary


class ArbitraryMod (cntMod :: Type -> Type) where
  unwrap :: cntMod a -> a

instance ArbitraryMod Identity where
  unwrap = runIdentity

instance ArbitraryMod Positive where
  unwrap = getPositive

newtype ValueWithMods (cntMod :: Type -> Type) = ValueWithMods { vwmValue :: Pl.Value }
  deriving newtype (Eq, Show)

instance (ArbitraryMod cntMod, Arbitrary (cntMod Integer)) => Arbitrary (ValueWithMods cntMod) where
  arbitrary = do
    symsCount <- chooseInt (0, 5)
    syms <- fmap concat $ replicateM symsCount $ do
      sym <- arbitrary

      toksCount <- chooseInt (0, 5)
      symToks <- replicateM toksCount $ do
        tok <- arbitrary
        cnt <- unwrap @cntMod <$> arbitrary
        pure (tok, cnt)

      pure [Pl.singleton sym tok cnt | (tok, cnt) <- symToks]
    pure $ ValueWithMods $ mconcat syms

instance Arbitrary MockBalancable where
  arbitrary = MockBalancable . vwmValue @Identity <$> arbitrary


spec :: SpecWith ()
spec = do
  describe "spendValueFrom" $ do
    -- In a simple case, where one has only one output, it is simply to balance it.
    it "spends money from the output" $
      let txOut1 = outsOf 1 utxoIndex0
       in shouldBe
            (spendValueFrom (Pl.lovelaceValueOf 10_000) txOut1)
            (map fst txOut1, Pl.lovelaceValueOf 99_990_000)
    -- It is necessary to spend both outputs of w11 to gather 8 Adas (8_000_000 lovelaces).
    it "spends money from the outputs" $
      let Right (st, _) = tracePayWallet11
       in let txOut11 = outsOf 11 $ mcstIndex st
           in shouldBe
                (spendValueFrom (Pl.lovelaceValueOf 8_000_000) txOut11)
                (map fst txOut11, Pl.lovelaceValueOf 400_000)
    it "spends nothing if nothing to balance" $
      property $ \utxos -> do
        let (usedUtxos, leftovers) = spendValueFrom @MockBalancable mempty utxos
        usedUtxos `shouldBe` []
        leftovers `shouldBe` mempty

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
      txSkel
        (wallet 1)
        [PaysPK (walletPKHash $ wallet 11) (Pl.lovelaceValueOf 4_200_000)]
    validateTxFromSkeleton $
      txSkel
        (wallet 3)
        [PaysPK (walletPKHash $ wallet 11) (Pl.lovelaceValueOf 4_200_000)]
    MockChainT get
