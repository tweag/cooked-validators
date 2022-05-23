{-# LANGUAGE NumericUnderscores #-}

module Cooked.MockChain.Monad.StagedSpec (tests) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Writer.Strict
import Cooked.MockChain
import Cooked.MockChain.Ltl
import Cooked.Tx.Constraints
import Data.Default
import Data.Foldable
import Data.Maybe
import qualified Ledger.Ada as Pl
import Test.Tasty
import Test.Tasty.HUnit

smcEq :: (Show a, Eq a) => StagedMockChain a -> StagedMockChain a -> Assertion
smcEq a b = go a @?= go b
  where
    go = map fst . interpretAndRun

possibleTraces :: [StagedMockChain ()]
possibleTraces =
  [ return (),
    void $ validateTxConstr [paysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 4_200_000)],
    void $ do
      validateTxConstr [paysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 4_200_000)]
      validateTxConstr [paysPK (walletPKHash $ wallet 3) (Pl.lovelaceValueOf 4_200_000)]
  ]

tests :: [TestTree]
tests =
  [ testGroup
      "unit"
      [ testCase "somewhere (Just . f) >> a >> b >> c == [f a >> b >> c , a >> f b >> c , a >> b >> f c]" $
          let f (TxSkel lbl opts cs) =
                case toConstraints cs of
                  is :=>: os -> TxSkel lbl opts (is :=>: (paysPK (walletPKHash $ wallet 5) (Pl.lovelaceValueOf 10000000) : os))
              tr f g h = void $ do
                validateTxSkel $ f $ txSkel [paysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 4200000)]
                validateTxSkel $ g $ txSkel [paysPK (walletPKHash $ wallet 3) (Pl.lovelaceValueOf 4200000)]
                -- This return is here on purpose; it adds something that is not a ValidateTxSkel to
                -- the staged AST. Check https://github.com/tweag/plutus-libs/pull/110 for the
                -- bug that triggered these tests to be created.
                _ <- return ()
                validateTxSkel $ h $ txSkel [paysPK (walletPKHash $ wallet 4) (Pl.lovelaceValueOf 4200000)]
           in somewhere (Just . f) (tr id id id) `smcEq` (tr f id id <|> tr id f id <|> tr id id f),
        testCase "somewhere (\\case b -> b'; _ -> Nothing) >> a >> b >> c == [a >> b' >> c]" $
          let paysWallet3 [] = False
              paysWallet3 (PaysPKWithDatum tgt _ _ _ : xs) = tgt == walletPKHash (wallet 3) || paysWallet3 xs

              f (TxSkel lbl opts cs) =
                case toConstraints cs of
                  is :=>: os ->
                    if paysWallet3 os
                      then Just $ TxSkel lbl opts (is :=>: (paysPK (walletPKHash $ wallet 5) (Pl.lovelaceValueOf 10000000) : os))
                      else Nothing
              tr f g h = void $ do
                validateTxSkel $ f $ txSkel [paysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 4200000)]
                validateTxSkel $ g $ txSkel [paysPK (walletPKHash $ wallet 3) (Pl.lovelaceValueOf 4200000)]
                validateTxSkel $ h $ txSkel [paysPK (walletPKHash $ wallet 4) (Pl.lovelaceValueOf 4200000)]
           in somewhere f (tr id id id) `smcEq` tr id (fromJust . f) id
      ]
  ]
