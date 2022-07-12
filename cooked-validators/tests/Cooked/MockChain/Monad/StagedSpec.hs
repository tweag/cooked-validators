{-# LANGUAGE NumericUnderscores #-}

module Cooked.MockChain.Monad.StagedSpec (tests) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Writer.Strict
import Cooked.Ltl
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
import Data.Foldable
import Data.Maybe
import qualified Ledger.Ada as Pl
import Test.Tasty
import Test.Tasty.HUnit

assertSubset :: (Show a, Eq a) => [a] -> [a] -> Assertion
assertSubset l r =
  testConjoin
    ( map
        ( \x ->
            assertBool
              ( "not a subset:\n\n" ++ show x
                  ++ "\n\nis not an element of\n\n"
                  ++ show r
              )
              $ x `elem` r
        )
        l
    )

assertSameSets :: (Show a, Eq a) => [a] -> [a] -> Assertion
assertSameSets l r = assertSubset l r .&&. assertSubset r l

smcEq :: (Show a, Eq a) => StagedMockChain a -> StagedMockChain a -> Assertion
smcEq a b = go a `assertSameSets` go b
  where
    go = map fst . interpretAndRun

tests :: [TestTree]
tests =
  [ testGroup
      "unit"
      [ -- This test case is the regression test for two bug fixes, namely PRs
        -- 110 and 132.  The `as` on the second transaction is there for a very
        -- specific reason: It introduces a 'Return' in the middle of the AST,
        -- since `as` is defined in terms of `signingWith`, which in turn is
        -- reified as
        --
        -- > SigningWith :: NonEmpty Wallet -> StagedMockChain a -> MockChainBuiltin a
        --
        -- Now, in a term like `SigningWith ws smc`, the last instruction in
        -- `smc` has to be a `Return`. This means that we can not define the
        -- `Return` case of `interpLtl` like this
        --
        -- > interpLtl (Return a) = get >>= \xs -> if all finished xs then return a else mzero
        --
        -- (which was the case before we found this bug), as it will potentially
        -- prune branches prematurely.
        --
        -- Instead, we must now use the function `interpLtlAndPruneUnfinished`
        -- if we wish to prune all branches that have not completely applied all
        -- modifications at the end of the computation.
        testCase "(PR110, PR132) somewhere f (a >> b >> c) == [f a >> b >> c , a >> f b >> c , a >> b >> f c]" $
          let f (TxSkel lbl opts cs) =
                case toConstraints cs of
                  is :=>: os -> TxSkel lbl opts (is :=>: (paysPK (walletPKHash $ wallet 5) (Pl.lovelaceValueOf 10_000_000) : os))
              tr f g h = void $ do
                validateTxSkel $ f $ txSkel [paysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 4_200_000)]
                validateTxSkel (g $ txSkel [paysPK (walletPKHash $ wallet 3) (Pl.lovelaceValueOf 4_200_000)]) `as` wallet 2
                validateTxSkel $ h $ txSkel [paysPK (walletPKHash $ wallet 4) (Pl.lovelaceValueOf 4_200_000)]
           in somewhere (\_ sk -> [f sk]) (tr id id id) `smcEq` (tr f id id <|> tr id f id <|> tr id id f),
        testCase "somewhere (\\case b -> [b']; _ -> []) (a >> b >> c) == [a >> b' >> c]" $
          let paysWallet3 [] = False
              paysWallet3 (PaysPKWithDatum tgt _ _ _ : xs) = tgt == walletPKHash (wallet 3) || paysWallet3 xs

              f (TxSkel lbl opts cs) =
                case toConstraints cs of
                  is :=>: os ->
                    if paysWallet3 os
                      then [TxSkel lbl opts (is :=>: (paysPK (walletPKHash $ wallet 5) (Pl.lovelaceValueOf 10_000_000) : os))]
                      else []
              tr f g h = void $ do
                validateTxSkel $ f $ txSkel [paysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 4_200_000)]
                validateTxSkel $ g $ txSkel [paysPK (walletPKHash $ wallet 3) (Pl.lovelaceValueOf 4_200_000)]
                validateTxSkel $ h $ txSkel [paysPK (walletPKHash $ wallet 4) (Pl.lovelaceValueOf 4_200_000)]
           in somewhere (\_ sk -> f sk) (tr id id id) `smcEq` tr id (head . f) id,
        testCase "somewhere (g <> f) returns correct set of traces" $
          let tr =
                foldr
                  ( \wallets rest ->
                      validateTxSkel
                        ( txSkel
                            ( map
                                (\wal -> paysPK (walletPKHash wal) (Pl.lovelaceValueOf 4_200_000))
                                wallets
                            )
                        )
                        >> rest
                  )
                  (return ())
              g _ (TxSkel lbl opts cs) =
                case toConstraints cs of
                  is :=>: os ->
                    [ TxSkel lbl opts (is :=>: (paysPK (walletPKHash $ wallet 4) (Pl.lovelaceValueOf 4_200_000) : os)),
                      TxSkel lbl opts (is :=>: (paysPK (walletPKHash $ wallet 5) (Pl.lovelaceValueOf 4_200_000) : os))
                    ]

              f _ (TxSkel lbl opts cs) =
                case toConstraints cs of
                  is :=>: os ->
                    [ TxSkel lbl opts (is :=>: (paysPK (walletPKHash $ wallet 6) (Pl.lovelaceValueOf 4_200_000) : os)),
                      TxSkel lbl opts (is :=>: (paysPK (walletPKHash $ wallet 7) (Pl.lovelaceValueOf 4_200_000) : os))
                    ]
           in somewhere
                (g <> f)
                ( tr
                    [ [wallet 2],
                      [wallet 3]
                    ]
                )
                `smcEq` ( tr
                            [ [wallet 2],
                              [wallet 4, wallet 6, wallet 3]
                            ]
                            <|> tr
                              [ [wallet 2],
                                [wallet 5, wallet 6, wallet 3]
                              ]
                            <|> tr
                              [ [wallet 2],
                                [wallet 4, wallet 7, wallet 3]
                              ]
                            <|> tr
                              [ [wallet 2],
                                [wallet 5, wallet 7, wallet 3]
                              ]
                            <|> tr
                              [ [wallet 4, wallet 6, wallet 2],
                                [wallet 3]
                              ]
                            <|> tr
                              [ [wallet 5, wallet 6, wallet 2],
                                [wallet 3]
                              ]
                            <|> tr
                              [ [wallet 4, wallet 7, wallet 2],
                                [wallet 3]
                              ]
                            <|> tr
                              [ [wallet 5, wallet 7, wallet 2],
                                [wallet 3]
                              ]
                        ),
        testCase "somewhere (g <> f) /= somewhere (f <> g)" $
          let paysWallet3 [] = False
              paysWallet3 (PaysPKWithDatum tgt _ _ _ : xs) = tgt == walletPKHash (wallet 3) || paysWallet3 xs

              f _ (TxSkel lbl opts cs) =
                case toConstraints cs of
                  is :=>: os ->
                    if paysWallet3 os
                      then [TxSkel lbl opts (is :=>: (paysPK (walletPKHash $ wallet 5) (Pl.lovelaceValueOf 6_000_000) : os))]
                      else []

              g _ (TxSkel lbl opts cs) =
                case toConstraints cs of
                  is :=>: os ->
                    [ TxSkel lbl opts (is :=>: (paysPK (walletPKHash $ wallet 6) (Pl.lovelaceValueOf 10_000_000) : os)),
                      TxSkel lbl opts (is :=>: (paysPK (walletPKHash $ wallet 3) (Pl.lovelaceValueOf 10_000_000) : os))
                    ]
              tr = validateTxSkel $ txSkel [paysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 4_200_000)]
           in (somewhere (g <> f) tr `smcEq` empty)
                .&&. ( somewhere (f <> g) tr
                         `smcEq` validateTxSkel
                           ( txSkel
                               [ paysPK (walletPKHash $ wallet 5) (Pl.lovelaceValueOf 6_000_000),
                                 paysPK (walletPKHash $ wallet 3) (Pl.lovelaceValueOf 10_000_000),
                                 paysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 4_200_000)
                               ]
                           )
                     )
      ]
  ]
