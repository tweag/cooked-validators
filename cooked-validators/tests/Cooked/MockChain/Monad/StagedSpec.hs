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

assertAll :: [a] -> (a -> Assertion) -> Assertion
assertAll space f = mapM_ f space

possibleTraces :: [StagedMockChain ()]
possibleTraces =
  [ return (),
    void $ validateTxConstr [paysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 4200000)],
    void $ do
      validateTxConstr [paysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 4200000)]
      validateTxConstr [paysPK (walletPKHash $ wallet 3) (Pl.lovelaceValueOf 4200000)]
  ]

tests :: [TestTree]
tests =
  [ testGroup
      "lawfulness"
      [ testCase "'LtlFalsity' can not be satisfied" $
          assertAll possibleTraces $ \tr ->
            (startLtl LtlFalsity >> tr) `smcEq` empty,
        testCase "'LtlTruth' does not change anything" $
          assertAll possibleTraces $ \tr ->
            (startLtl LtlTruth >> tr) `smcEq` tr
      ]
  ]

--   testGroup
--     "unit"
--     [ testCase "somewhere (Just . f) (a >> b >> c) == [f a >> b >> c , a >> f b >> c , a >> b >> f c]" $
--         let f (TxSkel lbl opts cs) =
--               case toConstraints cs of
--                 is :=>: os -> TxSkel lbl opts (is :=>: (paysPK (walletPKHash $ wallet 5) (Pl.lovelaceValueOf 10000000) : os))
--             tr f g h = void $ do
--               validateTxSkel $ f $ txSkel [paysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 4200000)]
--               validateTxSkel $ g $ txSkel [paysPK (walletPKHash $ wallet 3) (Pl.lovelaceValueOf 4200000)]
--               -- This return is here on purpose; it adds something that is not a ValidateTxSkel to
--               -- the staged AST. Check https://github.com/tweag/plutus-libs/pull/110 for the
--               -- bug that triggered these tests to be created.
--               _ <- return ()
--               validateTxSkel $ h $ txSkel [paysPK (walletPKHash $ wallet 4) (Pl.lovelaceValueOf 4200000)]
--          in interpret (somewhere (Just . f) (tr id id id)) `imcEq` interpret (tr f id id <|> tr id f id <|> tr id id f),
--       testCase "somewhere (\\case b -> b'; _ -> Nothing) (a >> b >> c) == [a >> b' >> c]" $
--         let paysWallet3 [] = False
--             paysWallet3 (PaysPKWithDatum tgt _ _ _ : xs) = tgt == walletPKHash (wallet 3) || paysWallet3 xs

--             f (TxSkel lbl opts cs) =
--               case toConstraints cs of
--                 is :=>: os ->
--                   if paysWallet3 os
--                     then Just $ TxSkel lbl opts (is :=>: (paysPK (walletPKHash $ wallet 5) (Pl.lovelaceValueOf 10000000) : os))
--                     else Nothing
--             tr f g h = void $ do
--               validateTxSkel $ f $ txSkel [paysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 4200000)]
--               validateTxSkel $ g $ txSkel [paysPK (walletPKHash $ wallet 3) (Pl.lovelaceValueOf 4200000)]
--               validateTxSkel $ h $ txSkel [paysPK (walletPKHash $ wallet 4) (Pl.lovelaceValueOf 4200000)]
--          in interpret (somewhere f (tr id id id)) `imcEq` interpret (tr id (fromJust . f) id)
--     ],
--   testCase
--     "Somewhere is exponential in branch number"
--     $
--     -- If we make a trace @tr = a >> b@ with two transactions, Then execute:
--     --
--     -- > tr' = somewhere f $ somewhere g $ tr
--     --
--     -- And we expect there to be four traces as a result:
--     --
--     -- > 1. f (g a) >> b
--     -- > 2. f a >> g b
--     -- > 3. g a >> f b
--     -- > 4. a >> f (g b)
--     --
--     -- If we execute @somewhere k tr'@ instead, we should expect to see 8 branches.
--     -- Because we're choosing @f = g = k = id@, we exept the eight traces to be equal to tr.
--     let tr =
--           validateTxConstr [paysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 4_200_000)]
--             >> validateTxConstr [paysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 8_400_000)]
--      in interpret (somewhere Just $ somewhere Just $ somewhere Just tr)
--           `imcEq` asum (replicate 8 $ interpret tr),
--   testCase "Modality order is respected" $
--     let -- Sample trace
--         tr f g =
--           validateTxConstr
--             [ paysPK
--                 (walletPKHash $ wallet 2)
--                 (f $ g $ Pl.lovelaceValueOf 4_200_000)
--             ]
--         -- Function to modify some specific skeletons
--         app f (TxSkel l opts constraintsSpec) =
--           case toConstraints constraintsSpec of
--             [] :=>: [PaysPKWithDatum pk stak dat val] ->
--               Just $ txSkelLblOpts l opts [PaysPKWithDatum pk stak dat (f val)]
--             _ -> Nothing
--         -- Two transformations
--         f x = Pl.lovelaceValueOf 3_000_000
--         g x = Pl.lovelaceValueOf (2 * Pl.getLovelace (Pl.fromValue x))
--      in interpret (everywhere (app f) $ everywhere (app g) (tr id id)) `imcEq` interpret (tr f g),
--   testGroup
--     "interpModalities unit tests"
--     [ testCase "works as expected for two 'Somewhere'" $
--         let f x = x + 5
--             g x = x * 5
--             ms = [Somewhere (Just . f), Somewhere (Just . g)]
--             x = 12
--          in map (second length) (interpModalities ms x)
--               @?= [ (f (g x), 0), -- applied both functions, nothing to consume
--                     (g x, 1), -- applied one, has one to consume
--                     (f x, 1), -- applied one, has one to consume
--                     (x, 2) -- applied none, has two to consume
--                   ],
--       testCase "works as expected for two 'Everywhere'" $
--         let f x = 3
--             g x = x * 5
--             ms = [Everywhere (Just . f), Everywhere (Just . g)]
--             x = 12
--          in map (second length) (interpModalities ms x)
--               @?= [(f (g x), 2)] -- applied both, but since they stay there, there's still 2.
--     ]
-- ]
