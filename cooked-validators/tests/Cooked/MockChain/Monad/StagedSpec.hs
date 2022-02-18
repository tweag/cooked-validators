{-# LANGUAGE NumericUnderscores #-}

module Cooked.MockChain.Monad.StagedSpec (spec) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Writer.Strict
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
import Data.Foldable
import qualified Ledger.Ada as Pl
import Test.HUnit
import Test.Hspec

imcEq :: (Show a, Eq a) => InterpMockChain a -> InterpMockChain a -> Assertion
imcEq a b = go a @?= go b
  where
    go = map fst . runWriterT . runMockChainTRaw def def

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

spec :: Spec
spec = do
  describe "somewhere is lawful" $ do
    it "somewhere (const Nothing) == const empty" $
      assertAll possibleTraces $ \tr ->
        interpret (somewhere (const Nothing) tr) `imcEq` empty

  describe "everywhere is lawful" $ do
    it "everywhere (const Nothing) == id" $
      assertAll possibleTraces $ \tr ->
        interpret (everywhere (const Nothing) tr) `imcEq` interpret tr

  it "Somewhere is exponential in branch number" $
    -- If we make a trace @tr = a >> b@ with two transactions, Then execute:
    --
    -- > tr' = somewhere f $ somewhere g $ tr
    --
    -- And we expect there to be four traces as a result:
    --
    -- > 1. f (g a) >> b
    -- > 2. f a >> g b
    -- > 3. g a >> f b
    -- > 4. a >> f (g b)
    --
    -- If we execute @somewhere k tr'@ instead, we should expect to see 8 branches.
    -- Because we're choosing @f = g = k = id@, we exept the eight traces to be equal to tr.
    let tr =
          validateTxConstr [paysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 4_200_000)]
            >> validateTxConstr [paysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 8_400_000)]
     in interpret (somewhere Just $ somewhere Just $ somewhere Just tr)
          `imcEq` asum (replicate 8 $ interpret tr)

  it "Modality order is respected" $
    let -- Sample trace
        tr f g =
          validateTxConstr
            [ paysPK
                (walletPKHash $ wallet 2)
                (f $ g $ Pl.lovelaceValueOf 4_200_000)
            ]
        -- Function to modify some specific skeletons
        app f (TxSkel l opts [PaysPKWithDatum pk stak dat val]) =
          Just $ TxSkel l opts [PaysPKWithDatum pk stak dat (f val)]
        app f _ = Nothing
        -- Two transformations
        f x = Pl.lovelaceValueOf 3_000_000
        g x = Pl.lovelaceValueOf (2 * Pl.getLovelace (Pl.fromValue x))
     in interpret (everywhere (app f) $ everywhere (app g) (tr id id)) `imcEq` interpret (tr f g)

  describe "interpModalities unit tests" $ do
    it "works as expected for two 'Somewhere'" $
      let f x = x + 5
          g x = x * 5
          ms = [Somewhere (Just . f), Somewhere (Just . g)]
          x = 12
       in map (second length) (interpModalities ms x)
            @?= [ (f (g x), 0), -- applied both functions, nothing to consume
                  (g x, 1), -- applied one, has one to consume
                  (f x, 1), -- applied one, has one to consume
                  (x, 2) -- applied none, has two to consume
                ]

    it "works as expected for two 'Everywhere'" $
      let f x = 3
          g x = x * 5
          ms = [Everywhere (Just . f), Everywhere (Just . g)]
          x = 12
       in map (second length) (interpModalities ms x)
            @?= [(f (g x), 2)] -- applied both, but since they stay there, there's still 2.
