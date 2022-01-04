module Cooked.MockChain.Monad.StagedSpec (spec) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Writer
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
    void $ validateTxConstr [PaysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 4200)],
    void $ do
      validateTxConstr [PaysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 4200)]
      validateTxConstr [PaysPK (walletPKHash $ wallet 3) (Pl.lovelaceValueOf 4200)]
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
    -- We'll make a trace @tr = a >> b@ with two transactions. Then, we'll execute:
    --
    -- > somewhere f $ somewhere g $ tr
    --
    -- And we expect there to be four traces as a result:
    --
    -- > 1. f (g a) >> b
    -- > 2. f a >> g b
    -- > 3. g a >> f b
    -- > 4. a >> f (g b)
    --
    -- Because we're choosing @f = g = id@, we exept the four traces to be equal to tr.
    let tr =
          validateTxConstr [PaysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 4200)]
            >> validateTxConstr [PaysPK (walletPKHash $ wallet 2) (Pl.lovelaceValueOf 8400)]
     in interpret (somewhere Just $ somewhere Just tr) `imcEq` asum (replicate 4 $ interpret tr)

  it "Modality order is respected" $
    let -- Sample trace
        tr f g =
          validateTxConstr
            [ PaysPK
                (walletPKHash $ wallet 2)
                (f $ g $ Pl.lovelaceValueOf 4200)
            ]
        -- Function to modify some specific skeletons
        app f (TxSkel l [PaysPK tgt val]) = Just $ TxSkel l [PaysPK tgt (f val)]
        app f _ = Nothing
        -- Two transformations
        f x = Pl.lovelaceValueOf 3000
        g x = Pl.lovelaceValueOf (2 * Pl.getLovelace (Pl.fromValue x))
     in interpret (everywhere (app f) $ everywhere (app g) (tr id id)) `imcEq` interpret (tr f g)

  describe "interpModalities" $ do
    it "Satisfy one unit test" $
      let f x = x + 5
          g x = x * 5
          ms = [Somewhere (Just . f), Somewhere (Just . g)]
          x = 12
       in map (id *** length) (interpModalities ms x)
            @?= [ (f (g x), 0),
                  (g x, 1),
                  (f x, 1),
                  (x, 2)
                ]
