module Cooked.MockChain.Monad.StagedSpec (spec) where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Cooked.MockChain
import Data.Default
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

imcEq :: (Show a, Eq a) => InterpMockChain a -> InterpMockChain a -> Assertion
imcEq a b = go a @?= go b
  where
    go = map fst . runWriterT . runMockChainTRaw def def

assertAll :: [a] -> (a -> Assertion) -> Assertion
assertAll space f = mapM_ f space

possibleTraces :: [StagedMockChain Int]
possibleTraces = [return 42] -- TODO: write more traces to test for laws

spec :: Spec
spec = do
  describe "MonadModal" $ do
    describe "somewhere is lawful" $ do
      it "somewhere (const Nothing) == const empty" $
        assertAll possibleTraces $ \tr ->
          interpret (somewhere (const Nothing) tr) `imcEq` empty

    describe "everywhere is lawful" $ do
      it "everywhere (const Nothing) == id" $
        assertAll possibleTraces $ \tr ->
          interpret (everywhere (const Nothing) tr) `imcEq` interpret tr
