{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Data.IORef
import Data.Typeable
import GHC.IO
import Test.Tasty
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.ConsoleReporter
import Test.Tasty.Providers
import Test.Tasty.WithMetadata
import Text.Heredoc

instance IsTest Bool where
  run opts res cb = return $ if res then testPassed "" else testFailed "too bad!"
  testOptions = return []

main = do
  ior <- newIORef mkTestMetaReportEmpty
  catchAny
    ( defaultMainWithIngredients
        [composeReporters (metareportTestReporter ior) consoleTestReporter]
        testTree
    )
    ( \e -> do
        report <- readIORef ior
        print report
        throwIO e
    )

testTree =
  testGroup
    "A group with some metadata tests"
    [ testCase
        "Two must not be three"
        ( Metadata
            Bug
            Critical
            [str|\issue{$severity}{lbl-$ix}{$name}
                |It is paramount that two must not be three!
                |]
        )
        (2 == 2),
      HU.testCase "Five is Five" $ (5 HU.@?= 5)
    ]
