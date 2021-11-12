{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Data.IORef
import qualified Data.Map as M
import System.Exit
import Test.Tasty
import Test.Tasty.ExpectedFailure
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.Ingredients.MetadataReporter
import Test.Tasty.Metadata
import qualified Test.Tasty.QuickCheck as QC
import Text.Heredoc

main :: IO ()
main = do
  ior <- newIORef M.empty
  res <- twauditMain' (Just ior) testTree
  readIORef ior >>= putStrLn . testReportRender renderTwauditLatex
  exitWith res

testTree :: TestTree
testTree =
  testGroup
    "A group with some metadata tests"
    [ bug
        Critical
        [str|It is paramount that two must not be three! This is
              |a first test for the metadata association.
              |Note how the \hs{bug} constructor will add the \\issue{...} header
              |that we need for latex.
              |]
        $ expectFail $
          HU.testCase "Two must not be three" $ 2 HU.@?= 3,
      -- Now a test without any metadata
      HU.testCase "Passing test without metadata" (5 HU.@?= 5),
      -- Now an inner group!
      testGroup
        "Inner Group"
        [ vuln'
            "important-vuln"
            High
            [str|Here's a potentially dangerous vulnerability! Beware!
                |*Drama Intensifies*
                |Here we use \hs{vuln'} to add a custom label to this vulnerability.
                |]
            $ QC.testProperty
              "Whatever cubed is greater than itself"
              (\x -> (x :: Integer) * x >= x),
          expectFail $ HU.testCase "Now a failing test without metadata" undefined,
          -- And another test in the "bug" group
          bug
            Medium
            [str|Finally, a last very problematic bug that we can try
                 |and cross-ref \Cref{important-vuln}
                 |]
            $ HU.testCase "Another Problematic Bug" (1 HU.@?= 1)
        ]
    ]
