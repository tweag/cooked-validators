{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Control.Monad
import Data.IORef
import qualified Data.Map as M
import System.Exit
import Test.Tasty
import Test.Tasty.ExpectedFailure
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.Ingredients.MetadataReporter
import qualified Test.Tasty.Metadata as TW
import qualified Test.Tasty.QuickCheck as QC
import Text.Heredoc

main :: IO ()
main = do
  ior <- newIORef M.empty
  res <- twauditMain' (Just ior) testTree
  putStrLn "We will now get the report and make sure it has all the issues we expect"
  readIORef ior >>= assert_TwoBugsOneVuln
  exitWith res

assert_TwoBugsOneVuln :: TW.TestReport TW.TwauditMetadata -> IO ()
assert_TwoBugsOneVuln report = do
  unless ((length <$> M.lookup TW.Vuln report) == Just 1) $
    fail "Expecting one vulnerability on the report"
  unless ((length <$> M.lookup TW.Bug report) == Just 2) $
    fail "Expecting two bugs on the report"

testTree :: TestTree
testTree =
  testGroup
    "A group with some metadata tests"
    [ TW.bug -- this pushes metadata down a tree that contains a single testcase
        TW.Critical
        [str|It is paramount that two must not be three! This is
            |a first test for the metadata association.
            |Note how the \hs{bug} constructor will add the \\issue{...} header
            |that we need for latex.
            |]
        $ expectFail $
          HU.testCase "Two must not be three" $ 2 HU.@?= 3,
      HU.testCase
        "Passing test without metadata"
        (5 HU.@?= 5),
      -- Inner testGroups work too.
      testGroup
        "Inner Group"
        [ TW.vuln' -- pushes metadata in the "Vulnerabilities" section
            "important-vuln"
            TW.High
            [str|Here's a potentially dangerous vulnerability! Beware!
                |*Drama Intensifies*
                |Here we use \hs{vuln'} to add a custom label to this vulnerability.
                |]
            $ QC.testProperty
              "Whatever squared is greater than itself"
              (\x -> (x :: Integer) * x >= x),
          expectFail $ HU.testCase "Now a failing test without metadata" undefined,
          -- Lets add another test in the 'Bug' section
          TW.bug
            TW.Medium
            [str|Finally, a last very problematic bug that we can try
                |and cross-ref \Cref{important-vuln}
                |]
            $ HU.testCase "Reasonable Starting Arithmetic Assumption" (1 HU.@?= 1)
        ]
    ]
