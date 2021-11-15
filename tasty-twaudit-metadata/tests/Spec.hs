{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Control.Monad
import Data.IORef
import Data.List (sort)
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
  readIORef ior >>= assertTwoBugsOneVuln
  exitWith res

assertTwoBugsOneVuln :: TW.TestReport TW.TwauditMetadata -> IO ()
assertTwoBugsOneVuln report = do
  unless ((length <$> M.lookup TW.Vuln report) == Just 1) $
    fail "Expecting one vulnerability on the report"
  unless ((length <$> M.lookup TW.Bug report) == Just 2) $
    fail "Expecting two bugs on the report"

tastyMetadataInternalTestTree :: TestTree
tastyMetadataInternalTestTree =
  testGroup
    "Internal tests for tasty-twaudit-metadata"
    [ HU.testCase
        "Issue Classes is ordered correctly: Vuln > Bug > Underspec"
        ( let classes = [TW.Underspec, TW.Bug, TW.Vuln]
           in sort classes HU.@?= classes
        ),
      HU.testCase
        "Issue Severity is ordered correctly: Critical > ... > Lowest"
        ( let sev = [TW.Lowest, TW.Low, TW.Medium, TW.High, TW.Critical]
           in sort sev HU.@?= sev
        )
    ]

testTree :: TestTree
testTree =
  testGroup
    "Tests for the metadata annotation mechanism"
    [ TW.bug -- this pushes metadata down a tree that contains a single testcase
        TW.Critical
        [str|It is paramount that two must not be three! This is
            |a first test for the metadata association.
            |Note how the \hs{bug} constructor will add the \\issue{...} header
            |that we need for latex.
            |]
        $ expectFail $
          HU.testCase "Two must not be three" $ 2 HU.@?= 3,
      -- We'll also add the actual tests for tasty-twaudit-metadata in here
      -- to make sure they DO NOT show up on the report. We do want some non-annotated
      -- tests in this tree after all
      tastyMetadataInternalTestTree,
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
