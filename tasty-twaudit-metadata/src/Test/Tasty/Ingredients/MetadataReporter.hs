{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Tasty.Ingredients.MetadataReporter
  ( metadataTestReporter,
    twauditMain,
    twauditMain',
    SaveReportToFile (..),
    ReportTestsSuchThat (..),
  )
where

import Control.Monad.State.Strict
import Data.Char (toLower)
import Data.IORef
import qualified Data.IntMap as IM
import qualified Data.Map.Strict as M
import Data.Typeable
import Debug.Trace
import GHC.Conc (TVar)
import GHC.Conc.Sync (atomically, readTVar, retry)
import GHC.IO
import System.Exit
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Ingredients.ConsoleReporter
import Test.Tasty.Metadata
import Test.Tasty.Options
import Test.Tasty.Providers
import Test.Tasty.Runners

-- * Twaudit-specific main functions

-- TODO: move this to a separate library if we ever release tasty-metadata to the wild

twauditMain :: TestTree -> IO ()
twauditMain tree = twauditMain' Nothing tree >>= exitWith

-- | A very general main function that stops Tasty from exiting the process, in case we
--  want to look at a copy of our TestReport instead of just sending it to a file.
twauditMain' :: Maybe (IORef (TestReport TwauditMetadata)) -> TestTree -> IO ExitCode
twauditMain' r tree = do
  ( defaultMainWithIngredients
      [composeReporters (metadataTestReporter renderTwauditLatex r) consoleTestReporter]
      tree
      >> return ExitSuccess
    )
    `catchException` return

-- * Reporting Test Metadata as an Ingredient

-- | This custom reporter will render and keep a test report for all test cases
-- that have metadata associated with them, that is, all tests cases define
-- with a 'WithMetadata'.
--
-- This ingredient is meant to be used with 'composeReporters', adding functionality
-- to some other reporter. Most often, you want to use:
--
-- > composeReporters (metadataTestReporter ...) consoleTestReporter
metadataTestReporter ::
  (IsMeta meta) =>
  -- | Renderer to use when converting the report to text
  TestReportRenderer meta ->
  -- | Whether we want to keep a copy of the report in a IORef
  Maybe (IORef (TestReport meta)) ->
  Ingredient
metadataTestReporter renderer mior = TestReporter metadataReporterOptions $
  \opts tree ->
    let SaveReportToFile saveAt = lookupOption opts
        ReportTestsSuchThat testPred = lookupOption opts

        mdict = buildMetadataDict opts tree
     in if IM.null mdict
          then Nothing
          else Just $ \smap -> do
            -- With the smap :: StatusMap at hand, we first build the entire TestReport
            -- then we filter the tests that pass the user-provided predicate, which
            -- is "const True" if none is provided. We also filter out the sections that
            -- end up with no DetailedTestResults to report.
            !report <-
              M.filter (not . null) . M.map (filter (testPred . fmap (const ())))
                <$> mkTestMetaReport mdict smap

            -- If we want to keep a copy within an IORef; we do so.
            case mior of
              Nothing -> return ()
              Just ior -> writeIORef ior report

            -- Call the renderer, so it can do whatever is suitable with the given report
            renderer report saveAt

            -- Finally, we rely on the existing computeStatistics to detect any failure,
            -- this was taken from "Test.Tasty.Ingredients.ConsoleReporter".
            stats <- computeStatistics smap
            return $ \_time -> return $ statFailures stats == 0

-- ** Reporter specific options

metadataReporterOptions :: [OptionDescription]
metadataReporterOptions =
  [ Option (Proxy :: Proxy SaveReportToFile),
    Option (Proxy :: Proxy ReportTestsSuchThat)
  ]

-- | Instructs tasty whether or not to save a rendered report to a file
newtype SaveReportToFile = SaveReportToFile (Maybe FilePath)
  deriving (Typeable)

instance IsOption SaveReportToFile where
  defaultValue = SaveReportToFile Nothing
  parseValue = Just . SaveReportToFile . Just
  optionName = return "save-metareport"
  optionHelp = return "Saves the metadata report to a given file "
  showDefaultValue _ = Nothing

-- | Instructs tasty to only report tests whose result satisfy a given predicate.
-- On the command line, three options are available:
-- - "all", "failed" and "passed"
-- If you want a more complex predicate, construct your preicated and pass it with 'localOption'
-- to the test tree you want to be affected.
newtype ReportTestsSuchThat = ReportTestsSuchThat (DetailedTestResult () -> Bool)
  deriving (Typeable)

parseTestResultPredicate :: String -> Maybe (DetailedTestResult () -> Bool)
parseTestResultPredicate s =
  case map toLower s of
    "all" -> return $ const True
    "passed" -> return passed
    "failed" -> return $ not . passed
    _ -> Nothing

instance IsOption ReportTestsSuchThat where
  defaultValue = ReportTestsSuchThat (const True)
  parseValue = fmap ReportTestsSuchThat . parseTestResultPredicate
  optionName = return "metareport-only"
  optionHelp =
    return $
      "Which tests to save metadata reports for\n"
        ++ "Can be all|passed|failed"

-- ** Reporter Internals

-- | Auxiliary structure; check 'buildMetaDict' for how its created, check 'metadataTestReporter'
--  for how its used. The idea is that it keeps the test name and metainformation of all tests
--  in a test tree that were 'WithMetadata'.
type TestMetaDict meta = IM.IntMap (String, meta)

-- | Collects all the tests that are defined with 'WithMetadata'
buildMetadataDict :: forall meta. (Typeable meta) => OptionSet -> TestTree -> TestMetaDict meta
buildMetadataDict opts =
  flip evalState 0 . getApp . foldTestTree (trivialFold {foldSingle = single}) opts
  where
    single :: forall t. (Typeable t) => OptionSet -> TestName -> t -> Ap (State Int) (TestMetaDict meta)
    single _opts tname mt = Ap $ do
      ix <- get
      put $! ix + 1
      case cast mt :: Maybe (WithMetadata meta) of
        Nothing -> return mempty
        Just t -> do
          trace ("Got: " ++ show tname) (return ())
          return $ IM.singleton ix (tname, testMeta t)

-- | Intersects the 'TestMetaDict', which contains all tests which had metainformation,
--  with the 'StatusMap'; gathering the results into a report organized by class.
mkTestMetaReport ::
  forall meta.
  (IsMeta meta) =>
  TestMetaDict meta ->
  StatusMap ->
  IO (TestReport meta)
mkTestMetaReport tdict smap =
  M.fromListWith (++) <$> mapM go (IM.toList $ IM.intersectionWith (,) tdict smap)
  where
    go :: (IM.Key, ((String, meta), TVar Status)) -> IO (Sections meta, [DetailedTestResult meta])
    go (ix, ((tname, meta), iores)) = do
      res <- getResultFromTVar iores
      return (sectionOf meta, [DetailedTestResult ix tname res meta])

-- | Waits on a TVar until the result is available.
getResultFromTVar :: TVar Status -> IO Result
getResultFromTVar var =
  atomically $ do
    status <- readTVar var
    case status of
      Done r -> return r
      _ -> retry
