{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tasty.Ingredients.MetadataReporter
  ( metadataTestReporter,
    twauditMain,
    twauditMain',
    SaveReportToFile (..),
  )
where

import Control.Monad.State.Strict
import Data.IORef
import qualified Data.IntMap as IM
import qualified Data.Map.Strict as M
import Data.Typeable
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
          return $ IM.singleton ix (tname, testMeta t)

-- | Waits on a TVar until the result is available.
getResultFromTVar :: TVar Status -> IO Result
getResultFromTVar var =
  atomically $ do
    status <- readTVar var
    case status of
      Done r -> return r
      _ -> retry

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

-- | Custom reporter for tests that have metadata on them
metadataTestReporter ::
  (IsMeta meta) =>
  TestReportRenderer meta ->
  Maybe (IORef (TestReport meta)) ->
  Ingredient
metadataTestReporter renderer mior = TestReporter metadataReporterOptions $
  \opts tree ->
    let SaveReportToFile saveAt = lookupOption opts
        mdict = buildMetadataDict opts tree
     in if IM.null mdict
          then Nothing
          else Just $ \smap -> do
            !report <- mkTestMetaReport mdict smap
            case mior of
              Nothing -> return ()
              Just ior -> writeIORef ior report
            case saveAt of
              Nothing -> return ()
              Just f -> writeFile f (testReportRender renderer report)
            stats <- computeStatistics smap
            return $ \_time -> return $ statFailures stats == 0

metadataReporterOptions :: [OptionDescription]
metadataReporterOptions = [Option (Proxy :: Proxy SaveReportToFile)]

newtype SaveReportToFile = SaveReportToFile (Maybe FilePath)
  deriving (Typeable)

instance IsOption SaveReportToFile where
  defaultValue = SaveReportToFile Nothing
  parseValue = Just . SaveReportToFile . Just
  optionName = return "save-report"
  optionHelp = return "Saves the metadata report to a given file "
  showDefaultValue _ = Nothing

-- * Twaudit-specific main functions

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
