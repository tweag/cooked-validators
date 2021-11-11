{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Test.Tasty.WithMetadata where

import Control.Monad.State.Strict
import Data.Foldable
import Data.IORef
import qualified Data.IntMap as IM
import Data.List (stripPrefix)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Typeable
import Debug.Trace
import GHC.Conc (TVar)
import GHC.Conc.Sync (atomically, readTVar, retry)
import Test.Tasty.Ingredients.ConsoleReporter
import Test.Tasty.Options
import Test.Tasty.Providers
import Test.Tasty.Runners

-- * Metadata to Attach to Test Cases:

data Severity = Critical | High | Medium | Low | Lowest
  deriving (Eq, Ord, Show)

data Class = Vuln | Bug | Underspec
  deriving (Eq, Ord, Show)

type Description = String

data Metadata = Metadata
  { mClass :: Class,
    mSeverity :: Severity,
    mDescr :: Description
  }
  deriving (Eq, Show)

-- | Renders the final description by replacing some metavariables
--  in the 'mDescr' body with their associated values.
--
--  In particular, we replace:
--  - "$$" -> "$"
--  - "$name" -> test name
--  - "$severity" -> test severity
--  - "$ix" -> tasty test index
--  - "$outcome" -> "pass" or "fail"
renderDescr :: Int -> TestName -> Outcome -> Metadata -> String
renderDescr ix tname out (Metadata _klass sev descr) =
  replace
    [ ("$$", "$"),
      ("$name", tname),
      ("$severity", show sev),
      ("$ix", show ix),
      ("$outcome", case out of Success -> "pass"; _ -> "fail")
    ]
    descr

replace :: [(String, String)] -> String -> String
replace _dict [] = []
replace dict (s : ss)
  | s /= '$' = s : replace dict ss
  | otherwise = case asum (map (uncurry $ replace1 $ s : ss) dict) of
    Just (s', rest) -> s' ++ replace dict rest
    Nothing -> s : replace dict ss

replace1 :: String -> String -> String -> Maybe (String, String)
replace1 haystack needle res = (res,) <$> stripPrefix needle haystack

-- * Wrapping of existing tests with some metadata

data WithMetadata where
  WithMetadata ::
    forall t.
    (IsTest t) =>
    { testMeta :: Metadata,
      testBody :: t
    } ->
    WithMetadata
  deriving (Typeable)

testCase :: (IsTest t) => TestName -> Metadata -> t -> TestTree
testCase name meta = singleTest name . WithMetadata meta

instance IsTest WithMetadata where
  run opts (WithMetadata _ body) rep = run opts body rep

  testOptions =
    return []

---------------------------------------------------------------

type TestMetaDict = IM.IntMap (String, Metadata)

buildMetadataDict :: OptionSet -> TestTree -> TestMetaDict
buildMetadataDict opts =
  flip evalState 0 . getApp . foldTestTree (trivialFold {foldSingle = single}) opts
  where
    single :: forall t. (Typeable t) => OptionSet -> TestName -> t -> Ap (State Int) TestMetaDict
    single _opts tname mt = Ap $ do
      ix <- get
      put $! ix + 1
      case cast mt :: Maybe WithMetadata of
        Nothing -> return mempty
        Just t -> do
          return $ IM.singleton ix (tname, testMeta t)

getResultFromTVar :: TVar Status -> IO Result
getResultFromTVar var =
  atomically $ do
    status <- readTVar var
    case status of
      Done r -> return r
      _ -> retry

type TestMetaReport = M.Map Class (S.Set String)

mkTestMetaReportEmpty :: TestMetaReport
mkTestMetaReportEmpty = M.empty

-- | Intersects the 'TestMetaDict', which contains all tests which had metainformation,
--  with the 'StatusMap'; gathering the results into a report organized by class.
mkTestMetaReport :: TestMetaDict -> StatusMap -> IO TestMetaReport
mkTestMetaReport tdict smap =
  M.fromListWith S.union <$> mapM go (IM.toList $ IM.intersectionWith (,) tdict smap)
  where
    go :: (IM.Key, ((String, Metadata), TVar Status)) -> IO (Class, S.Set String)
    go (ix, ((tname, meta), iores)) = do
      res <- getResultFromTVar iores
      return (mClass meta, S.singleton $ renderDescr ix tname (resultOutcome res) meta)

metareportTestReporter :: IORef TestMetaReport -> Ingredient
metareportTestReporter ior = TestReporter metareportReporterOptions $
  \opts tree ->
    let mdict = buildMetadataDict opts tree
     in if IM.null mdict
          then trace "oops" Nothing
          else Just $ \smap -> do
            !report <- mkTestMetaReport mdict smap
            writeIORef ior report
            stats <- computeStatistics smap
            return $ \_time -> return $ statFailures stats == 0

metareportReporterOptions :: [OptionDescription]
metareportReporterOptions = []
