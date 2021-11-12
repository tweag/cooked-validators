{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Tasty.Metadata where

import Data.Foldable (asum)
import Data.List (intersperse, isSuffixOf, stripPrefix)
import qualified Data.Map.Strict as M
import Data.Typeable
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Providers as Tasty
import qualified Test.Tasty.Runners as Tasty

-- * Attaching Metadata to Tasty Tests

-- | A 'DetailedTestResult' groups up all information that comes out of a tasty test suite,
--  including whatever metadata the user annotated their tests with.
data DetailedTestResult meta = DetailedTestResult
  { trIndex :: Int,
    trTestName :: Tasty.TestName,
    trResult :: Tasty.Result,
    trMetadata :: meta
  }
  deriving (Functor)

-- | Whether or not a given 'DetailedTestResult' indicates the test passed.
passed :: DetailedTestResult meta -> Bool
passed dtr = case Tasty.resultOutcome (trResult dtr) of
  Tasty.Success -> True
  _ -> False

-- | A 'TestReport' organizes sets of tests into sections; We are using lists
-- instead of sets in order to avoid having to define @Eq@ and @Ord@ instances
-- for 'DetailedTestResult'.
type TestReport meta = M.Map (Sections meta) [DetailedTestResult meta]

class (Typeable meta, Ord (Sections meta)) => IsMeta meta where
  type Sections meta :: *
  sectionOf :: meta -> Sections meta

-- | Rendering a report consists in producing an @IO ()@ action given a
-- @Maybe FilePath@ and a 'TestReport'.
type TestReportRenderer meta = TestReport meta -> Maybe FilePath -> IO ()

-- ** Pushing metadata to existing test cases

data WithMetadata meta :: * where
  WithMetadata ::
    forall t meta.
    (Tasty.IsTest t) =>
    { testMeta :: meta,
      testBody :: t
    } ->
    WithMetadata meta
  deriving (Typeable)

-- | Pushes some metadata down a 'Tasty.TestTree'. This succeeds as long as the test tree
--  contains a single 'Tasty.SingleTest' and no 'Tasty.After' nodes.
pushMetadata :: (Typeable meta) => meta -> Tasty.TestTree -> Tasty.TestTree
pushMetadata m (Tasty.SingleTest tname t) = Tasty.SingleTest tname (WithMetadata m t)
pushMetadata m (Tasty.AskOptions f) = Tasty.AskOptions (pushMetadata m . f)
pushMetadata m (Tasty.PlusTestOptions plus tree) = Tasty.PlusTestOptions plus (pushMetadata m tree)
pushMetadata m (Tasty.WithResource spec genTree) = Tasty.WithResource spec (pushMetadata m . genTree)
pushMetadata m (Tasty.TestGroup name [t]) = Tasty.TestGroup name [pushMetadata m t]
pushMetadata _ _ =
  error "pushMetadata can only be applied to TestTrees containing a single Tasty.SingleTest and no Tasty.After node"

instance (Typeable meta) => Tasty.IsTest (WithMetadata meta) where
  run opts (WithMetadata _ body) rep = Tasty.run opts body rep
  testOptions = return []

-- * Tweag Audit Specific Metadata

-- TODO: move to its own module?

data Severity = Critical | High | Medium | Low | Lowest
  deriving (Eq, Ord, Show)

data Class = Vuln | Bug | Underspec
  deriving (Eq, Ord, Show)

type Description = String

data TwauditMetadata = TwauditMetadata
  { mClass :: Class,
    mSeverity :: Severity,
    mDescr :: Description
  }
  deriving (Eq, Show, Typeable)

instance IsMeta TwauditMetadata where
  type Sections TwauditMetadata = Class

  sectionOf = mClass

-- ** Some Smart constructors

prependTwauditIssue :: Maybe String -> Description -> Description
prependTwauditIssue mLbl desc =
  unlines
    [ concat ["\\issue{\\$severity}{", maybe "tasty-lbl-$ix" id mLbl, "}{$name}"],
      desc
    ]

vuln :: Severity -> Description -> Tasty.TestTree -> Tasty.TestTree
vuln sev d = pushMetadata (TwauditMetadata Vuln sev (prependTwauditIssue Nothing d))

bug :: Severity -> Description -> Tasty.TestTree -> Tasty.TestTree
bug sev d = pushMetadata (TwauditMetadata Bug sev (prependTwauditIssue Nothing d))

underspec :: Severity -> Description -> Tasty.TestTree -> Tasty.TestTree
underspec sev d = pushMetadata (TwauditMetadata Underspec sev (prependTwauditIssue Nothing d))

vuln' :: String -> Severity -> Description -> Tasty.TestTree -> Tasty.TestTree
vuln' lbl sev d = pushMetadata (TwauditMetadata Vuln sev (prependTwauditIssue (Just lbl) d))

bug' :: String -> Severity -> Description -> Tasty.TestTree -> Tasty.TestTree
bug' lbl sev d = pushMetadata (TwauditMetadata Bug sev (prependTwauditIssue (Just lbl) d))

underspec' :: String -> Severity -> Description -> Tasty.TestTree -> Tasty.TestTree
underspec' lbl sev d = pushMetadata (TwauditMetadata Underspec sev (prependTwauditIssue (Just lbl) d))

-- ** Rendering to text

-- | Renders a 'TestReport' containing our own metadata into three latex files,
--  if the filepath is a @Just@, does nothing otherwise.
--  The reason for creating three latex files is that this makes it easier to
--  later on include hand-written issues in each different category.
renderTwauditLatex :: TestReportRenderer TwauditMetadata
renderTwauditLatex _ Nothing = return ()
renderTwauditLatex report (Just fn) =
  flip mapM_ [Vuln, Bug, Underspec] $ \klass ->
    case M.lookup klass report of
      Nothing -> return ()
      Just issues ->
        writeFile (renderFilePath klass fn) $
          showsConcatWith (showChar '\n') (map renderDetailedTwauditMeta issues) ""

renderFilePath :: Class -> FilePath -> FilePath
renderFilePath klass fn =
  let fn' = maybe fn id $ stripSuffix ".tex" fn
   in fn' ++ '-' : klassStr ++ ".tex"
  where
    klassStr = case klass of
      Vuln -> "vuln"
      Underspec -> "underspec"
      Bug -> "bug"

    stripSuffix suf xs
      | suf `isSuffixOf` xs = Just $ take (length xs - length suf) xs
      | otherwise = Nothing

showsConcatWith :: ShowS -> [ShowS] -> ShowS
showsConcatWith sep = foldr (.) id . intersperse sep

-- | Renders the final description by replacing some metavariables
--  in the 'mDescr' body with their associated values. Check the source
--  to see exactly which escape sequences are available.
renderDetailedTwauditMeta :: DetailedTestResult TwauditMetadata -> ShowS
renderDetailedTwauditMeta (DetailedTestResult ix tname out (TwauditMetadata _klass sev descr)) =
  showString $
    replace
      [ ("$$", "$"),
        ("$name", tname),
        ("$severity", show sev),
        ("$ix", show ix),
        ("$outcome", case Tasty.resultOutcome out of Tasty.Success -> "pass"; _ -> "fail"),
        ("$time", show (Tasty.resultTime out))
      ]
      descr
  where
    replace :: [(String, String)] -> String -> String
    replace _dict [] = []
    replace dict (s : ss)
      | s /= '$' = s : replace dict ss
      | otherwise = case asum (map (uncurry $ replace1 $ s : ss) dict) of
        Just (s', rest) -> s' ++ replace dict rest
        Nothing -> s : replace dict ss

    replace1 :: String -> String -> String -> Maybe (String, String)
    replace1 haystack needle res = (res,) <$> stripPrefix needle haystack
