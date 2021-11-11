{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Tasty.Metadata where

import Control.Arrow (second)
import Data.Foldable (asum)
import Data.Function (on)
import Data.List (intersperse, sortBy, stripPrefix)
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

-- | A 'TestReport' organizes sets of tests into sections; We are using lists
-- instead of sets in order to avoid having to define @Eq@ and @Ord@ instances
-- for 'DetailedTestResult'.
type TestReport meta = M.Map (Sections meta) [DetailedTestResult meta]

class (Typeable meta, Ord (Sections meta)) => IsMeta meta where
  type Sections meta :: *
  sectionOf :: meta -> Sections meta

-- | Rendering a report relies on an explicit dictionary in order to easily customize
--  the formatting behavior.
data TestReportRenderer meta = TestReportRenderer
  { trrRenderSection :: Sections meta -> [ShowS] -> ShowS,
    trrRenderResult :: DetailedTestResult meta -> ShowS,
    trrSort :: [DetailedTestResult meta] -> [DetailedTestResult meta]
  }

-- | Renders a given report according to some some renderer
testReportRender :: TestReportRenderer meta -> TestReport meta -> String
testReportRender TestReportRenderer {..} =
  ($ "") . showsConcatWith (showChar '\n') . map shows1 . M.toList
  where
    shows1 = uncurry trrRenderSection . second (map trrRenderResult . trrSort)

showsConcatWith :: ShowS -> [ShowS] -> ShowS
showsConcatWith sep = foldr (.) id . intersperse sep

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

pushMetadata :: (Typeable meta) => meta -> Tasty.TestTree -> Tasty.TestTree
pushMetadata m (Tasty.SingleTest tname t) = Tasty.SingleTest tname (WithMetadata m t)
pushMetadata _ _ = error "pushMetadata can only be applied to a Tasty.SingleTest"

(#>) :: (Typeable meta) => meta -> Tasty.TestTree -> Tasty.TestTree
(#>) = pushMetadata

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

renderClassLatex :: Class -> ShowS
renderClassLatex Vuln = showString "\\section{Vulnerabilities}"
renderClassLatex Bug = showString "\\section{Implementation Bugs}"
renderClassLatex Underspec = showString "\\section{Unclear Specification}"

renderTwauditLatex :: TestReportRenderer TwauditMetadata
renderTwauditLatex =
  TestReportRenderer
    { trrRenderSection = \sect -> showsConcatWith (showChar '\n') . (renderClassLatex sect :),
      trrRenderResult = renderDetailedTwauditMeta,
      trrSort = sortBy (compare `on` trIndex)
    }

-- ** Some Smart constructors

vuln0 :: Description -> TwauditMetadata
vuln0 = TwauditMetadata Vuln Critical

vuln1 :: Description -> TwauditMetadata
vuln1 = TwauditMetadata Vuln High

vuln2 :: Description -> TwauditMetadata
vuln2 = TwauditMetadata Vuln Medium

bug0 :: Description -> TwauditMetadata
bug0 = TwauditMetadata Bug Critical

bug1 :: Description -> TwauditMetadata
bug1 = TwauditMetadata Bug High

bug2 :: Description -> TwauditMetadata
bug2 = TwauditMetadata Bug Medium

bug3 :: Description -> TwauditMetadata
bug3 = TwauditMetadata Bug Low

bug4 :: Description -> TwauditMetadata
bug4 = TwauditMetadata Bug Lowest

underspec0 :: Description -> TwauditMetadata
underspec0 = TwauditMetadata Underspec Critical

underspec1 :: Description -> TwauditMetadata
underspec1 = TwauditMetadata Underspec High

underspec2 :: Description -> TwauditMetadata
underspec2 = TwauditMetadata Underspec Medium

underspec3 :: Description -> TwauditMetadata
underspec3 = TwauditMetadata Underspec Low

underspec4 :: Description -> TwauditMetadata
underspec4 = TwauditMetadata Underspec Lowest
