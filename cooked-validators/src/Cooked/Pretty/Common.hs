{-# LANGUAGE OverloadedStrings #-}

-- | Common tools to help implement pretty-printers in cooked-validators
module Cooked.Pretty.Common
  ( DocCooked,
    renderString,
    prettyItemize,
    prettyItemizeNoTitle,
    prettyItemizeNonEmpty,
    prettyEnumerate,
    prettyHash,
  )
where

import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PP

type DocCooked = Doc ()

-- | Use this to convert a pretty-printer to a regular show function using
-- default layout options. This is used in "Testing" because Tasty uses
-- strings.
renderString :: (a -> DocCooked) -> a -> String
renderString printer = PP.renderString . PP.layoutPretty PP.defaultLayoutOptions . printer

-- | Print an item list with a title.
--
-- >>> prettyItemize "Foo" "-" ["bar1", "bar2", "bar3"]
-- Foo
--   - bar1
--   - bar2
--   - bar3
prettyItemize :: DocCooked -> DocCooked -> [DocCooked] -> DocCooked
prettyItemize title bullet items =
  PP.vsep
    [ title,
      PP.indent 2 . prettyItemizeNoTitle bullet $ items
    ]

prettyItemizeNoTitle :: DocCooked -> [DocCooked] -> DocCooked
prettyItemizeNoTitle bullet = PP.vsep . map (bullet <+>)

prettyItemizeNonEmpty :: DocCooked -> DocCooked -> [DocCooked] -> Maybe DocCooked
prettyItemizeNonEmpty _ _ [] = Nothing
prettyItemizeNonEmpty title bullet items = Just $ prettyItemize title bullet items

prettyEnumerate :: DocCooked -> DocCooked -> [DocCooked] -> DocCooked
prettyEnumerate title bullet items =
  PP.vsep
    [ title,
      PP.indent 2 . PP.vsep $
        zipWith (\index item -> PP.pretty index <> bullet <+> PP.align item) [1 :: Int ..] items
    ]

-- | Pretty print a prefix of a hash with a given length.
prettyHash :: (Show a) => Int -> a -> DocCooked
prettyHash printedLength = PP.pretty . ('#' :) . take printedLength . show
