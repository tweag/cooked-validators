{-# LANGUAGE OverloadedStrings #-}

-- | Common tools to help implement pretty-printers in cooked-validators
module Cooked.Pretty.Common where

import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.String as PP

type DocCooked = Doc ()

-- | Use this to convert a pretty-printer to a regular show function using
-- default layout options. This is used in "Testing" because Tasty uses
-- strings.
renderString :: (a -> DocCooked) -> a -> String
renderString printer = PP.renderString . PP.layoutPretty PP.defaultLayoutOptions . printer

-- prettyItemize "Foo" "-" ["bar1", "bar2", "bar3"]
--    Foo
--      - bar1
--      - bar2
--      - bar3
prettyItemize :: DocCooked -> DocCooked -> [DocCooked] -> DocCooked
prettyItemize title bullet items =
  PP.vsep
    [ title,
      PP.indent 2 . PP.vsep $
        map (bullet <+>) items
    ]

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

-- prettyHash 28a3d93cc3daac
-- #28a3d9
prettyHash :: (Show a) => Int -> a -> DocCooked
prettyHash printedLength = PP.pretty . ('#' :) . take printedLength . show

-- prettyNumericUnderscore 23798423723
-- 23_798_423_723
prettyNumericUnderscore :: Integer -> DocCooked
prettyNumericUnderscore i
  | 0 == i = "0"
  | i > 0 = psnTerm "" 0 i
  | otherwise = "-" <> psnTerm "" 0 (-i)
  where
    psnTerm :: DocCooked -> Integer -> Integer -> DocCooked
    psnTerm acc _ 0 = acc
    psnTerm acc 3 nb = psnTerm (PP.pretty (nb `mod` 10) <> "_" <> acc) 1 (nb `div` 10)
    psnTerm acc n nb = psnTerm (PP.pretty (nb `mod` 10) <> acc) (n + 1) (nb `div` 10)
