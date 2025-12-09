-- | This module provides common functions to help implement pretty-printers in
-- cooked-validators
module Cooked.Pretty.Class
  ( DocCooked,
    PrettyCooked (..),
    PrettyCookedList (..),
    PrettyCookedMaybe (..),
    printCookedOpt,
    printCooked,
    renderString,
    prettyHash,
    prettyItemize,
    prettyItemizeNoTitle,
    prettyItemizeNonEmpty,
  )
where

import Cooked.Pretty.Hashable
import Cooked.Pretty.Options
import Data.ByteString qualified as ByteString
import Data.Default
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Ratio
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Numeric qualified
import PlutusTx.Builtins.Internal qualified as PlutusTx
import Prettyprinter (Doc, (<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.String qualified as PP
import Prettyprinter.Render.Text qualified as PP

-- | A standard 'PP.Doc' without any annotation
type DocCooked = Doc ()

-- | Type class of things that can be pretty printed as a single document. You
-- need to implement either 'prettyCookedOpt' or 'prettyCooked' manually,
-- otherwise calling either of them will resulting in a infinite loop.
class PrettyCooked a where
  -- | Pretty prints an element based on some 'PrettyCookedOpts'
  prettyCookedOpt :: PrettyCookedOpts -> a -> DocCooked
  prettyCookedOpt _ = prettyCooked

  -- | Pretty prints an element directly
  prettyCooked :: a -> DocCooked
  prettyCooked = prettyCookedOpt def

instance PrettyCooked DocCooked where
  prettyCookedOpt _ = id

-- | Type class of things that can be pretty printed as a list of
-- documents. Similarly to 'PrettyCooked', at least of the functions from this
-- class needs to be manually implemented to avoid infinite loops.
class PrettyCookedList a where
  -- | Pretty prints an element as a list on some 'PrettyCookedOpts'
  prettyCookedOptList :: PrettyCookedOpts -> a -> [DocCooked]
  prettyCookedOptList opts = catMaybes . prettyCookedOptListMaybe opts

  -- | Pretty prints an element as a list of optional documents
  prettyCookedOptListMaybe :: PrettyCookedOpts -> a -> [Maybe DocCooked]
  prettyCookedOptListMaybe opts = fmap Just . prettyCookedOptList opts

  -- | Pretty prints an elements as a list
  prettyCookedList :: a -> [DocCooked]
  prettyCookedList = prettyCookedOptList def

instance (PrettyCooked a) => PrettyCookedList [a] where
  prettyCookedOptList opts = fmap (prettyCookedOpt opts)

instance (PrettyCooked a) => PrettyCookedList (Set a) where
  prettyCookedOptList opts = prettyCookedOptList opts . Set.toList

-- | Type class of things that can be optionally pretty printed as a document
class PrettyCookedMaybe a where
  -- | Pretty prints an optional document on some 'PrettyCookedOpts'
  prettyCookedOptMaybe :: PrettyCookedOpts -> a -> Maybe DocCooked
  prettyCookedOptMaybe _ = prettyCookedMaybe

  -- | Pretty prints an option document
  prettyCookedMaybe :: a -> Maybe DocCooked
  prettyCookedMaybe = prettyCookedOptMaybe def

instance PrettyCookedMaybe (Maybe DocCooked) where
  prettyCookedOptMaybe _ = id

-- | Use this in the REPL as an alternative to the default 'print' function when
-- dealing with pretty-printable cooked values.
--
-- For example, @printCookedOpt def runMockChain i0 foo@
printCookedOpt :: (PrettyCooked a) => PrettyCookedOpts -> a -> IO ()
printCookedOpt opts e = PP.putDoc $ prettyCookedOpt opts e <+> PP.line

-- | Version of 'printCookedOpt' that uses default pretty printing options.
printCooked :: (PrettyCooked a) => a -> IO ()
printCooked = printCookedOpt def

-- | Use this to convert a pretty-printer to a regular show function using
-- default layout options. This is used in "Testing" because Tasty uses strings.
renderString :: (a -> DocCooked) -> a -> String
renderString printer = PP.renderString . PP.layoutPretty PP.defaultLayoutOptions . printer

-- | Print an item list with a title
--
-- >>> prettyCookedOpts opts "Foo" "-" ["bar1", "bar2", "bar3"]
-- Foo
--   - bar1
--   - bar2
--   - bar3
prettyItemize :: (PrettyCookedList a) => PrettyCookedOpts -> DocCooked -> DocCooked -> a -> DocCooked
prettyItemize opts title bullet items =
  PP.vsep
    [ title,
      PP.indent 2 $ prettyItemizeNoTitle opts bullet items
    ]

-- | Print an item list without a title
prettyItemizeNoTitle :: (PrettyCookedList a) => PrettyCookedOpts -> DocCooked -> a -> DocCooked
prettyItemizeNoTitle opts bullet docs = PP.vsep $ map (bullet <+>) $ prettyCookedOptList opts docs

-- | Print an item list with a title, but only when the list is non-empty
prettyItemizeNonEmpty :: (PrettyCookedList a) => PrettyCookedOpts -> DocCooked -> DocCooked -> a -> Maybe DocCooked
prettyItemizeNonEmpty opts _ _ (prettyCookedOptList opts -> []) = Nothing
prettyItemizeNonEmpty opts title bullet items = Just $ prettyItemize opts title bullet items

-- * Pretty printing of hashable data types

-- | Pretty prints hashable elements based on 'pcOptHashes' in the
-- 'PrettyCookedOpts'. This cannot be made an instance as it would be
-- undecidable (the hope was @(ToHash a) => PrettyCooked a@)
prettyHash :: (ToHash a) => PrettyCookedOpts -> a -> DocCooked
prettyHash
  (PrettyCookedOpts {pcOptHashes = PrettyCookedHashOpts {..}})
  (toHash -> bbs@(PlutusTx.BuiltinByteString bs)) =
    let hexRepresentation =
          PP.pretty
            . take pcOptHashLength
            . concatMap
              -- We pad the result of 'Numeric.showHex' to reach exactly
              -- 2 characters as it might only have 1 in some occasions.
              (\((`Numeric.showHex` "") -> res) -> if length res == 1 then '0' : res else res)
            . ByteString.unpack
            $ bs
     in case Map.lookup bbs pcOptHashNames of
          Nothing -> "#" <> hexRepresentation
          Just name | pcOptHashVerbose -> "#" <> hexRepresentation <+> PP.parens (PP.pretty name)
          Just name -> PP.pretty name

-- * Pretty instances for some common base types

instance PrettyCooked Int where
  prettyCookedOpt _ = PP.pretty

instance PrettyCooked Integer where
  prettyCookedOpt opts =
    if pcOptNumericUnderscores opts
      then prettyNumericUnderscore
      else PP.pretty
    where
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

instance PrettyCooked Bool where
  prettyCookedOpt _ = PP.pretty

instance PrettyCooked () where
  prettyCookedOpt _ = PP.pretty

instance PrettyCooked Rational where
  prettyCookedOpt opts q = "(" <+> prettyCookedOpt opts (numerator q) <+> "/" <+> prettyCookedOpt opts (denominator q) <+> ")"

instance PrettyCooked Text where
  prettyCookedOpt _ = PP.pretty
