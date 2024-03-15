{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Pretty-printing options for 'prettyCookedOpt' and their default values.
module Cooked.Pretty.Options
  ( PrettyCookedOpts (..),
    PrettyCookedHashOpts (..),
    PCOptTxOutRefs (..),
    hashNamesFromList,
  )
where

import Cooked.Pretty.Hashable
import Data.Bifunctor (first)
import Data.Default
import Data.Map (Map)
import qualified Data.Map as Map
import qualified PlutusTx.Prelude as Pl

data PrettyCookedOpts = PrettyCookedOpts
  { -- | Whether to print transaction ids of validated transactions.
    -- By default: False
    pcOptPrintTxHashes :: Bool,
    -- | Whether to print transaction outputs references.
    -- By default: hidden
    pcOptPrintTxOutRefs :: PCOptTxOutRefs,
    -- | Whether to print tx options that have not been modified from their
    -- default.
    -- By default: False
    pcOptPrintDefaultTxOpts :: Bool,
    -- | Whether to print big integers with numeric underscores.
    -- For example @53_000_000@ instead of @53000000@.
    -- By default: True
    pcOptNumericUnderscores :: Bool,
    -- | Options relative to printing hashes
    pcOptHashes :: PrettyCookedHashOpts
  }
  deriving (Eq, Show)

instance Default PrettyCookedOpts where
  def =
    PrettyCookedOpts
      { pcOptPrintTxHashes = False,
        pcOptPrintTxOutRefs = PCOptTxOutRefsHidden,
        pcOptPrintDefaultTxOpts = False,
        pcOptNumericUnderscores = True,
        pcOptHashes = def
      }

-- | Whether to print transaction outputs references.
data PCOptTxOutRefs
  = -- | Hide them
    PCOptTxOutRefsHidden
  | -- | Always show them.
    -- Warning: this will disable printing similar UTxOs as a group
    -- (for instance @(×10) Lovelace: 100_000_000@)
    PCOptTxOutRefsFull
  | -- | Show them for UTxOs which are not grouped with similar others.
    -- This avoids  the downside of 'PCOptTxOutRefsFull' which disables printing
    -- UTxOs as a group.
    PCOptTxOutRefsPartial
  deriving (Eq, Show)

data PrettyCookedHashOpts = PrettyCookedHashOpts
  { -- | Length of printed hashes (e.g. addresses, transaction ids)
    -- By default: 7
    pcOptHashLength :: Int,
    -- | Association between hashes and given names to ease readability.
    -- For example @Map.singleton (walletPKHash (wallet 1)) "Alice"@
    -- By default: @Map.empty@
    pcOptHashNames :: Map Pl.BuiltinByteString String,
    -- | When a given name exists for a hash, this flag also prints the
    -- original hash after the name
    -- By default: @False@
    pcOptHashVerbose :: Bool,
    -- | Try to parse token names as hashes and, if applicable, display the
    -- associated given name.
    -- By default: @True@
    -- TODO NOT YET IMPLEMENTED
    pcOptHashParseTokenNames :: Bool
  }
  deriving (Eq, Show)

instance Default PrettyCookedHashOpts where
  def =
    PrettyCookedHashOpts
      { pcOptHashLength = 7,
        pcOptHashNames = Map.empty,
        pcOptHashVerbose = False,
        pcOptHashParseTokenNames = True
      }

hashNamesFromList :: (Hashable a) => [(a, String)] -> Map Pl.BuiltinByteString String
hashNamesFromList = Map.fromList . map (first toHash)
