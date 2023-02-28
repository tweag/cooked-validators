-- | Pretty-printing options for 'prettyCookedOpt' and their default values.
module Cooked.Pretty.Options
  ( PrettyCookedOpts (..),
    PCOptTxOutRefs (..),
  )
where

import Data.Default

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
    -- | Length of printed hashes (e.g. addresses, transaction ids)
    -- By default: 7
    pcOptPrintedHashLength :: Int,
    -- | Whether to print big integers with numeric underscores.
    -- For example @53_000_000@ instead of @53000000@.
    -- By default: True
    pcOptNumericUnderscores :: Bool
  }
  deriving (Eq, Show)

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

instance Default PrettyCookedOpts where
  def =
    PrettyCookedOpts
      { pcOptPrintTxHashes = False,
        pcOptPrintTxOutRefs = PCOptTxOutRefsHidden,
        pcOptPrintDefaultTxOpts = False,
        pcOptPrintedHashLength = 7,
        pcOptNumericUnderscores = True
      }
