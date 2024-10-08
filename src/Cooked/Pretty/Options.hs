-- | This module defines pretty-printing options for 'prettyCookedOpt' and their
-- default values.
module Cooked.Pretty.Options
  ( PrettyCookedOpts (..),
    PrettyCookedHashOpts (..),
    PCOptTxOutRefs (..),
    hashNamesFromList,
    defaultHashNames,
  )
where

import Cooked.Conversion.ToHash
import Cooked.Currencies (permanentCurrencySymbol, quickCurrencySymbol)
import Cooked.Wallet (wallet)
import Data.Bifunctor (first)
import Data.Default
import Data.Map (Map)
import Data.Map qualified as Map
import PlutusLedgerApi.V3 qualified as Api

data PrettyCookedOpts = PrettyCookedOpts
  { -- | Whether to print transaction ids of validated transactions. By
    -- default: False
    pcOptPrintTxHashes :: Bool,
    -- | Whether to print transaction outputs references. By default: hidden
    pcOptPrintTxOutRefs :: PCOptTxOutRefs,
    -- | Whether to print tx options that have not been modified from their
    -- default. By default: False
    pcOptPrintDefaultTxOpts :: Bool,
    -- | Whether to print big integers with numeric underscores. For example
    -- @53_000_000@ instead of @53000000@. By default: True
    pcOptNumericUnderscores :: Bool,
    -- | Options related to printing hashes
    pcOptHashes :: PrettyCookedHashOpts,
    -- | Whether to display the log
    pcOptPrintLog :: Bool
  }
  deriving (Eq, Show)

instance Default PrettyCookedOpts where
  def =
    PrettyCookedOpts
      { pcOptPrintTxHashes = False,
        pcOptPrintTxOutRefs = PCOptTxOutRefsHidden,
        pcOptPrintDefaultTxOpts = False,
        pcOptNumericUnderscores = True,
        pcOptHashes = def,
        pcOptPrintLog = True
      }

-- | Whether to print transaction outputs references.
data PCOptTxOutRefs
  = -- | Hide them
    PCOptTxOutRefsHidden
  | -- | Always show them.
    --
    -- Warning: this will disable printing similar UTxOs as a group (for
    -- instance @(×10) Lovelace: 100_000_000@)
    PCOptTxOutRefsFull
  | -- | Show them for UTxOs which are not grouped with similar others. This
    -- avoids the downside of 'PCOptTxOutRefsFull' which disables printing UTxOs
    -- as a group.
    PCOptTxOutRefsPartial
  deriving (Eq, Show)

data PrettyCookedHashOpts = PrettyCookedHashOpts
  { -- | Length of printed hash prefix. By default: 7
    pcOptHashLength :: Int,
    -- | Association between hashes and given names to ease readability.  For
    -- example @Map.singleton (walletPKHash (wallet 1)) "Alice"@ By default:
    -- "defaultHashNames" which assigns Lovelace, Quick, and Permanent as names
    -- for the associated currency symbols
    pcOptHashNames :: Map Api.BuiltinByteString String,
    -- | When a given name exists for a hash, this flag also prints the original
    -- hash after the name. By default: @False@
    pcOptHashVerbose :: Bool
  }
  deriving (Eq, Show)

instance Default PrettyCookedHashOpts where
  def =
    PrettyCookedHashOpts
      { pcOptHashLength = 7,
        pcOptHashNames = defaultHashNames,
        pcOptHashVerbose = False
      }

-- | Default hash to names map that assigns Lovelace, Quick, and Permanent to
-- the associated currency symbols. This is used as the default for the
-- pretty-printing option and is recommended to use as a basis to extend with
-- custom names.
defaultHashNames :: Map Api.BuiltinByteString String
defaultHashNames =
  hashNamesFromList
    [ (Api.CurrencySymbol "", "Lovelace"),
      (quickCurrencySymbol, "Quick"),
      (permanentCurrencySymbol, "Permanent")
    ]
    <> hashNamesFromList
      ((\i -> (wallet i, "wallet " <> show i)) <$> [1 .. 10])

-- | Smart constructor for maps to be used in the "pcOptHashNames"
-- pretty-printing option.
hashNamesFromList :: (ToHash a) => [(a, String)] -> Map Api.BuiltinByteString String
hashNamesFromList = Map.fromList . map (first toHash)
