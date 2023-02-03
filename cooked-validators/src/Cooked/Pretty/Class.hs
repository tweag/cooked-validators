-- | We provide the 'PrettyCooked' class and instances for common Plutus types.
-- We don't rely on 'Pretty' from "Prettyprinter" in order to define better
-- printers for Plutus types which already have instances of 'Pretty'. Also,
-- 'PrettyCooked' makes it possible to optionally modify pretty printing
-- settings 'PrettyCookedOpts' (e.g. length of printed hashes).
-- You just need to implement either 'prettyCooked' or 'prettyCookedOpt'.
module Cooked.Pretty.Class where

import Data.Default
import Prettyprinter (Doc)

type DocCooked = Doc ()

data PrettyCookedOpts = PrettyCookedOpts
  { -- | Whether to print transaction ids of validated transactions.
    -- By default: False
    pcOptPrintTxHashes :: Bool,
    -- | Whether to print tx options that have not been modified from their
    -- default.
    -- By default: False
    pcOptPrintDefaultTxOpts :: Bool,
    -- | Length of printed hashes (e.g. addresses, tx ids)
    -- By default: 7
    pcOptPrintedHashLength :: Int
  }
  deriving (Eq, Show)

instance Default PrettyCookedOpts where
  def =
    PrettyCookedOpts
      { pcOptPrintTxHashes = False,
        pcOptPrintDefaultTxOpts = False,
        pcOptPrintedHashLength = 7
      }

class PrettyCooked a where
  prettyCooked :: a -> DocCooked
  prettyCooked = prettyCookedOpt def
  prettyCookedOpt :: PrettyCookedOpts -> a -> DocCooked
  prettyCookedOpt _ = prettyCooked
