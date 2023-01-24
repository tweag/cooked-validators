module Cooked.Pretty.Class where

import Data.Default
import Prettyprinter (Doc)

type DocCooked = Doc ()

data PrettyCookedOpts = PrettyCookedOpts
  { pcOptPrintTxHashes :: Bool,
    pcOptPrintTxOutHashes :: Bool,
    pcOptPrintDefaultTxOpts :: Bool
  }

instance Default PrettyCookedOpts where
  def =
    PrettyCookedOpts
      { pcOptPrintTxHashes = False,
        pcOptPrintTxOutHashes = False,
        pcOptPrintDefaultTxOpts = False
      }

class PrettyCooked a where
  prettyCooked :: a -> DocCooked
  prettyCooked = prettyCookedOpt def
  prettyCookedOpt :: PrettyCookedOpts -> a -> DocCooked
  prettyCookedOpt _ = prettyCooked
