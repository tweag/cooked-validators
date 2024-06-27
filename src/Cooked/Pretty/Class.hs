-- | This module provides the 'PrettyCooked' class and instances for common
-- Plutus types.  We don't rely on 'Pretty' from "Prettyprinter" in order to
-- define better printers for Plutus types which already have instances of
-- 'Pretty'. Also, 'PrettyCooked' makes it possible to optionally modify pretty
-- printing settings 'PrettyCookedOpts' (e.g. length of printed hashes).
--
-- When defining a new 'PrettyCooked' instance, prefer implementing
-- 'prettyCookedOpt' and relay the option parameter to other printers.
module Cooked.Pretty.Class
  ( PrettyCooked (..),
    printCookedOpt,
    printCooked,
  )
where

import Cooked.Pretty.Common
import Cooked.Pretty.Hashable
import Cooked.Pretty.Options
import Data.Default
import Ledger.Index qualified as Ledger
import Ledger.Scripts qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP

class PrettyCooked a where
  prettyCooked :: a -> DocCooked
  prettyCooked = prettyCookedOpt def
  prettyCookedOpt :: PrettyCookedOpts -> a -> DocCooked
  prettyCookedOpt _ = prettyCooked

-- | Use this in the REPL as an alternative to the default 'print' function when
-- dealing with pretty-printable cooked values.
--
-- For example, @printCookedOpt def runMockChain i0 foo@
printCookedOpt :: (PrettyCooked a) => PrettyCookedOpts -> a -> IO ()
printCookedOpt opts e = PP.putDoc $ prettyCookedOpt opts e <+> PP.line

-- | Version of 'printCookedOpt' that uses default pretty printing options.
printCooked :: (PrettyCooked a) => a -> IO ()
printCooked = printCookedOpt def

instance PrettyCooked Api.TxId where
  prettyCookedOpt opts = prettyHash (pcOptHashes opts) . toHash

instance PrettyCooked Api.TxOutRef where
  prettyCookedOpt opts (Api.TxOutRef txId index) =
    prettyHash (pcOptHashes opts) (toHash txId) <> "!" <> prettyCookedOpt opts index

instance PrettyCooked (Script.Versioned Script.MintingPolicy) where
  prettyCookedOpt opts = prettyHash (pcOptHashes opts) . toHash

instance PrettyCooked (Script.Versioned Script.Script) where
  prettyCookedOpt opts = prettyHash (pcOptHashes opts) . toHash

instance PrettyCooked Api.Address where
  prettyCookedOpt opts (Api.Address addrCr Nothing) = prettyCookedOpt opts addrCr
  prettyCookedOpt opts (Api.Address addrCr (Just (Api.StakingHash stakCr))) =
    prettyCookedOpt opts addrCr <+> PP.angles ("staking:" <+> prettyCookedOpt opts stakCr)
  prettyCookedOpt opts (Api.Address addrCr (Just (Api.StakingPtr p1 p2 p3))) =
    prettyCookedOpt opts addrCr <+> PP.angles ("staking:" <+> PP.pretty (p1, p2, p3))

instance PrettyCooked Api.PubKeyHash where
  prettyCookedOpt opts = prettyHash (pcOptHashes opts) . toHash

instance PrettyCooked Api.Credential where
  prettyCookedOpt opts (Api.ScriptCredential vh) = "script" <+> prettyHash (pcOptHashes opts) (toHash vh)
  prettyCookedOpt opts (Api.PubKeyCredential pkh) = "pubkey" <+> prettyCookedOpt opts pkh

instance PrettyCooked Api.Value where
  -- Example output:
  --
  -- > Value:
  -- >   - Lovelace: 45_000_000
  -- >   - Quick "hello": 3
  -- >   - #12bc3d "usertoken": 1
  --
  -- In case of an empty value (even though not an empty map):
  -- > Empty value
  prettyCookedOpt opts =
    prettySingletons
      . map prettySingletonValue
      . filter (\(_, _, n) -> n /= 0)
      . Script.flattenValue
    where
      prettySingletons :: [DocCooked] -> DocCooked
      prettySingletons [] = "Empty value"
      prettySingletons [doc] = doc
      prettySingletons docs = prettyItemize "Value:" "-" docs
      prettySingletonValue :: (Api.CurrencySymbol, Api.TokenName, Integer) -> DocCooked
      prettySingletonValue (symbol, name, amount) =
        prettyCookedOpt opts (Script.AssetClass (symbol, name)) <> ":" <+> prettyCookedOpt opts amount

instance PrettyCooked Api.CurrencySymbol where
  prettyCookedOpt opts symbol = prettyHash (pcOptHashes opts) (toHash symbol)

instance PrettyCooked Api.TokenName where
  prettyCookedOpt _ = PP.pretty

instance PrettyCooked Script.AssetClass where
  prettyCookedOpt opts (Script.AssetClass (symbol, name)) =
    prettyCookedOpt opts symbol
      <+> if symbol /= Api.CurrencySymbol ""
        then prettyCookedOpt opts name
        else mempty

instance PrettyCooked Ledger.ValidationPhase where
  prettyCookedOpt _ Ledger.Phase1 = "Phase 1"
  prettyCookedOpt _ Ledger.Phase2 = "Phase 2"

instance PrettyCooked Ledger.ValidationError where
  prettyCookedOpt opts (Ledger.TxOutRefNotFound txIn) = "TxOutRef not found" <+> prettyCookedOpt opts (Ledger.fromCardanoTxIn txIn)
  prettyCookedOpt opts (Ledger.ScriptFailure scriptError) = "Script failure" <+> prettyCookedOpt opts scriptError
  prettyCookedOpt _ (Ledger.CardanoLedgerValidationError text) = "Cardano ledger validation error " <+> PP.pretty text
  prettyCookedOpt _ Ledger.MaxCollateralInputsExceeded = "Max collateral inputs exceeded"

instance PrettyCooked Ledger.ScriptError where
  prettyCookedOpt _ (Ledger.EvaluationError text string) = "Evaluation error" <+> PP.pretty text <+> PP.pretty string
  prettyCookedOpt _ (Ledger.EvaluationException string1 string2) = "Evaluation exception" <+> PP.pretty string1 <+> PP.pretty string2

instance PrettyCooked Api.POSIXTime where
  prettyCookedOpt opts (Api.POSIXTime n) = "POSIXTime" <+> prettyCookedOpt opts n

instance PrettyCooked Ledger.ScriptHash where
  prettyCookedOpt opts = prettyHash (pcOptHashes opts) . toHash

instance (PrettyCooked a) => PrettyCooked [a] where
  prettyCookedOpt opts = prettyItemizeNoTitle "-" . map (prettyCookedOpt opts)

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

instance PrettyCooked Api.BuiltinData where
  prettyCookedOpt _ = PP.pretty
