{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | We provide the 'PrettyCooked' class and instances for common Plutus types.
-- We don't rely on 'Pretty' from "Prettyprinter" in order to define better
-- printers for Plutus types which already have instances of 'Pretty'. Also,
-- 'PrettyCooked' makes it possible to optionally modify pretty printing
-- settings 'PrettyCookedOpts' (e.g. length of printed hashes).
--
-- When defining a new 'PrettyCooked' instance, prefer implementing
-- 'prettyCookedOpt' and relay the option parameter to other printers.
module Cooked.Pretty.Class
  ( PrettyCooked (..),
    printCookedOpt,
    printCooked,
  )
where

import Cooked.Currencies (permanentCurrencySymbol, quickCurrencySymbol)
import Cooked.Pretty.Common
import Cooked.Pretty.Options
import Cooked.Wallet
import Data.Default
import qualified Ledger.Index as Pl
import qualified Plutus.Script.Utils.Scripts as Pl
import qualified Plutus.Script.Utils.Value as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP

class PrettyCooked a where
  prettyCooked :: a -> DocCooked
  prettyCooked = prettyCookedOpt def
  prettyCookedOpt :: PrettyCookedOpts -> a -> DocCooked
  prettyCookedOpt _ = prettyCooked

-- | Use this in the REPL as an alternative to the default 'print' function
-- when dealing with pretty-printable cooked values.
--
-- For example, @printCookedOpt def runMockChain i0 foo@
printCookedOpt :: PrettyCooked a => PrettyCookedOpts -> a -> IO ()
printCookedOpt opts e = PP.putDoc $ prettyCookedOpt opts e <+> PP.line

-- | Version of 'printCookedOpt' that uses default pretty printing options.
printCooked :: PrettyCooked a => a -> IO ()
printCooked = printCookedOpt def

instance PrettyCooked Pl.TxId where
  prettyCookedOpt opts = prettyHash (pcOptPrintedHashLength opts)

instance PrettyCooked Pl.TxOutRef where
  prettyCookedOpt opts (Pl.TxOutRef txId index) =
    prettyHash (pcOptPrintedHashLength opts) txId <> "!" <> prettyCookedOpt opts index

instance PrettyCooked (Pl.Versioned Pl.MintingPolicy) where
  prettyCookedOpt opts = prettyHash (pcOptPrintedHashLength opts) . Pl.mintingPolicyHash

instance PrettyCooked Pl.Address where
  prettyCookedOpt opts (Pl.Address addrCr Nothing) = prettyCookedOpt opts addrCr
  prettyCookedOpt opts (Pl.Address addrCr (Just (Pl.StakingHash stakCr))) =
    prettyCookedOpt opts addrCr <+> PP.angles ("staking:" <+> prettyCookedOpt opts stakCr)
  prettyCookedOpt opts (Pl.Address addrCr (Just (Pl.StakingPtr p1 p2 p3))) =
    prettyCookedOpt opts addrCr <+> PP.angles ("staking:" <+> PP.pretty (p1, p2, p3))

instance PrettyCooked Pl.PubKeyHash where
  -- If the pubkey is a known wallet
  -- #abcdef (wallet 3)
  --
  -- Otherwise
  -- #123456
  --
  prettyCookedOpt opts pkh =
    case walletPKHashToId pkh of
      Nothing -> prettyHash (pcOptPrintedHashLength opts) pkh
      Just walletId ->
        prettyHash (pcOptPrintedHashLength opts) pkh
          <+> PP.parens ("wallet" <+> PP.viaShow walletId)

instance PrettyCooked Pl.Credential where
  prettyCookedOpt opts (Pl.ScriptCredential vh) = "script" <+> prettyHash (pcOptPrintedHashLength opts) vh
  prettyCookedOpt opts (Pl.PubKeyCredential pkh) = "pubkey" <+> prettyCookedOpt opts pkh

instance PrettyCooked Pl.Value where
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
      . Pl.flattenValue
    where
      prettySingletons :: [DocCooked] -> DocCooked
      prettySingletons [] = "Empty value"
      prettySingletons [doc] = doc
      prettySingletons docs = prettyItemize "Value:" "-" docs
      prettySingletonValue :: (Pl.CurrencySymbol, Pl.TokenName, Integer) -> DocCooked
      prettySingletonValue (symbol, name, amount) =
        prettyAssetClass <> ":" <+> prettyCookedOpt opts amount
        where
          prettyAssetClass
            | symbol == Pl.CurrencySymbol "" = "Lovelace"
            | symbol == quickCurrencySymbol = "Quick" <+> PP.pretty name
            | symbol == permanentCurrencySymbol = "Permanent" <+> PP.pretty name
            | otherwise = prettyHash (pcOptPrintedHashLength opts) symbol <+> PP.pretty name

instance PrettyCooked Pl.ValidationErrorInPhase where
  -- In Plutus V2, most errors no longer have dedicated constructors we can
  -- pattern match on, they are mostly wrapped as text which makes it difficult
  -- to improve upon default printing.
  prettyCookedOpt _ = PP.pretty

instance PrettyCooked Pl.POSIXTime where
  prettyCookedOpt opts (Pl.POSIXTime n) = "POSIXTime" <+> prettyCookedOpt opts n

instance PrettyCooked a => PrettyCooked [a] where
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
