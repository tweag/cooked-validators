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
module Cooked.Pretty.Class where

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

class PrettyCooked a where
  prettyCooked :: a -> DocCooked
  prettyCooked = prettyCookedOpt def
  prettyCookedOpt :: PrettyCookedOpts -> a -> DocCooked
  prettyCookedOpt _ = prettyCooked

instance PrettyCooked Pl.TxId where
  prettyCookedOpt opts = prettyHash (pcOptPrintedHashLength opts)

instance PrettyCooked Pl.TxOutRef where
  prettyCookedOpt opts (Pl.TxOutRef txId index) =
    prettyHash (pcOptPrintedHashLength opts) txId <> "!" <> PP.pretty index

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
  -- prettyValue example output:
  --
  -- Value:
  --   - Lovelace: 45_000_000
  --   - Quick "hello": 3
  --   - #12bc3d "usertoken": 1
  --
  -- In case of an empty value (even though not an empty map):
  -- Empty value
  --
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
        prettyAssetClass <> ":" <+> prettyNumericUnderscore amount
        where
          prettyAssetClass
            | symbol == Pl.CurrencySymbol "" = "Lovelace"
            | symbol == quickCurrencySymbol = "Quick" <+> PP.pretty name
            | symbol == permanentCurrencySymbol = "Permanent" <+> PP.pretty name
            | otherwise = prettyHash (pcOptPrintedHashLength opts) symbol <+> PP.pretty name

instance PrettyCooked Pl.ValidationErrorInPhase where
  -- TODO Implement better pretty-printing for errors such as
  -- 'ValueNotPreserved'
  prettyCookedOpt _ = PP.pretty
