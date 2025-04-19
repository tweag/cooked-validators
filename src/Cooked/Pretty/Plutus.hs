{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module provides 'PrettyCooked' instances of plutus types
module Cooked.Pretty.Plutus where

import Cooked.Pretty.Class
import Ledger.Index qualified as Ledger
import Ledger.Scripts qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP

-- * Pretty instances for data types coming from plutus-ledger-api

instance PrettyCooked Api.BuiltinData where
  prettyCookedOpt _ = PP.pretty

instance PrettyCooked Api.TxOutRef where
  prettyCookedOpt opts (Api.TxOutRef txId index) =
    prettyHash opts txId <> "!" <> prettyCookedOpt opts index

instance PrettyCooked Api.Address where
  prettyCookedOpt opts (Api.Address addrCr Nothing) = prettyCookedOpt opts addrCr
  prettyCookedOpt opts (Api.Address addrCr (Just (Api.StakingHash stakCr))) =
    prettyCookedOpt opts addrCr <+> PP.angles ("staking:" <+> prettyCookedOpt opts stakCr)
  prettyCookedOpt opts (Api.Address addrCr (Just (Api.StakingPtr p1 p2 p3))) =
    prettyCookedOpt opts addrCr <+> PP.angles ("staking:" <+> PP.pretty (p1, p2, p3))

instance PrettyCooked Api.Credential where
  prettyCookedOpt opts (Api.ScriptCredential vh) = "script" <+> prettyHash opts vh
  prettyCookedOpt opts (Api.PubKeyCredential pkh) = "pubkey" <+> prettyHash opts pkh

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
      . Api.flattenValue
    where
      prettySingletons :: [DocCooked] -> DocCooked
      prettySingletons [] = "Empty value"
      prettySingletons [doc] = doc
      prettySingletons docs = prettyItemize opts "Value:" "-" docs
      prettySingletonValue :: (Api.CurrencySymbol, Api.TokenName, Integer) -> DocCooked
      prettySingletonValue (symbol, name, amount) =
        prettyCookedOpt opts (Api.AssetClass (symbol, name)) <> ":" <+> prettyCookedOpt opts amount

instance PrettyCooked Api.AssetClass where
  prettyCookedOpt opts (Api.AssetClass (symbol, _)) | symbol == Api.adaSymbol = prettyHash opts symbol
  prettyCookedOpt opts (Api.AssetClass (symbol, name)) = prettyHash opts symbol <+> prettyHash opts name

instance PrettyCooked Api.POSIXTime where
  prettyCookedOpt opts (Api.POSIXTime n) = "POSIXTime" <+> prettyCookedOpt opts n

-- * Pretty instances for evalution error coming from plutus-ledger

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
