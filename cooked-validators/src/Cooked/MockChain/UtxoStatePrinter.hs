{-# LANGUAGE OverloadedStrings #-}

module Cooked.MockChain.UtxoStatePrinter (showUtxoState) where

import Cooked.MockChain.Base (UtxoState)
import Data.List (intercalate)
import qualified Data.Map as Map (toList)
import qualified Ledger.Address as Pl
import qualified Ledger.Scripts as Pl
import qualified Ledger.Value as Pl
import qualified PlutusTx.AssocMap as Pl

showTokenValue :: (Pl.CurrencySymbol, Pl.Map Pl.TokenName Integer) -> String
showTokenValue (symb, amountMap) =
  case (symb, Pl.toList amountMap) of
    ("", [("", adaAmount)]) -> "Ada: " <> show adaAmount
    (_, tokenValueMap) -> show symb <> ": " <> show tokenValueMap

-- Partial function here: address carries either pubkey or validator hash but
-- the API does not expose the constructors to pattern match.
showAddressTypeAndHash :: Pl.Address -> String
showAddressTypeAndHash a =
  case Pl.toPubKeyHash a of
    Nothing ->
      case Pl.toValidatorHash a of
        Nothing -> error "Printing address: Neither pubkey nor validator hash"
        Just hash -> "script " <> show hash
    Just hash -> "pubkey " <> show hash

showValue :: Pl.Value -> String
showValue =
  (\s -> if null s then "" else "{ " <> s <> " }\n")
    . intercalate "; "
    . map showTokenValue
    . Pl.toList
    . Pl.getValue

showPayload :: (Pl.Value, Maybe Pl.Datum) -> String
showPayload (value, mDatum) =
  showValue value <> maybe "" ((\s -> "(" <> s <> ")") . show) mDatum

showAddress :: (Pl.Address, [(Pl.Value, Maybe Pl.Datum)]) -> String
showAddress (address, payloads) =
  showAddressTypeAndHash address
    <> ":\n"
    <> concatMap ((\s -> "- " <> s <> "\n") . showPayload) payloads

showUtxoState :: UtxoState -> String
showUtxoState = concatMap showAddress . Map.toList
