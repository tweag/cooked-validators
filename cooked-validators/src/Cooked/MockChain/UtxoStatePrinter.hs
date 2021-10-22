{-# LANGUAGE OverloadedStrings #-}

module Cooked.MockChain.UtxoStatePrinter (showUtxoState) where

import Cooked.MockChain.Base (UtxoState)
import Data.List (intercalate)
import qualified Data.Map as Map (toList)
import Data.Proxy (Proxy)
import qualified Ledger as Pl
import qualified Ledger.Value as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import qualified PlutusPrelude as Pl (pretty)
import qualified PlutusTx.AssocMap as Pl

showTokenValue :: (Pl.CurrencySymbol, Pl.Map Pl.TokenName Integer) -> String
showTokenValue (symb, amountMap) =
  case (symb, Pl.toList amountMap) of
    ("", [("", adaAmount)]) -> "Ada: " <> show (Pl.pretty adaAmount)
    (_, tokenValueMap) ->
      show (Pl.pretty symb) <> ": " <> show (Pl.pretty tokenValueMap)

-- Partial function here: address carries either pubkey or validator hash but
-- the API does not expose the constructors to pattern match.
showAddressTypeAndHash :: Pl.Address -> String
showAddressTypeAndHash a =
  case Pl.toPubKeyHash a of
    Nothing ->
      case Pl.toValidatorHash a of
        Nothing -> error "Printing address: Neither pubkey nor validator hash"
        Just hash -> "script " <> show (Pl.pretty hash)
    Just hash -> "pubkey " <> show (Pl.pretty hash)

showValue :: Pl.Value -> String
showValue =
  (\s -> if null s then "" else "{ " <> s <> " }\n")
    . intercalate "; "
    . map showTokenValue
    . Pl.toList
    . Pl.getValue

showDatum ::
  (Show a, Pl.UnsafeFromData a) =>
  -- | Proxy carrying the datum type
  Proxy a ->
  -- | Raw Plutus datum to show
  Pl.Datum ->
  String
showDatum proxy = show . convert proxy . Pl.getDatum
  where
    convert :: Pl.UnsafeFromData a => Proxy a -> Pl.BuiltinData -> a
    convert _proxy = Pl.unsafeFromBuiltinData

showPayload ::
  (Show a, Pl.UnsafeFromData a) =>
  -- | Proxy carrying the datum type
  Proxy a ->
  (Pl.Value, Maybe Pl.Datum) ->
  String
showPayload proxy (value, mDatum) =
  showValue value
    <> maybe "" ((\s -> "(" <> s <> ")") . showDatum proxy) mDatum

showAddress ::
  (Show a, Pl.UnsafeFromData a) =>
  -- | Proxy carrying the datum type
  Proxy a ->
  -- | Adress to show
  (Pl.Address, [(Pl.Value, Maybe Pl.Datum)]) ->
  String
showAddress proxy (address, payloads) =
  showAddressTypeAndHash address
    <> ":\n"
    <> concatMap ((\s -> "- " <> s <> "\n") . showPayload proxy) payloads

showUtxoState ::
  (Show a, Pl.UnsafeFromData a) =>
  -- | Proxy carrying the datum type
  Proxy a ->
  -- | UtxoState to show
  UtxoState ->
  String
showUtxoState proxy = concatMap (showAddress proxy) . Map.toList
