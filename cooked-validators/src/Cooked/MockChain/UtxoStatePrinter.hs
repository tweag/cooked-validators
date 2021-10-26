{-# LANGUAGE OverloadedStrings #-}

module Cooked.MockChain.UtxoStatePrinter (prettyUtxoState) where

import Cooked.MockChain.Base (UtxoState)
import qualified Data.List as List (intersperse)
import qualified Data.Map as Map (toList)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Ledger as Pl
import qualified Ledger.Value as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import qualified PlutusTx.AssocMap as Pl
import Prettyprinter (Doc)
import qualified Prettyprinter

prettyCurrencyAndAmount ::
  (Pl.CurrencySymbol, Pl.Map Pl.TokenName Integer) -> Doc ann
prettyCurrencyAndAmount (symbol, amountMap) =
  case (symbol, Pl.toList amountMap) of
    ("", [("", adaAmount)]) ->
      "Ada"
        <> Prettyprinter.colon
        <> Prettyprinter.space
        <> Prettyprinter.pretty adaAmount
    _ -> Prettyprinter.vsep . map (uncurry prettyToken) . Pl.toList $ amountMap
  where
    prettySymbol :: Pl.CurrencySymbol -> Doc ann
    prettySymbol = Prettyprinter.pretty . take 7 . show
    prettyToken :: Pl.TokenName -> Integer -> Doc ann
    prettyToken name n =
      Prettyprinter.parens
        ( prettySymbol symbol
            <> "‣"
            <> Prettyprinter.pretty name
        )
        <> ":"
        <> Prettyprinter.space
        <> Prettyprinter.pretty n

-- Unsafe: address carries either pubkey or validator hash but
-- the API does not expose the constructors to pattern match.
prettyAddressTypeAndHash :: Pl.Address -> Doc ann
prettyAddressTypeAndHash a =
  case Pl.toPubKeyHash a of
    Nothing ->
      case Pl.toValidatorHash a of
        Nothing -> error "Printing address: Neither pubkey nor validator hash"
        Just hash -> prettyAux "script" hash
    Just hash -> prettyAux "pubkey" hash
  where
    prettyAux :: Show hash => String -> hash -> Doc ann
    prettyAux addressType hash =
      mconcat
        [ Prettyprinter.pretty addressType,
          Prettyprinter.space,
          Prettyprinter.pretty . take 7 . show $ hash,
          Prettyprinter.colon
        ]

-- Returns `Nothing` if the value is empty to avoid having an empty document
-- whose height is 1 in the `prettyprinter` library and would generate empty
-- lines.
mPrettyValue :: Pl.Value -> Maybe (Doc ann)
mPrettyValue =
  ( \vs ->
      case vs of
        [] -> Nothing
        [v] -> Just v
        _ -> Just $ Prettyprinter.encloseSep "{" "}" "; " vs
  )
    . map prettyCurrencyAndAmount
    . Pl.toList
    . Pl.getValue

-- Returns `Nothing` if the value is empty to avoid having an empty document
-- whose height is 1 in the `prettyprinter` library and would generate empty
-- lines.
prettyPayload :: (Pl.Value, Maybe (Pl.Datum, String)) -> Maybe (Doc ann)
prettyPayload (value, mDatum) =
  (\vs -> if null vs then Nothing else Just $ Prettyprinter.vsep vs)
    . catMaybes
    $ [ mPrettyValue value,
        ("📦" <>)
          . Prettyprinter.indent 1
          . Prettyprinter.pretty
          . snd
          <$> mDatum
      ]

prettyAddress ::
  -- | Adress to show
  (Pl.Address, [(Pl.Value, Maybe (Pl.Datum, String))]) ->
  Doc ann
prettyAddress (address, payloads) =
  Prettyprinter.vsep
    [ prettyAddressTypeAndHash address,
      Prettyprinter.indent 2
        . Prettyprinter.vsep
        . map (("-" <>) . Prettyprinter.indent 1)
        . mapMaybe prettyPayload
        $ payloads
    ]

prettyUtxoState :: UtxoState -> Doc ann
prettyUtxoState =
  Prettyprinter.sep
    . List.intersperse Prettyprinter.line
    . map prettyAddress
    . Map.toList
