{-# LANGUAGE OverloadedStrings #-}

module Cooked.MockChain.UtxoState
  ( UtxoState (..),
    UtxoDatum (..),
    prettyUtxoState,
    prettyCurrencyAndAmount,
    prettyAddressTypeAndHash,
    mPrettyValue,
  )
where

import Cooked.MockChain.Wallet
import qualified Data.List as List (intersperse)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, mapMaybe)
import qualified Ledger as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Value as Pl
import qualified PlutusTx.AssocMap as Pl
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter

-- | A 'UtxoState' provides us with the mental picture of the state of the UTxO graph.
newtype UtxoState = UtxoState {utxoState :: M.Map Pl.Address [(Pl.Value, Maybe UtxoDatum)]}

-- | A 'UtxoDatum' contains a datum whic his @Datum $ Pl.toBuiltinData x@ for some @x :: X@,
-- but we also include @show x@ to be able to print this value in a more user friendly fashion.
data UtxoDatum = UtxoDatum {utxoDatum :: Pl.Datum, utxoShow :: String}

instance Show UtxoDatum where
  show = utxoShow

instance Show UtxoState where
  show = show . prettyUtxoState

-- | Pretty prints a 'UtxoState'.
-- The entire point of producing a 'UtxoState' instead of a 'Pl.UtxoIndex' is to
-- provide the user a picture that is closer to their mental model of what is going
-- on. Hence, we must be able to display our 'UtxoState's in a human readable fashion.
prettyUtxoState :: UtxoState -> Doc ann
prettyUtxoState =
  Prettyprinter.vsep
    . List.intersperse Prettyprinter.emptyDoc
    . map (uncurry prettyAddress)
    . M.toList
    . utxoState

prettyAddress :: Pl.Address -> [(Pl.Value, Maybe UtxoDatum)] -> Doc ann
prettyAddress address payloads =
  Prettyprinter.vsep
    [ prettyAddressTypeAndHash address,
      Prettyprinter.indent 2
        . Prettyprinter.vsep
        . map (("-" <>) . Prettyprinter.indent 1)
        . mapMaybe (uncurry prettyPayload)
        $ payloads
    ]

-- Returns `Nothing` if the value is empty to avoid having an empty document
-- whose height is 1 in the `prettyprinter` library and would generate empty
-- lines.
prettyPayload :: Pl.Value -> Maybe UtxoDatum -> Maybe (Doc ann)
prettyPayload value mDatum =
  (\vs -> if null vs then Nothing else Just $ Prettyprinter.vsep vs)
    . catMaybes
    $ [ mPrettyValue value,
        (":" <>)
          . Prettyprinter.indent 1
          . Prettyprinter.pretty
          . utxoShow
          <$> mDatum
      ]

-- Returns `Nothing` if the value is empty to avoid having an empty document
-- whose height is 1, which would cause `prettyprinter` to generate empty
-- lines.
mPrettyValue :: Pl.Value -> Maybe (Doc ann)
mPrettyValue =
  ( \vs ->
      case vs of
        [] -> Nothing
        [v] -> Just v
        _ ->
          Just $
            Prettyprinter.lbrace
              <> Prettyprinter.indent 1 (Prettyprinter.vsep vs)
              <> Prettyprinter.space
              <> Prettyprinter.rbrace
  )
    . map (uncurry prettyCurrencyAndAmount)
    . Pl.toList
    . Pl.getValue

prettyCurrencyAndAmount :: Pl.CurrencySymbol -> Pl.Map Pl.TokenName Integer -> Doc ann
prettyCurrencyAndAmount symbol =
  Prettyprinter.vsep . map (uncurry prettyToken) . Pl.toList
  where
    prettySymbol :: Pl.CurrencySymbol -> Doc ann
    prettySymbol = Prettyprinter.pretty . take 7 . show

    prettyToken :: Pl.TokenName -> Integer -> Doc ann
    prettyToken name n =
      ( if symbol == Pl.CurrencySymbol ""
          then (if name == Pl.TokenName "" then "Ada" else Prettyprinter.pretty name)
          else
            Prettyprinter.parens
              ( prettySymbol symbol
                  <+> "$"
                  <+> Prettyprinter.pretty name
              )
      )
        <> ":"
        <> Prettyprinter.space
        <> Prettyprinter.pretty n

prettyAddressTypeAndHash :: Pl.Address -> Doc ann
prettyAddressTypeAndHash (Pl.Address addrCr _) =
  case addrCr of
    (Pl.ScriptCredential vh) -> prettyAux "script" vh
    (Pl.PubKeyCredential pkh) ->
      prettyAux "pubkey" pkh
        <> maybe
          Prettyprinter.emptyDoc
          ( (Prettyprinter.space <>)
              . Prettyprinter.parens
              . ("wallet #" <>)
              . Prettyprinter.pretty
          )
          (walletPKHashToId pkh)
  where
    prettyAux :: Show hash => String -> hash -> Doc ann
    prettyAux addressType hash =
      mconcat
        [ Prettyprinter.pretty addressType,
          Prettyprinter.space,
          Prettyprinter.pretty . take 7 . show $ hash
        ]
        <> Prettyprinter.colon
