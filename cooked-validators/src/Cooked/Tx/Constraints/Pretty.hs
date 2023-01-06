{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cooked.Tx.Constraints.Pretty where

import Cooked.MockChain.Misc
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints.Type
import Data.Char
import Data.Default
import Data.Either
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import qualified Ledger as Pl hiding (mintingPolicyHash, unspentOutputs, validatorHash, TxOut)
import qualified Ledger.Typed.Scripts as Pl (DatumType, TypedValidator, validatorAddress, validatorHash)
import qualified Ledger.Value as Pl
import Optics.Core
import qualified Plutus.Script.Utils.V2.Scripts as Pl (mintingPolicyHash)
import qualified Plutus.V2.Ledger.Api as Pl
import qualified PlutusTx.IsData.Class as Pl
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP
import Test.QuickCheck (NonZero)
import Test.Tasty.QuickCheck (NonZero (..))
import Data.Map (Map)

-- prettyEnum "Foo" "-" ["bar1", "bar2", "bar3"]
--    Foo
--      - bar1
--      - bar2
--      - bar3
prettyEnum :: Doc ann -> Doc ann -> [Doc ann] -> Doc ann
prettyEnum title bullet items =
  PP.vsep
    [ title,
      PP.indent 2 . PP.vsep $
        map (bullet <+>) items
    ]

prettyEnumNonEmpty :: Doc ann -> Doc ann -> [Doc ann] -> Maybe (Doc ann)
prettyEnumNonEmpty _ _ [] = Nothing
prettyEnumNonEmpty title bullet items = Just $ prettyEnum title bullet items

-- data TxSkel where
--   TxSkel ::
--     { txSkelLabel :: Set TxLabel,
--       txSkelOpts :: TxOpts,
--       txSkelMints :: TxSkelMints,
--       txSkelValidityRange :: Pl.POSIXTimeRange,
--       txSkelRequiredSigners :: Set Pl.PubKeyHash,
--       txSkelIns :: Map Pl.TxOutRef TxSkelRedeemer,
--       txSkelOuts :: [TxSkelOut],
--       txSkelFee :: Integer -- Fee in Lovelace
--     } ->
--     TxSkel
--   deriving (Show)

prettyTxSkel :: [Wallet] -> TxSkel -> Doc ann
prettyTxSkel signers (TxSkel lbl opts mints validityRange reqSigners ins outs fee) =
  -- undefined
  PP.vsep $
    "Transaction Skeleton:" :
    map
      ("-" <+>)
      ( catMaybes
          [ prettyEnumNonEmpty "Signers:" "-" (prettyPubKeyHash . walletPKHash <$> signers),
            prettyEnumNonEmpty "Labels:" "-" (PP.viaShow <$> Set.toList lbl),
            -- fmap ("Opts:" <+>) (prettyOpts opts),
            prettyEnumNonEmpty "Mints:" "-" (prettyMints <$> (mints ^. mintsListIso)),
            Just $ "Validity interval:" <+> PP.pretty validityRange,
            prettyEnumNonEmpty "Required signers:" "-" (prettyPubKeyHash <$> Set.toList reqSigners),
            -- prettyEnumNonEmpty "Inputs:" "-" <$> mapNonEmpty prettyTxSkelIn (Map.toList ins),
            prettyEnumNonEmpty "Outputs:" "-" (prettyTxSkelOut <$> outs)
          ]
      )

-- prettyPubKeyHash
--
-- If the pubkey is a know wallet
-- #abcdef (wallet 3)
--
-- Otherwise
-- #123456
--
prettyPubKeyHash :: Pl.PubKeyHash -> Doc ann
prettyPubKeyHash pkh =
  case walletPKHashToId pkh of
    Nothing -> "Pubkey" <+> prettyHash pkh
    Just walletId ->
      "Pubkey" <+> prettyHash pkh
        <+> PP.parens ("wallet" <+> PP.viaShow walletId)

-- prettyMints
--
-- Examples without and with redeemer
-- #abcdef "Foo" -> 500
-- #123456 "Bar" | Redeemer -> 1000
prettyMints :: (Pl.Versioned Pl.MintingPolicy, MintsRedeemer, Pl.TokenName, NonZero Integer) -> Doc ann
prettyMints (Pl.Versioned policy _, NoMintsRedeemer, tokenName, NonZero amount) =
  prettyMintingPolicy policy
    <+> PP.viaShow tokenName
    <+> "->"
    <+> PP.viaShow amount
prettyMints (Pl.Versioned policy _, SomeMintsRedeemer redeemer, tokenName, NonZero amount) =
  prettyMintingPolicy policy
    <+> PP.viaShow tokenName
    <+> "|"
    <+> PP.viaShow redeemer
    <+> "->"
    <+> PP.viaShow amount

prettyAddress :: Pl.Address -> Doc ann
prettyAddress (Pl.Address addrCr _stakingCred) =
  -- TODO print staking credentials
  case addrCr of
    (Pl.ScriptCredential vh) -> "script" <+> prettyHash vh
    (Pl.PubKeyCredential pkh) -> "pubkey" <+> prettyPubKeyHash pkh

prettyTxSkelOut :: TxSkelOut -> Doc ann
prettyTxSkelOut (Pays output) =
  prettyEnum
    ("Pays to" <+> prettyAddress (outputAddress output))
    "-"
    ( "Value:" <+> PP.pretty (outputValue output) :
      case outputOutputDatum output of
        Pl.OutputDatum _datum -> ["Datum (inlined):" <+> PP.viaShow (output ^. outputDatumL)]
        Pl.OutputDatumHash _datum -> ["Datum (hashed):" <+> PP.viaShow (output ^. outputDatumL)]
        Pl.NoOutputDatum -> []
    )

-- TODO
prettyTxSkelIn :: Map Pl.TxOutRef Pl.TxOut -> (Pl.TxOutRef, TxSkelRedeemer) -> Doc ann
prettyTxSkelIn managedTxOuts (txOutRef, txSkelRedeemer) = undefined

-- prettyHash 28a3d93cc3daac
-- #28a3d9
prettyHash :: (Show a) => a -> Doc ann
prettyHash = PP.pretty . ('#' :) . take 6 . show

prettyMintingPolicy :: Pl.MintingPolicy -> Doc ann
prettyMintingPolicy = prettyHash . Pl.mintingPolicyHash

-- | Prettifies a 'TxOpts'; returns 'Nothing' if we're looking at default options.
-- prettyOpts :: TxOpts -> Maybe (Doc ann)
-- prettyOpts opts = case mapMaybe cmpAgainstDefAndPrint fields of
--   [] -> Nothing
--   xs -> Just $ PP.sep $ map (PP.semi <+>) xs
--   where
--     cmpAgainstDefAndPrint :: Field TxOpts -> Maybe (Doc ann)
--     cmpAgainstDefAndPrint (Field fn f)
--       | f opts == f def = Nothing
--       | otherwise = Just $ PP.pretty fn <> PP.colon <+> PP.viaShow (f opts)

--     -- Internal: if you add fields to TxOpts, make sure to add them here.
--     fields :: [Field TxOpts]
--     fields =
--       [ Field "adjustUnbalTx" adjustUnbalTx,
--         Field "awaitTxConfirmed" awaitTxConfirmed,
--         Field "autoSlotIncrease" autoSlotIncrease,
--         Field "unsafeModTx" unsafeModTx,
--         Field "balance" balance,
--         Field "balanceOutputPolicy" balanceOutputPolicy
--       ]
