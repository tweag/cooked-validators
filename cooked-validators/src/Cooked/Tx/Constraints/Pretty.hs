{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cooked.Tx.Constraints.Pretty where

import Cooked.MockChain.UtxoState
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints.Type
import Data.Char
import Data.Maybe (catMaybes)
import qualified Ledger as Pl hiding (unspentOutputs)
import qualified Ledger.Typed.Scripts as Pl (DatumType, TypedValidator, validatorScript)
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP

prettyEnum :: Doc ann -> Doc ann -> [Doc ann] -> Doc ann
prettyEnum title tag items =
  PP.hang 1 $ PP.vsep $ title : map (tag <+>) items

prettyTxSkel :: TxSkel -> Doc ann
prettyTxSkel (TxSkel lbl signer constr) =
  PP.vsep $
    map ("-" <+>) $
      catMaybes
        [ Just $ "Signer:" <+> prettyWallet (walletPKHash signer),
          fmap (("Label:" <+>) . prettyDatum) lbl,
          Just $ prettyEnum "Constraints:" "/\\" (map prettyConstraint constr)
        ]

prettyWallet :: Pl.PubKeyHash -> Doc ann
prettyWallet pkh =
  "wallet" <+> (maybe phash ((<+> PP.parens phash) . ("#" <>) . PP.pretty) . walletPKHashToId $ pkh)
  where
    phash = prettyHash pkh

prettyConstraint :: Constraint -> Doc ann
prettyConstraint (PaysScript val outs) =
  prettyEnum ("PaysScript" <+> prettyTypedValidator val) "-" (map (uncurry (prettyDatumVal val)) outs)
prettyConstraint (SpendsScript val red outdat) =
  prettyEnum
    ("SpendsScript" <+> prettyTypedValidator val)
    "-"
    ["Redeemer:" <+> PP.viaShow red, prettyOutputDatum val outdat]
prettyConstraint (PaysPK pkh val) =
  prettyEnum ("PaysPK" <+> prettyWallet pkh) PP.emptyDoc (catMaybes [mPrettyValue val])
prettyConstraint (SpendsPK out) =
  let (ppAddr, mppVal) = prettyTxOut $ Pl.toTxOut $ snd out
   in prettyEnum "SpendsPK" "-" $ catMaybes [Just ppAddr, mppVal]
prettyConstraint (Mints Nothing policies val) =
  prettyEnum "Mints" "-" $
    catMaybes
      [ mPrettyValue val,
        Just $ "Policies:" <+> PP.list (map prettyMintingPolicy policies)
      ]
prettyConstraint (Mints (Just r) policies val) =
  prettyEnum "Mints" "-" $
    catMaybes
      [ mPrettyValue val,
        Just $ "With the redeemer" <+> prettyDatum r,
        Just $ "Policies:" <+> PP.list (map prettyMintingPolicy policies)
      ]
prettyConstraint (SignedBy ws) =
  prettyEnum "SignedBy" "-" $ map (prettyWallet . walletPKHash) ws
prettyConstraint _ = "<constraint without pretty def>"

prettyHash :: (Show a) => a -> Doc ann
prettyHash = PP.pretty . take 6 . show

prettyMintingPolicy :: Pl.MintingPolicy -> Doc ann
prettyMintingPolicy = prettyHash . Pl.mintingPolicyHash

prettyOutputDatum :: (Show (Pl.DatumType a)) => Pl.TypedValidator a -> (SpendableOut, Pl.DatumType a) -> Doc ann
prettyOutputDatum _ (out, dat) =
  let (ppAddr, mppVal) = prettyTxOut $ Pl.toTxOut $ snd out
   in PP.align $
        PP.vsep $
          catMaybes
            [Just $ "Output" <+> "at" <+> ppAddr, mppVal, Just $ "Datum:" <+> prettyDatum dat]

prettyTxOut :: Pl.TxOut -> (Doc ann, Maybe (Doc ann))
prettyTxOut tout = (prettyAddressTypeAndHash $ Pl.txOutAddress tout, mPrettyValue $ Pl.txOutValue tout)

prettyTypedValidator :: Pl.TypedValidator a -> Doc ann
prettyTypedValidator = prettyAddressTypeAndHash . Pl.scriptAddress . Pl.validatorScript

prettyDatumVal ::
  (Show (Pl.DatumType a)) =>
  Pl.TypedValidator a ->
  Pl.DatumType a ->
  Pl.Value ->
  Doc ann
prettyDatumVal _ d value =
  PP.align $ PP.vsep $ catMaybes [Just $ prettyDatum d, mPrettyValue value]

-- * Shortening Hashes from Show Instances

-- | The 'prettyDatum' relies on Haskell's default show implementation,
--  but it display shortened hashes. It is a big hack to detect hashes and
--  it works in the vast majority of cases. It might shorten a string that
--  /is not/ a hash iff such string has more than 24 hexadecimal characters,
--  which arguably, is pretty unlikely for something that is not a hash.
prettyDatum :: (Show a) => a -> Doc ann
prettyDatum = PP.align . prettyWordPunct . map fixHashes . words' . show
  where
    -- TODO: It might be worthwhile to make a little parser for haskell records
    -- and actually prettify the output. Legibility is a massively important
    -- factor for us to be able to diagnose tests that go wrong.
    isHashChar c = ('a' <= c && c <= 'f') || isDigit c

    fixHashes :: WordPunct -> WordPunct
    fixHashes (Punct str) = Punct str
    fixHashes (Word str)
      | length str < 24 = Word str
      | all isHashChar str = Word $ take 6 str
      | otherwise = Word str

    words' :: String -> [WordPunct]
    words' = concatMap splitPunct . words

    splitPunct :: String -> [WordPunct]
    splitPunct [] = []
    splitPunct s =
      let (p, s') = span isPunctuation s
          (w, s'') = break isPunctuation s'
       in appP p $ appW w $ splitPunct s''
      where
        appP [] = id
        appP x = (Punct x :)

        appW [] = id
        appW x = (Word x :)

    prettyWordPunct :: [WordPunct] -> Doc ann
    prettyWordPunct = go
      where
        go (Word w : Word y : zs) = PP.pretty w <+> go (Word y : zs)
        go (Word w : Punct y : zs)
          | w == "=" = PP.pretty w <+> go (Punct y : zs)
          | otherwise = PP.pretty w <> go (Punct y : zs)
        go (Punct w : zs) = PP.pretty w <> go zs
        go [Word w] = PP.pretty w
        go [] = PP.emptyDoc

data WordPunct = Word String | Punct String
  deriving (Show)
