{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cooked.Tx.Constraints.Pretty where

import Cooked.MockChain.Misc
import Cooked.MockChain.UtxoState
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints.Type
import Data.Char
import Data.Default
import Data.Either
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import qualified Ledger as Pl hiding (mintingPolicyHash, unspentOutputs, validatorHash)
import qualified Ledger.Typed.Scripts as Pl (DatumType, TypedValidator, validatorAddress, validatorHash)
import qualified Ledger.Value as Pl
import Optics.Core
import qualified Plutus.Script.Utils.V2.Scripts as Pl (mintingPolicyHash)
import qualified PlutusTx.IsData.Class as Pl
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP
import Test.QuickCheck (NonZero)
import Test.Tasty.QuickCheck (NonZero (..))

prettyEnum :: Doc ann -> Doc ann -> [Doc ann] -> Doc ann
prettyEnum title tag items =
  PP.hang 1 $ PP.vsep $ title : map (tag <+>) items

prettyTxSkel :: [Wallet] -> TxSkel -> Doc ann
prettyTxSkel signers (TxSkel lbl opts mints validityRange reqSigners ins outs fee) =
  PP.vsep $
    "Transaction Skeleton:" :
    map
      ("-" <+>)
      ( catMaybes
          [ Just $ "Signers:" <+> PP.list (map (prettyWallet . walletPKHash) signers),
            prettyEnum "Labels:" "," <$> mapNonEmpty PP.viaShow (Set.toList lbl),
            fmap ("Opts:" <+>) (prettyOpts opts),
            prettyEnum "Mints:" "/\\" <$> mapNonEmpty prettyMints (mints ^. mintsListIso),
            Just $ "ValidateIn:" <+> PP.pretty validityRange,
            ("Required signers:" <+>) . PP.list <$> mapNonEmpty PP.viaShow (Set.toList reqSigners),
            prettyEnum "Inputs:" "/\\" <$> mapNonEmpty prettyTxSkelIn (Set.toList ins),
            prettyEnum "Outputs:" "/\\" <$> mapNonEmpty prettyTxSkelOut outs,
            Just $ "Fee:" <+> PP.pretty fee
          ]
      )
  where
    mapNonEmpty :: (a -> b) -> [a] -> Maybe [b]
    mapNonEmpty _ [] = Nothing
    mapNonEmpty f l = Just . map f $ l

prettyWallet :: Pl.PubKeyHash -> Doc ann
prettyWallet pkh =
  "wallet" <+> (maybe phash ((<+> PP.parens phash) . ("#" <>) . PP.pretty) . walletPKHashToId $ pkh)
  where
    phash = prettyHash pkh

prettyMints :: (Pl.Versioned Pl.MintingPolicy, MintsRedeemer, Pl.TokenName, NonZero Integer) -> Doc ann
prettyMints (Pl.Versioned policy _, NoMintsRedeemer, tName, NonZero amount) =
  prettyEnum
    "Mints"
    "-"
    [ "Policy:" <+> prettyMintingPolicy policy,
      "Value:" <+> prettySingletonValue (Pl.mpsSymbol . Pl.mintingPolicyHash $ policy) tName amount
    ]
prettyMints (Pl.Versioned policy _, SomeMintsRedeemer mr, tName, NonZero amount) =
  prettyEnum
    "Mints"
    "-"
    [ "Redeemer:" <+> prettyDatum mr,
      "Policy:" <+> prettyMintingPolicy policy,
      "Value:" <+> prettySingletonValue (Pl.mpsSymbol . Pl.mintingPolicyHash $ policy) tName amount
    ]

prettyTxSkelOut :: TxSkelOut -> Doc ann
prettyTxSkelOut (PaysScript val msc datum value) =
  prettyEnum
    ("PaysScript" <+> prettyAddressTypeAndHash addr)
    "-"
    ( map
        (\(d, v) -> prettyDatumVal val d v <+> "with hash:" <+> prettyHash (Pl.datumHash . Pl.Datum . Pl.toBuiltinData $ d)) -- uncurry (prettyDatumVal val))
        [(datum, value)]
    )
  where
    addr = (Pl.scriptHashAddress $ Pl.validatorHash val) {Pl.addressStakingCredential = msc}
prettyTxSkelOut (PaysPK pkh stak dat val) =
  prettyEnum
    ("PaysPK" <+> prettyWallet pkh)
    PP.emptyDoc
    ( catMaybes
        [ fmap (("StakePKH:" <+>) . PP.pretty) stak,
          fmap (("Datum:" <+>) . prettyDatum) dat,
          mPrettyValue val
        ]
    )

prettyTxSkelIn :: TxSkelIn -> Doc ann
prettyTxSkelIn (SpendsPK out) =
  let (ppAddr, mppVal) = prettyTxOut $ sOutTxOut out
   in prettyEnum "SpendsPK" "-" $ catMaybes [Just ppAddr, mppVal]
prettyTxSkelIn (SpendsScript val red spOut) =
  prettyEnum
    ("SpendsScript" <+> prettyTypedValidator val)
    "-"
    ["Redeemer:" <+> PP.viaShow red, prettyScriptOutputDatum val spOut]

prettyHash :: (Show a) => a -> Doc ann
prettyHash = PP.pretty . take 6 . show

prettyMintingPolicy :: Pl.MintingPolicy -> Doc ann
prettyMintingPolicy = prettyHash . Pl.mintingPolicyHash

prettyScriptOutputDatum ::
  forall a ann.
  (Pl.UnsafeFromData (Pl.DatumType a), Show (Pl.DatumType a)) =>
  Pl.TypedValidator a ->
  SpendableOut ->
  Doc ann
prettyScriptOutputDatum _ (SpendableOut _ chainIndexTxOut) =
  let (ppAddr, mppVal) = prettyTxOut $ fromRight undefined $ Pl.toTxOut theNetworkId chainIndexTxOut -- TODO PORT Either?
   in PP.align $
        PP.vsep $
          catMaybes
            [ Just $ "Output" <+> "at" <+> ppAddr,
              mppVal,
              case chainIndexTxOut of
                Pl.ScriptChainIndexTxOut _ _ (datumHash, maybeDatum) _ _ ->
                  case maybeDatum of
                    Nothing -> Just $ "Datum hash:" <+> prettyHash datumHash
                    Just datum ->
                      let typedDatum :: Pl.DatumType a
                          typedDatum = Pl.unsafeFromBuiltinData (Pl.getDatum datum)
                       in Just $ "Datum:" <+> prettyDatum typedDatum <+> "with hash:" <+> prettyHash datumHash
                _ -> error "Not a script output"
            ]

prettyTxOut :: Pl.TxOut -> (Doc ann, Maybe (Doc ann))
prettyTxOut tout = (prettyAddressTypeAndHash $ Pl.txOutAddress tout, mPrettyValue $ Pl.txOutValue tout)

prettyTypedValidator :: Pl.TypedValidator a -> Doc ann
prettyTypedValidator = prettyAddressTypeAndHash . Pl.validatorAddress

prettyDatumVal ::
  (Show (Pl.DatumType a)) =>
  Pl.TypedValidator a ->
  Pl.DatumType a ->
  Pl.Value ->
  Doc ann
prettyDatumVal _ d value =
  PP.align $ PP.vsep $ catMaybes [Just $ prettyDatum d, mPrettyValue value]

-- | Prettifies a 'TxOpts'; returns 'Nothing' if we're looking at default options.
prettyOpts :: TxOpts -> Maybe (Doc ann)
prettyOpts opts = case mapMaybe cmpAgainstDefAndPrint fields of
  [] -> Nothing
  xs -> Just $ PP.sep $ map (PP.semi <+>) xs
  where
    cmpAgainstDefAndPrint :: Field TxOpts -> Maybe (Doc ann)
    cmpAgainstDefAndPrint (Field fn f)
      | f opts == f def = Nothing
      | otherwise = Just $ PP.pretty fn <> PP.colon <+> PP.viaShow (f opts)

    -- Internal: if you add fields to TxOpts, make sure to add them here.
    fields :: [Field TxOpts]
    fields =
      [ Field "adjustUnbalTx" adjustUnbalTx,
        Field "awaitTxConfirmed" awaitTxConfirmed,
        Field "autoSlotIncrease" autoSlotIncrease,
        Field "unsafeModTx" unsafeModTx,
        Field "balance" balance,
        Field "balanceOutputPolicy" balanceOutputPolicy
      ]

data Field record where
  Field :: (Show x, Eq x) => String -> (record -> x) -> Field record

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
    isHashChar c = 'a' <= c && c <= 'f' || isDigit c

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
