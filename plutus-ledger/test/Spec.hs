{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Main (main, genCardanoBuildTx, genCardanoTx) where

import Cardano.Api qualified as C
import Data.Aeson qualified as JSON
import Data.Aeson.Extras qualified as JSON
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.List (sort)
import Hedgehog (Property, forAll, fromGenT, property)
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Ledger (Slot (Slot))
import Ledger.Tx.CardanoAPI (CardanoBuildTx (CardanoBuildTx), CardanoTx (CardanoTx))
import Ledger.Tx.CardanoAPI qualified as CardanoAPI
import Ledger.Tx.CardanoAPISpec qualified
import Plutus.Script.Utils.Value qualified as Value
import PlutusLedgerApi.V1.Interval qualified as Interval
import PlutusLedgerApi.V1.Value qualified as Value
import Test.Gen.Cardano.Api.Typed qualified as Gen
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.Hedgehog (testPropertyNamed)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "all tests"
    [ testGroup
        "intervals"
        [ testPropertyNamed "member" "intvlMember," intvlMember,
          testPropertyNamed "contains" "intvlContains" intvlContains
        ],
      testGroup
        "Etc."
        [ testPropertyNamed "encodeByteString" "encodeByteStringTest," encodeByteStringTest,
          testPropertyNamed "encodeSerialise" "encodeSerialiseTest" encodeSerialiseTest
        ],
      testGroup
        "Value"
        ( [ testPropertyNamed
              "TokenName looks like escaped bytestring ToJSON/FromJSON"
              "tokenname_escaped_roundtrip"
              (jsonRoundTrip . pure $ Value.TokenName "\NUL0xc0ffee")
          ]
            ++ ( let vlJson :: BSL.ByteString
                     vlJson = "{\"getValue\":[[{\"unCurrencySymbol\":\"6d7943757272656e637953796d626f6c\"},[[{\"unTokenName\":\"myToken\"},50]]]]}"
                     vlValue = Value.singleton (Value.CurrencySymbol "myCurrencySymbol") (Value.TokenName "myToken") 50
                  in byteStringJson vlJson vlValue
               )
            ++ ( let vlJson :: BSL.ByteString
                     vlJson = "{\"getValue\":[[{\"unCurrencySymbol\":\"\"},[[{\"unTokenName\":\"\"},50]]]]}"
                     vlValue = Value.lovelace 50
                  in byteStringJson vlJson vlValue
               )
        ),
      testGroup
        "TxIn"
        [ testPropertyNamed
            "Check that Ord instances of TxIn match"
            "txInOrdInstanceEquivalenceTest"
            txInOrdInstanceEquivalenceTest
        ],
      testGroup
        "CardanoTx"
        [ testPropertyNamed "Value ToJSON/FromJSON" "genCardanoTx" (jsonRoundTrip genCardanoTx)
        ],
      Ledger.Tx.CardanoAPISpec.tests
    ]

intvlMember :: Property
intvlMember = property $ do
  (i1, i2) <-
    forAll $
      (,)
        <$> Gen.integral (fromIntegral <$> Range.linearBounded @Int)
        <*> Gen.integral (fromIntegral <$> Range.linearBounded @Int)
  let (from, to) = (min i1 i2, max i1 i2)
      i = Interval.interval (Slot from) (Slot to)
  Hedgehog.assert $ Interval.member (Slot from) i || Interval.isEmpty i
  Hedgehog.assert $ not (Interval.member (Slot (from - 1)) i) || Interval.isEmpty i
  Hedgehog.assert $ Interval.member (Slot to) i || Interval.isEmpty i
  Hedgehog.assert $ not (Interval.member (Slot (to + 1)) i) || Interval.isEmpty i

intvlContains :: Property
intvlContains = property $ do
  -- generate two intervals from a sorted list of ints
  -- the outer interval contains the inner interval
  ints <-
    forAll $
      traverse (const $ Gen.integral (fromIntegral <$> Range.linearBounded @Int)) [(1 :: Integer) .. 4]
  let [i1, i2, i3, i4] = Slot <$> sort ints
      outer = Interval.interval i1 i4
      inner = Interval.interval i2 i3

  Hedgehog.assert $ Interval.contains outer inner

encodeByteStringTest :: Property
encodeByteStringTest = property $ do
  bs <- forAll $ Gen.bytes $ Range.linear 0 1000
  let enc = JSON.String $ JSON.encodeByteString bs
      result = Aeson.iparse JSON.decodeByteString enc

  Hedgehog.assert $ result == Aeson.ISuccess bs

encodeSerialiseTest :: Property
encodeSerialiseTest = property $ do
  txt <- forAll $ Gen.text (Range.linear 0 1000) Gen.unicode
  let enc = JSON.String $ JSON.encodeSerialise txt
      result = Aeson.iparse JSON.decodeSerialise enc

  Hedgehog.assert $ result == Aeson.ISuccess txt

jsonRoundTrip :: (Show a, Eq a, JSON.FromJSON a, JSON.ToJSON a) => Hedgehog.Gen a -> Property
jsonRoundTrip gen = property $ do
  bts <- forAll gen
  let enc = JSON.toJSON bts
      result = Aeson.iparse JSON.parseJSON enc

  Hedgehog.annotateShow (result, bts)
  Hedgehog.assert $ result == Aeson.ISuccess bts

byteStringJson ::
  (Show a, Eq a, JSON.ToJSON a, JSON.FromJSON a) => BSL.ByteString -> a -> [TestTree]
byteStringJson jsonString value =
  [ testCase "decoding" $
      HUnit.assertEqual "Simple Decode" (Right value) (JSON.eitherDecode jsonString),
    testCase "encoding" $ HUnit.assertEqual "Simple Encode" jsonString (JSON.encode value)
  ]

-- | Check that Ord instances of cardano-api's 'TxIn' and plutus-ledger-api's 'TxIn' match.
txInOrdInstanceEquivalenceTest :: Property
txInOrdInstanceEquivalenceTest = property $ do
  txIns <- sort <$> forAll (Gen.list (Range.singleton 10) Gen.genTxIn)
  let toPlutus = map CardanoAPI.fromCardanoTxIn
  let plutusTxIns = sort $ toPlutus txIns
  Hedgehog.assert $ toPlutus txIns == plutusTxIns

genCardanoBuildTx :: Hedgehog.Gen CardanoBuildTx
genCardanoBuildTx = do
  tx <- Gen.genTxBodyContent C.ShelleyBasedEraConway
  let tx' =
        tx
          { C.txCertificates = C.TxCertificatesNone,
            C.txUpdateProposal = C.TxUpdateProposalNone,
            C.txAuxScripts = onlyPlutusScripts $ C.txAuxScripts tx
          }
  pure $ CardanoBuildTx tx'
  where
    onlyPlutusScripts C.TxAuxScriptsNone = C.TxAuxScriptsNone
    onlyPlutusScripts (C.TxAuxScripts p scripts) = C.TxAuxScripts p $ filter isPlutusScript scripts
    isPlutusScript (C.ScriptInEra _ C.PlutusScript {}) = True
    isPlutusScript _ = False

-- TODO Unfortunately, there's no way to get a warning if another era has been
-- added to EraInMode. Alternative way?
genCardanoTx :: Hedgehog.Gen CardanoTx
genCardanoTx =
  Gen.choice
    [ genShelleyEraInCardanoModeTx,
      genAllegraEraInCardanoModeTx,
      genMaryEraInCardanoModeTx,
      genAlonzoEraInCardanoModeTx,
      genBabbageEraInCardanoModeTx,
      genConwayEraInCardanoModeTx
    ]

genShelleyEraInCardanoModeTx :: Hedgehog.Gen CardanoTx
genShelleyEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.ShelleyBasedEraShelley
  pure $ CardanoTx tx C.ShelleyBasedEraShelley

genAllegraEraInCardanoModeTx :: Hedgehog.Gen CardanoTx
genAllegraEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.ShelleyBasedEraAllegra
  pure $ CardanoTx tx C.ShelleyBasedEraAllegra

genMaryEraInCardanoModeTx :: Hedgehog.Gen CardanoTx
genMaryEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.ShelleyBasedEraMary
  pure $ CardanoTx tx C.ShelleyBasedEraMary

genAlonzoEraInCardanoModeTx :: Hedgehog.Gen CardanoTx
genAlonzoEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.ShelleyBasedEraAlonzo
  pure $ CardanoTx tx C.ShelleyBasedEraAlonzo

genBabbageEraInCardanoModeTx :: Hedgehog.Gen CardanoTx
genBabbageEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.ShelleyBasedEraBabbage
  pure $ CardanoTx tx C.ShelleyBasedEraBabbage

genConwayEraInCardanoModeTx :: Hedgehog.Gen CardanoTx
genConwayEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.ShelleyBasedEraConway
  pure $ CardanoTx tx C.ShelleyBasedEraConway
