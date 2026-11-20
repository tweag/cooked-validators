module Spec.Serialization where

import Control.Exception (IOException, try)
import Control.Monad ((>=>))
import Cooked
import Data.Aeson (FromJSON, ToJSON, Value (..), eitherDecode, eitherDecodeFileStrict, encode)
import Data.Aeson.Types (parseEither)
import Data.Default (def)
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.String (fromString)
import Plutus.InlineDatums
import Plutus.MultiPurpose
import Plutus.ProposingScript
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Prettyprinter qualified as PP
import Spec.InlineDatums ()
import Spec.Serialization.DatumA qualified as A
import Spec.Serialization.DatumB qualified as B
import System.IO.Extra (captureOutput, withTempDir)
import Test.Tasty
import Test.Tasty.HUnit

-- | A trace producing a chain index with consumed outputs, typed datums of
-- every kind, a reference script and a constitution script.
serializationTrace :: DirectMockChain ()
serializationTrace = do
  setConstitutionScript alwaysTrueProposingValidator
  validateTxSkel_
    txSkelEmulatorTemplate
      { txSkelOutputs =
          [ requireInlineDatumInInputValidator `receives` InlineDatum FirstPaymentDatum,
            requireInlineDatumInInputValidator `receives` VisibleHashedDatum SecondPaymentDatum,
            requireHashedDatumInInputValidator
              `receives` HiddenHashedDatum FirstPaymentDatum
              <&&> ReferenceScript requireInlineDatumInInputValidator
          ],
        txSkelSignatories = txSkelSignatoriesFromList [wallet 1]
      }

-- | The chain index resulting from 'serializationTrace', alongside the
-- returned value of the trace.
serializationRun :: (ChainIndex, Either ChainError ())
serializationRun =
  head $
    runMockChainFromConf
      mockChainConfTemplate
        { mccInitialDistribution = initialDistributionTemplate,
          mccFunOnResult = \(_, (chainIndex, (_, res))) -> (chainIndex, res)
        }
      serializationTrace

-- | The chain index resulting from 'serializationTrace'
traceChainIndex :: ChainIndex
traceChainIndex = fst serializationRun

-- | The default registries, extended with the datum type used in the trace
testRegistries :: Registries
testRegistries = extendDatumRegistry (mkRegistry [Codec @SimpleContractDatum]) defaultRegistries

-- | Encodes a chain index to bytes and decodes it back, using the given
-- registries.
roundTrip :: Registries -> ChainIndex -> Either String ChainIndex
roundTrip registries =
  (eitherDecode . encode . chainIndexToJSON)
    >=> parseEither (chainIndexFromJSON registries)

-- | Ensures the given action throws an 'IOException'
assertIOFails :: IO a -> Assertion
assertIOFails action = do
  res <- try @IOException action
  assertBool "Expected an IO exception" (isLeft res)

-- | Ensures a decoding failed with an error containing the given message
assertDecodingFails :: String -> Either String a -> Assertion
assertDecodingFails msg (Left err) = assertBool err $ msg `isInfixOf` err
assertDecodingFails _ (Right _) = assertFailure "Expected a deserialization failure"

-- | A datum type with a custom registration key
newtype KeyedDatum = KeyedDatum Integer
  deriving newtype (Show, Eq, Api.ToData, Api.FromData)

instance PrettyCooked KeyedDatum where
  prettyCooked = PP.viaShow

instance HasTypeKey KeyedDatum where
  typeKey = "keyed-datum"

-- | Replaces every occurrence of a given JSON string by another one
replaceString :: String -> String -> Value -> Value
replaceString from to = go
  where
    go (String s) | s == fromString from = String (fromString to)
    go (Object o) = Object (go <$> o)
    go (Array a) = Array (go <$> a)
    go v = v

-- | A script type which is not part of the default registries
newtype UnknownScript = UnknownScript (Script.Versioned Script.Validator)
  deriving newtype (ToJSON, FromJSON)

instance HasTypeKey UnknownScript

instance Script.ToVersioned Script.Script UnknownScript where
  toVersioned (UnknownScript script) = Script.toVersioned script

-- | A pubkey type which is not part of the default registries
newtype UnknownPubKey = UnknownPubKey Api.PubKeyHash
  deriving newtype (ToJSON, FromJSON)

instance HasTypeKey UnknownPubKey

instance Script.ToPubKeyHash UnknownPubKey where
  toPubKeyHash (UnknownPubKey pkh) = pkh

-- | A chain index holding a single output
singletonChainIndex :: TxSkelOut -> ChainIndex
singletonChainIndex out = ChainIndex (Map.singleton (Api.TxOutRef (Api.TxId mempty) 0) (out, True)) Nothing

-- | Replaces the owner of an output
ownedBy :: TxSkelOut -> User IsEither Allocation -> TxSkelOut
ownedBy out owner = out {txSkelOutOwner = owner}

-- | A manually built chain index covering every owner type and datum kind
-- supported by the default registries, staking credentials and values which
-- are not subject to min ada adjustment.
manualChainIndex :: ChainIndex
manualChainIndex =
  ChainIndex
    ( Map.fromList $
        zip
          [Api.TxOutRef (Api.TxId mempty) i | i <- [0 ..]]
          [ (wallet 1 `receives` AdaValue 3, True),
            ( Script.toPubKeyHash (wallet 2)
                `receives` LovelaceValue 2_000_000
                <&&> StakingCredential (Api.StakingHash (Api.PubKeyCredential (Script.toPubKeyHash (wallet 3)))),
              False
            ),
            ((wallet 3 `receives` AdaValue 2) `ownedBy` UserPubKey (TxSkelSignatory (Script.toPubKeyHash (wallet 3)) Nothing), True),
            (mpScript (Api.TxId mempty) `receives` AdaValue 2, True),
            (alwaysTrueProposingValidator `receives` AdaValue 2, False),
            (Script.toScriptHash requireInlineDatumInInputValidator `receives` InlineDatum (Api.toBuiltinData @Integer 42), True),
            ((wallet 4 `receives` AdaValue 2) {txSkelOutDatum = SomeTxSkelOutDatumHash (Api.DatumHash mempty)}, True)
          ]
    )
    (Just $ toVScript alwaysTrueProposingValidator)

-- | Location of a serialized version of 'manualChainIndex', used to detect
-- changes in the serialization format.
goldenFile :: FilePath
goldenFile = "tests/golden/chain-index.json"

tests :: TestTree
tests =
  testGroup
    "Serialization"
    [ testGroup
        "JSON round trip"
        [ testCase "The trace succeeds and produces a rich chain index" $ do
            either (assertFailure . show) return $ snd serializationRun
            let outputs = Map.elems $ chainIndexOutputs traceChainIndex
            assertBool "Missing consumed outputs" $ not (all snd outputs)
            assertBool "Missing reference script" $ any (isJust . txSkelOutReferenceScript . fst) outputs
            assertBool "Missing constitution" $ isJust $ chainIndexConstitution traceChainIndex,
          testCase "Empty chain index" $
            roundTrip defaultRegistries def @?= Right def,
          testCase "Chain index from a mockchain run" $
            roundTrip testRegistries traceChainIndex @?= Right traceChainIndex,
          -- Equality on datums and users does not account for their types, so
          -- we also compare the serialized forms, which contain type names.
          testCase "Types are preserved" $
            chainIndexToJSON
              <$> roundTrip testRegistries traceChainIndex
              @?= Right (chainIndexToJSON traceChainIndex),
          testCase "Manual chain index with all supported owners and datums" $
            roundTrip defaultRegistries manualChainIndex @?= Right manualChainIndex,
          testCase "Types are preserved in the manual chain index" $
            chainIndexToJSON
              <$> roundTrip defaultRegistries manualChainIndex
              @?= Right (chainIndexToJSON manualChainIndex)
        ],
      testGroup
        "Registries"
        [ testCase "Unregistered datum types are rejected" $
            assertDecodingFails "unknown datum type" $
              roundTrip defaultRegistries traceChainIndex,
          testCase "Unregistered script types are rejected" $
            assertDecodingFails "unknown script type" $
              roundTrip defaultRegistries $
                singletonChainIndex $
                  (wallet 1 `receives` AdaValue 2) `ownedBy` UserScript (UnknownScript requireInlineDatumInInputValidator),
          testCase "Unregistered pubkey types are rejected" $
            assertDecodingFails "unknown pubkey type" $
              roundTrip defaultRegistries $
                singletonChainIndex $
                  (wallet 1 `receives` AdaValue 2) `ownedBy` UserPubKey (UnknownPubKey (Script.toPubKeyHash (wallet 1))),
          testCase "Registered script and pubkey types are accepted" $
            let index =
                  ChainIndex
                    ( Map.fromList
                        [ (Api.TxOutRef (Api.TxId mempty) 0, ((wallet 1 `receives` AdaValue 2) `ownedBy` UserScript (UnknownScript requireInlineDatumInInputValidator), True)),
                          (Api.TxOutRef (Api.TxId mempty) 1, ((wallet 1 `receives` AdaValue 2) `ownedBy` UserPubKey (UnknownPubKey (Script.toPubKeyHash (wallet 1))), True))
                        ]
                    )
                    Nothing
                registries =
                  extendPubKeyRegistry (mkRegistry [Codec @UnknownPubKey]) $
                    extendScriptRegistry (mkRegistry [Codec @UnknownScript]) defaultRegistries
             in chainIndexToJSON <$> roundTrip registries index @?= Right (chainIndexToJSON index),
          testCase "Datums incompatible with their registered type are rejected" $
            assertDecodingFails "failed to deserialize datum" $
              parseEither (chainIndexFromJSON (extendDatumRegistry (mkRegistry [Codec @B.Datum]) defaultRegistries)) $
                replaceString (typeKey @A.Datum) (typeKey @B.Datum) $
                  chainIndexToJSON $
                    singletonChainIndex $
                      wallet 1 `receives` InlineDatum (A.Datum 42),
          testCase "Homonymous datum types from different modules do not collide" $
            let index =
                  ChainIndex
                    ( Map.fromList
                        [ (Api.TxOutRef (Api.TxId mempty) 0, (wallet 1 `receives` InlineDatum (A.Datum 42), True)),
                          (Api.TxOutRef (Api.TxId mempty) 1, (wallet 1 `receives` InlineDatum (B.Datum "foo"), True))
                        ]
                    )
                    Nothing
                registries = extendDatumRegistry (mkRegistry [Codec @A.Datum, Codec @B.Datum]) defaultRegistries
             in chainIndexToJSON <$> roundTrip registries index @?= Right (chainIndexToJSON index),
          testCase "Custom type keys are used for serialization and registration" $
            let index = singletonChainIndex $ wallet 1 `receives` InlineDatum (KeyedDatum 42)
                registries = extendDatumRegistry (mkRegistry [Codec @KeyedDatum]) defaultRegistries
             in do
                  let json = show $ encode $ chainIndexToJSON index
                  assertBool "the custom key is serialized" $ "keyed-datum" `isInfixOf` json
                  chainIndexToJSON <$> roundTrip registries index @?= Right (chainIndexToJSON index)
        ],
      testGroup
        "Golden"
        [ testCase "The golden file can still be loaded" $ do
            res <- chainIndexFromSource $ ChainIndexFromFile goldenFile defaultRegistries False False
            res @?= manualChainIndex,
          testCase "The golden file matches the current serialization" $ do
            golden <- eitherDecodeFileStrict @Value goldenFile
            golden @?= Right (chainIndexToJSON manualChainIndex)
        ],
      testGroup
        "Files"
        [ testCase "Saving then loading" $ withTempDir $ \dir -> do
            let path = dir <> "/index.json"
            saveChainIndex path traceChainIndex
            res <- chainIndexFromSource $ ChainIndexFromFile path testRegistries False False
            res @?= traceChainIndex,
          testCase "Saving keeps a backup of the previous file" $ withTempDir $ \dir -> do
            let path = dir <> "/index.json"
            saveChainIndex path def
            saveChainIndex path traceChainIndex
            backup <- chainIndexFromSource $ ChainIndexFromFile (path <> ".bak") testRegistries False False
            backup @?= def
            current <- chainIndexFromSource $ ChainIndexFromFile path testRegistries False False
            current @?= traceChainIndex,
          testCase "Missing file is allowed" $ withTempDir $ \dir -> do
            (out, res) <- captureOutput $ chainIndexFromSource $ ChainIndexFromFile (dir <> "/none.json") testRegistries True False
            res @?= def
            assertBool out $ "does not exist" `isInfixOf` out,
          testCase "Missing file is rejected" $ withTempDir $ \dir ->
            assertIOFails $ chainIndexFromSource $ ChainIndexFromFile (dir <> "/none.json") testRegistries False False,
          testCase "Malformed file is allowed" $ withTempDir $ \dir -> do
            let path = dir <> "/index.json"
            writeFile path "not a chain index"
            (out, res) <- captureOutput $ chainIndexFromSource $ ChainIndexFromFile path testRegistries False True
            res @?= def
            assertBool out $ "is malformed" `isInfixOf` out,
          testCase "Malformed file is rejected" $ withTempDir $ \dir -> do
            let path = dir <> "/index.json"
            writeFile path "not a chain index"
            assertIOFails $ chainIndexFromSource $ ChainIndexFromFile path testRegistries False False,
          testCase "Unregistered datum types in a file are rejected" $ withTempDir $ \dir -> do
            let path = dir <> "/index.json"
            saveChainIndex path traceChainIndex
            assertIOFails $ chainIndexFromSource $ ChainIndexFromFile path defaultRegistries False False,
          testCase "Valid JSON with a wrong structure is rejected" $ withTempDir $ \dir -> do
            let path = dir <> "/index.json"
            writeFile path "{}"
            assertIOFails $ chainIndexFromSource $ ChainIndexFromFile path testRegistries False False,
          testCase "Valid JSON with a wrong structure is allowed" $ withTempDir $ \dir -> do
            let path = dir <> "/index.json"
            writeFile path "{}"
            (out, res) <- captureOutput $ chainIndexFromSource $ ChainIndexFromFile path testRegistries False True
            res @?= def
            assertBool out $ "is malformed" `isInfixOf` out,
          testCase "A directory is considered a missing file" $ withTempDir $ \dir -> do
            (out, res) <- captureOutput $ chainIndexFromSource $ ChainIndexFromFile dir testRegistries True False
            res @?= def
            assertBool out $ "does not exist" `isInfixOf` out,
          testCase "Saving to a directory fails" $ withTempDir $ \dir ->
            assertIOFails $ saveChainIndex dir def,
          testCase "Saving backs up a malformed file" $ withTempDir $ \dir -> do
            let path = dir <> "/index.json"
            writeFile path "not a chain index"
            saveChainIndex path def
            backup <- readFile $ path <> ".bak"
            backup @?= "not a chain index"
        ]
    ]
