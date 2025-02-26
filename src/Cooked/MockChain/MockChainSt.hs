module Cooked.MockChain.MockChainSt where

import Cardano.Api qualified as Cardano
import Cardano.Api.Shelley qualified as Cardano
import Cardano.Ledger.Shelley.API qualified as Shelley
import Cardano.Ledger.Shelley.LedgerState qualified as Shelley
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Arrow
import Cooked.Conversion.ToScriptHash
import Cooked.Conversion.ToVersionedScript
import Cooked.InitialDistribution
import Cooked.MockChain.GenerateTx (GenerateTxError (..), generateTxOut)
import Cooked.MockChain.UtxoState
import Cooked.Output
import Cooked.Skeleton
import Data.Bifunctor (bimap)
import Data.Default
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Ledger.Index qualified as Ledger
import Ledger.Orphans ()
import Ledger.Slot qualified as Ledger
import Ledger.Tx qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core (view)
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Slightly more concrete version of 'UtxoState', used to actually run the
-- simulation.
data MockChainSt = MockChainSt
  { mcstParams :: Emulator.Params,
    mcstIndex :: Ledger.UtxoIndex,
    -- map from datum hash to (datum, count), where count is the number of UTxOs
    -- that currently have the datum. This map is used to display the contents
    -- of the state to the user, and to recover datums for transaction
    -- generation.
    mcstDatums :: Map Api.DatumHash (TxSkelOutDatum, Integer),
    mcstValidators :: Map Script.ValidatorHash (Script.Versioned Script.Validator),
    mcstCurrentSlot :: Ledger.Slot
  }
  deriving (Show, Eq)

instance Default MockChainSt where
  def = mockChainSt0

-- | Converts a builtin UtxoIndex into our own usable map between utxos and
-- associated outputs.
getIndex :: Ledger.UtxoIndex -> Map Api.TxOutRef Api.TxOut
getIndex =
  Map.fromList
    . map (bimap Ledger.fromCardanoTxIn (Ledger.fromCardanoTxOutToPV2TxInfoTxOut . toCtxTxTxOut))
    . Map.toList
    . Cardano.unUTxO
  where
    -- We need to convert a UTxO context TxOut to a Transaction context Tx out.
    -- It's complicated because the datum type is indexed by the context.
    toCtxTxTxOut :: Cardano.TxOut Cardano.CtxUTxO era -> Cardano.TxOut Cardano.CtxTx era
    toCtxTxTxOut (Cardano.TxOut addr val d refS) =
      let dat = case d of
            Cardano.TxOutDatumNone -> Cardano.TxOutDatumNone
            Cardano.TxOutDatumHash s h -> Cardano.TxOutDatumHash s h
            Cardano.TxOutDatumInline s sd -> Cardano.TxOutDatumInline s sd
       in Cardano.TxOut addr val dat refS

mcstToUtxoState :: MockChainSt -> UtxoState
mcstToUtxoState MockChainSt {mcstIndex, mcstDatums} =
  UtxoState
    . foldr (\(address, utxoValueSet) acc -> Map.insertWith (<>) address utxoValueSet acc) Map.empty
    . mapMaybe
      ( extractPayload
          . bimap
            Ledger.fromCardanoTxIn
            Ledger.fromCardanoTxOutToPV2TxInfoTxOut'
      )
    . Map.toList
    . Cardano.unUTxO
    $ mcstIndex
  where
    extractPayload :: (Api.TxOutRef, Api.TxOut) -> Maybe (Api.Address, UtxoPayloadSet)
    extractPayload (txOutRef, out@Api.TxOut {Api.txOutAddress, Api.txOutValue, Api.txOutDatum}) =
      do
        let mRefScript = outputReferenceScriptHash out
        txSkelOutDatum <-
          case txOutDatum of
            Api.NoOutputDatum -> Just TxSkelOutNoDatum
            Api.OutputDatum datum -> fst <$> Map.lookup (Script.datumHash datum) mcstDatums
            Api.OutputDatumHash hash -> fst <$> Map.lookup hash mcstDatums
        return
          ( txOutAddress,
            UtxoPayloadSet [UtxoPayload txOutRef txOutValue txSkelOutDatum mRefScript]
          )

-- | Generating a skeleton context from a mockchain state. This is dedicated to
-- allowing the pretty printer to resolve skeleton parts.
mcstToSkelContext :: MockChainSt -> SkelContext
mcstToSkelContext MockChainSt {..} =
  SkelContext
    (getIndex mcstIndex)
    (Map.map fst mcstDatums)

-- | Generating an emulated state for the emulator from a mockchain state and
-- some parameters, based on a standard initial state
mcstToEmulatedLedgerState :: MockChainSt -> Emulator.EmulatedLedgerState
mcstToEmulatedLedgerState MockChainSt {..} =
  let els@(Emulator.EmulatedLedgerState le mps) = Emulator.initialState mcstParams
   in els
        { Emulator._ledgerEnv = le {Shelley.ledgerSlotNo = fromIntegral mcstCurrentSlot},
          Emulator._memPoolState =
            mps
              { Shelley.lsUTxOState =
                  Shelley.smartUTxOState
                    (Emulator.emulatorPParams mcstParams)
                    (Ledger.fromPlutusIndex mcstIndex)
                    (Emulator.Coin 0)
                    (Emulator.Coin 0)
                    def
                    (Emulator.Coin 0)
              }
        }

addDatums :: [(Api.DatumHash, TxSkelOutDatum)] -> MockChainSt -> MockChainSt
addDatums toAdd st@(MockChainSt {mcstDatums}) =
  st
    { mcstDatums =
        foldl
          ( \datumMap (dHash, dat) ->
              Map.insertWith (\(d, n) (_, n') -> (d, n + n')) dHash (dat, 1) datumMap
          )
          mcstDatums
          toAdd
    }

removeDatums :: [Api.DatumHash] -> MockChainSt -> MockChainSt
removeDatums toRemove st@(MockChainSt {mcstDatums}) =
  st
    { mcstDatums =
        foldl
          (flip (Map.update (\(dat, n) -> (dat,) <$> minusMaybe n)))
          mcstDatums
          toRemove
    }
  where
    -- This is unsafe as this assumes n >= 1
    minusMaybe :: Integer -> Maybe Integer
    minusMaybe n | n == 1 = Nothing
    minusMaybe n = Just $ n - 1

addValidators :: Map Script.ValidatorHash (Script.Versioned Script.Validator) -> MockChainSt -> MockChainSt
addValidators valMap st@(MockChainSt {mcstValidators}) = st {mcstValidators = Map.union valMap mcstValidators}

-- * Canonical initial values

utxoState0 :: UtxoState
utxoState0 = mcstToUtxoState mockChainSt0

mockChainSt0 :: MockChainSt
mockChainSt0 = MockChainSt def utxoIndex0 Map.empty Map.empty 0

-- * Initial `MockChainSt` from an initial distribution

mockChainSt0From :: InitialDistribution -> MockChainSt
mockChainSt0From i0 = MockChainSt def (utxoIndex0From i0) (datumMap0From i0) (referenceScriptMap0From i0 <> scriptMap0From i0) 0

-- | Reference scripts from initial distributions should be accounted for in the
-- `MockChainSt` which is done using this function.
referenceScriptMap0From :: InitialDistribution -> Map Script.ValidatorHash (Script.Versioned Script.Validator)
referenceScriptMap0From =
  -- This builds a map of entries from the reference scripts contained in the
  -- initial distribution
  Map.fromList . mapMaybe unitMaybeFrom . unInitialDistribution
  where
    -- This takes a single output and returns a possible map entry when it
    -- contains a reference script
    unitMaybeFrom :: TxSkelOut -> Maybe (Script.ValidatorHash, Script.Versioned Script.Validator)
    unitMaybeFrom (Pays output) = do
      refScript <- view outputReferenceScriptL output
      let vScript@(Script.Versioned script version) = toVersionedScript refScript
      return (Script.ValidatorHash $ Api.getScriptHash $ toScriptHash vScript, Script.Versioned (Script.Validator script) version)

-- | Scripts from initial distributions should be accounted for in the
-- `MockChainSt` which is done using this function.
scriptMap0From :: InitialDistribution -> Map Script.ValidatorHash (Script.Versioned Script.Validator)
scriptMap0From =
  -- This builds a map of entries from the scripts contained in the initial
  -- distribution
  Map.fromList . mapMaybe unitMaybeFrom . unInitialDistribution
  where
    -- This takes a single output and returns a possible map entry when it
    -- contains a script
    unitMaybeFrom :: TxSkelOut -> Maybe (Script.ValidatorHash, Script.Versioned Script.Validator)
    unitMaybeFrom txSkelOut = do
      val <- txSkelOutValidator txSkelOut
      return (Script.ValidatorHash $ Api.getScriptHash $ toScriptHash val, val)

-- | Datums from initial distributions should be accounted for in the
-- `MockChainSt` which is done using this function.
datumMap0From :: InitialDistribution -> Map Api.DatumHash (TxSkelOutDatum, Integer)
datumMap0From (InitialDistribution initDist) =
  -- This concatenates singleton maps from inputs and accounts for the number of
  -- occurrences of similar datums
  foldl' (\m -> Map.unionWith (\(d, n1) (_, n2) -> (d, n1 + n2)) m . unitMapFrom) Map.empty initDist
  where
    -- This takes a single output and creates an empty map if it contains no
    -- datum, or a singleton map if it contains one
    unitMapFrom :: TxSkelOut -> Map Api.DatumHash (TxSkelOutDatum, Integer)
    unitMapFrom txSkelOut =
      let datum = view txSkelOutDatumL txSkelOut
       in maybe Map.empty (flip Map.singleton (datum, 1) . Script.datumHash) $ txSkelOutUntypedDatum datum

-- | This creates the initial UtxoIndex from an initial distribution by
-- submitting an initial transaction with the appropriate content:
--
-- - inputs consist of a single dummy pseudo input
--
-- - all non-ada assets in outputs are considered minted
--
-- - outputs are translated from the `TxSkelOut` list in the initial
--   distribution
--
-- Two things to note:
--
-- - We don't know what "Magic" means for the network ID (TODO)
--
-- - The genesis key hash has been taken from
--   https://github.com/input-output-hk/cardano-node/blob/543b267d75d3d448e1940f9ec04b42bd01bbb16b/cardano-api/test/Test/Cardano/Api/Genesis.hs#L60
utxoIndex0From :: InitialDistribution -> Ledger.UtxoIndex
utxoIndex0From (InitialDistribution initDist) = case mkBody of
  Left err -> error $ show err
  -- TODO: There may be better ways to generate this initial state, see
  -- createGenesisTransaction for instance
  Right body -> Ledger.initialise [[Emulator.unsafeMakeValid $ Ledger.CardanoEmulatorEraTx $ Cardano.Tx body []]]
  where
    mkBody :: Either GenerateTxError (Cardano.TxBody Cardano.ConwayEra)
    mkBody = do
      let theNetworkId = Cardano.Testnet $ Cardano.NetworkMagic 42
          genesisKeyHash = Cardano.GenesisUTxOKeyHash $ Shelley.KeyHash "23d51e91ae5adc7ae801e9de4cd54175fb7464ec2680b25686bbb194"
          inputs = [(Cardano.genesisUTxOPseudoTxIn theNetworkId genesisKeyHash, Cardano.BuildTxWith $ Cardano.KeyWitness Cardano.KeyWitnessForSpending)]
      outputs <- mapM (generateTxOut theNetworkId) initDist
      left (TxBodyError "Body error") $
        Cardano.createAndValidateTransactionBody Cardano.ShelleyBasedEraConway $
          Ledger.emptyTxBodyContent {Cardano.txOuts = outputs, Cardano.txIns = inputs}

utxoIndex0 :: Ledger.UtxoIndex
utxoIndex0 = utxoIndex0From def
