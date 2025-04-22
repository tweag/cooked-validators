-- | This module exposes the internal state in which our direct simulation is
-- run, and functions to update and query it.
module Cooked.MockChain.MockChainState where

import Cardano.Api qualified as Cardano
import Cardano.Api.Shelley qualified as Cardano
import Cardano.Ledger.Shelley.API qualified as Shelley
import Cardano.Ledger.Shelley.LedgerState qualified as Shelley
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Monad
import Control.Monad.Except
import Cooked.InitialDistribution
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx
import Cooked.MockChain.GenerateTx.Output
import Cooked.MockChain.MinAda
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
import Plutus.Script.Utils.Data qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | The state used to run the simulation in 'Cooked.MockChain.Direct'
data MockChainState = MockChainState
  { mcstParams :: Emulator.Params,
    mcstIndex :: Ledger.UtxoIndex,
    -- map from datum hash to (datum, count), where count is the number of UTxOs
    -- that currently have the datum. This map is used to display the contents
    -- of the state to the user, and to recover datums for transaction
    -- generation.
    mcstDatums :: Map Api.DatumHash (DatumContent, Integer),
    mcstScripts :: Map Script.ScriptHash (Script.Versioned Script.Script),
    mcstCurrentSlot :: Ledger.Slot
  }
  deriving (Show, Eq)

instance Default MockChainState where
  def = MockChainState def (Ledger.initialise [[]]) Map.empty Map.empty 0

-- | Converts a builtin UtxoIndex into our own usable map between utxos and
-- associated outputs.
getIndex :: Ledger.UtxoIndex -> Map Api.TxOutRef Api.TxOut
getIndex =
  Map.fromList
    . map
      ( bimap
          Ledger.fromCardanoTxIn
          (Ledger.fromCardanoTxOutToPV2TxInfoTxOut . Cardano.fromCtxUTxOTxOut)
      )
    . Map.toList
    . Cardano.unUTxO

-- | Builds a 'UtxoState' from a 'MockChainState'
mcstToUtxoState :: MockChainState -> UtxoState
mcstToUtxoState MockChainState {mcstIndex, mcstDatums} =
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
    extractPayload (txOutRef, Api.TxOut {txOutAddress, txOutValue, txOutDatum, txOutReferenceScript}) =
      do
        mTxOutDatum <-
          case txOutDatum of
            Api.NoOutputDatum -> return Nothing
            Api.OutputDatum datum -> Just . (,False) . fst <$> Map.lookup (Script.datumHash datum) mcstDatums
            Api.OutputDatumHash hash -> Just . (,True) . fst <$> Map.lookup hash mcstDatums
        return
          ( txOutAddress,
            UtxoPayloadSet [UtxoPayload txOutRef txOutValue mTxOutDatum txOutReferenceScript]
          )

-- | Generating an emulated state for the emulator from a mockchain state and
-- some parameters, based on a standard initial state
mcstToEmulatedLedgerState :: MockChainState -> Emulator.EmulatedLedgerState
mcstToEmulatedLedgerState MockChainState {..} =
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

-- | Adds a list of pairs @(datumHash, datum)@ into a 'MockChainState'
addDatums :: [DatumContent] -> MockChainState -> MockChainState
addDatums toAdd st@(MockChainState {mcstDatums}) =
  st
    { mcstDatums =
        foldl
          ( \datumMap dat ->
              Map.insertWith (\(d, n) (_, n') -> (d, n + n')) (datumContentToDatumHash dat) (dat, 1) datumMap
          )
          mcstDatums
          toAdd
    }

-- | Removes a certain amound of datum hashes from a 'MockChainState'
removeDatums :: [Api.DatumHash] -> MockChainState -> MockChainState
removeDatums toRemove st@(MockChainState {mcstDatums}) =
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

-- | Stores a script in a 'MockChainState'
addScript :: (Script.ToScriptHash s, Script.ToVersioned Script.Script s) => s -> MockChainState -> MockChainState
addScript script st = st {mcstScripts = Map.insert (Script.toScriptHash script) (Script.toVersioned script) (mcstScripts st)}

-- * Initial `MockChainState` from an 'InitialDistribution'

-- | Builds a 'MockChainState' from an 'InitialDistribution'. This lives in
-- 'MonadBlockChainBalancing' because the creation of 'mcstIndex' is impure
mockChainState0From :: (MonadBlockChainBalancing m) => InitialDistribution -> m MockChainState
mockChainState0From i0 = (\x -> MockChainState def x (datumMap0From i0) (referenceScriptMap0From i0 <> scriptMap0From i0) 0) <$> utxoIndex0From i0

-- | Collects the reference scripts present in an 'InitialDistribution'
referenceScriptMap0From :: InitialDistribution -> Map Script.ScriptHash (Script.Versioned Script.Script)
referenceScriptMap0From =
  -- This builds a map of entries from the reference scripts contained in the
  -- initial distribution
  Map.fromList . mapMaybe unitMaybeFrom . unInitialDistribution
  where
    -- This takes a single output and returns a possible map entry when it
    -- contains a reference script
    unitMaybeFrom :: TxSkelOut -> Maybe (Script.ScriptHash, Script.Versioned Script.Script)
    unitMaybeFrom (Pays output) = do
      vScript <- Script.toVersioned @Script.Script <$> view outputReferenceScriptL output
      return (Script.toScriptHash vScript, vScript)

-- | Collects the scripts paid to in an 'InitialDistribution'
scriptMap0From :: InitialDistribution -> Map Script.ScriptHash (Script.Versioned Script.Script)
scriptMap0From =
  -- This builds a map of entries from the scripts contained in the initial
  -- distribution
  Map.fromList . mapMaybe unitMaybeFrom . unInitialDistribution
  where
    -- This takes a single output and returns a possible map entry when it
    -- contains a script
    unitMaybeFrom :: TxSkelOut -> Maybe (Script.ScriptHash, Script.Versioned Script.Script)
    unitMaybeFrom txSkelOut = do
      val <- txSkelOutValidator txSkelOut
      return (Script.toScriptHash val, Script.toVersioned @Script.Script val)

-- | Collects the datums paid in an 'InitialDistribution'
datumMap0From :: InitialDistribution -> Map Api.DatumHash (DatumContent, Integer)
datumMap0From =
  -- This concatenates singleton maps from inputs and accounts for the number of
  -- occurrences of similar datums
  foldl'
    ( \m ->
        Map.unionWith
          (\(d, n1) (_, n2) -> (d, n1 + n2))
          m
          . maybe
            Map.empty
            (\dat -> Map.singleton (datumContentToDatumHash dat) (dat, 1))
          . txSkelOutDatumContent
          . view txSkelOutDatumL
    )
    Map.empty
    . unInitialDistribution

-- | This creates the initial UtxoIndex from an initial distribution by
-- submitting an initial transaction with the appropriate content:
--
-- - inputs consist of a single dummy pseudo input
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
utxoIndex0From :: (MonadBlockChainBalancing m) => InitialDistribution -> m Ledger.UtxoIndex
utxoIndex0From (InitialDistribution initDist) = do
  networkId <- Emulator.pNetworkId <$> getParams
  let genesisKeyHash = Cardano.GenesisUTxOKeyHash $ Shelley.KeyHash "23d51e91ae5adc7ae801e9de4cd54175fb7464ec2680b25686bbb194"
      inputs = [(Cardano.genesisUTxOPseudoTxIn networkId genesisKeyHash, Cardano.BuildTxWith $ Cardano.KeyWitness Cardano.KeyWitnessForSpending)]
  outputs <- mapM (toTxSkelOutWithMinAda >=> toCardanoTxOut) initDist
  Ledger.initialise . (: []) . (: []) . Emulator.unsafeMakeValid . Ledger.CardanoEmulatorEraTx . txSignersAndBodyToCardanoTx []
    <$> either
      (throwError . MCETxBodyError "generateTx :")
      return
      ( Cardano.createTransactionBody
          Cardano.ShelleyBasedEraConway
          (Ledger.emptyTxBodyContent {Cardano.txOuts = outputs, Cardano.txIns = inputs})
      )

-- | Same as 'utxoIndex0From' with the default 'InitialDistribution'
utxoIndex0 :: (MonadBlockChainBalancing m) => m Ledger.UtxoIndex
utxoIndex0 = utxoIndex0From def
