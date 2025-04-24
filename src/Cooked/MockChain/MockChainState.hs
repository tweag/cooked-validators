-- | This module exposes the internal state in which our direct simulation is
-- run, and functions to update and query it.
module Cooked.MockChain.MockChainState where

import Cardano.Api qualified as Cardano
import Cardano.Api.Shelley qualified as Cardano
import Cardano.Ledger.Shelley.API qualified as Shelley
import Cardano.Ledger.Shelley.LedgerState qualified as Shelley
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Monad.Except
import Cooked.InitialDistribution
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx
import Cooked.MockChain.GenerateTx.Output
import Cooked.MockChain.MinAda
import Cooked.MockChain.UtxoState
import Cooked.Skeleton
import Data.Bifunctor (bimap)
import Data.Default
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Ledger.Index qualified as Ledger
import Ledger.Orphans ()
import Ledger.Slot qualified as Ledger
import Ledger.Tx qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
import Optics.Core
import PlutusLedgerApi.V3 qualified as Api

-- | The state used to run the simulation in 'Cooked.MockChain.Direct'
data MockChainState = MockChainState
  { mcstParams :: Emulator.Params,
    mcstIndex :: Ledger.UtxoIndex,
    -- | For each 'Api.TxOutRef' associates the 'TxSkelOut' that produced it,
    -- alongside a boolean to state whether this UTxO is still present in the
    -- index ('True') or has already been consumed ('False').
    mcstOutputs :: Map Api.TxOutRef (TxSkelOut, Bool),
    mcstCurrentSlot :: Ledger.Slot
  }
  deriving (Show, Eq)

instance Default MockChainState where
  def = MockChainState def (Ledger.initialise [[]]) Map.empty 0

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
mcstToUtxoState =
  foldl extractPayload mempty . Map.toList . mcstOutputs
  where
    extractPayload :: UtxoState -> (Api.TxOutRef, (TxSkelOut, Bool)) -> UtxoState
    extractPayload utxoState (txOutRef, (txSkelOut, bool)) =
      let newAddress = txSkelOutAddress txSkelOut
          newPayloadSet =
            UtxoPayloadSet
              [ UtxoPayload
                  txOutRef
                  (txSkelOutValue txSkelOut)
                  ( case txSkelOut ^. txSkelOutDatumL of
                      TxSkelOutNoDatum -> Nothing
                      TxSkelOutSomeDatum content Inline -> Just (content, False)
                      TxSkelOutSomeDatum content _ -> Just (content, True)
                  )
                  (txSkelOutReferenceScriptHash txSkelOut)
              ]
       in if bool
            then utxoState {availableUtxos = Map.insertWith (<>) newAddress newPayloadSet (availableUtxos utxoState)}
            else utxoState {consumedUtxos = Map.insertWith (<>) newAddress newPayloadSet (consumedUtxos utxoState)}

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

-- | Stores an output in a 'MockChainState'
addOutput :: Api.TxOutRef -> TxSkelOut -> MockChainState -> MockChainState
addOutput oRef txSkelOut st =
  st
    { mcstOutputs = Map.insert oRef (txSkelOut, True) (mcstOutputs st)
    }

-- | Removes an output from the 'MockChainState'
removeOutput :: Api.TxOutRef -> MockChainState -> MockChainState
removeOutput oRef st =
  st
    { mcstOutputs = Map.update (\(output, _) -> Just (output, False)) oRef (mcstOutputs st)
    }

-- * Initial `MockChainState` from an 'InitialDistribution'

-- | Builds a 'MockChainState' from an 'InitialDistribution'. This lives in
-- 'MonadBlockChainBalancing' because the creation of 'mcstIndex' is impure
mockChainState0From :: (MonadBlockChainBalancing m) => InitialDistribution -> m MockChainState
mockChainState0From i0 = do
  (index, outputs) <- utxoIndex0From i0
  return $ MockChainState def index outputs 0

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
utxoIndex0From :: (MonadBlockChainBalancing m) => InitialDistribution -> m (Ledger.UtxoIndex, Map Api.TxOutRef (TxSkelOut, Bool))
utxoIndex0From (InitialDistribution initDist) = do
  networkId <- Emulator.pNetworkId <$> getParams
  let genesisKeyHash = Cardano.GenesisUTxOKeyHash $ Shelley.KeyHash "23d51e91ae5adc7ae801e9de4cd54175fb7464ec2680b25686bbb194"
      inputs = [(Cardano.genesisUTxOPseudoTxIn networkId genesisKeyHash, Cardano.BuildTxWith $ Cardano.KeyWitness Cardano.KeyWitnessForSpending)]
  outputsMinAda <- mapM toTxSkelOutWithMinAda initDist
  outputs <- mapM toCardanoTxOut outputsMinAda
  cardanoTx <-
    Ledger.CardanoEmulatorEraTx . txSignersAndBodyToCardanoTx []
      <$> either
        (throwError . MCETxBodyError "generateTx :")
        return
        ( Cardano.createTransactionBody
            Cardano.ShelleyBasedEraConway
            (Ledger.emptyTxBodyContent {Cardano.txOuts = outputs, Cardano.txIns = inputs})
        )
  return
    ( Ledger.initialise [[Emulator.unsafeMakeValid cardanoTx]],
      Map.fromList $
        zipWith
          (\x y -> (x, (y, True)))
          (Ledger.fromCardanoTxIn . snd <$> Ledger.getCardanoTxOutRefs cardanoTx)
          outputsMinAda
    )

-- | Same as 'utxoIndex0From' with the default 'InitialDistribution'
utxoIndex0 :: (MonadBlockChainBalancing m) => m (Ledger.UtxoIndex, Map Api.TxOutRef (TxSkelOut, Bool))
utxoIndex0 = utxoIndex0From def
