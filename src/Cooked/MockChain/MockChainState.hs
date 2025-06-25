-- | This module exposes the internal state in which our direct simulation is
-- run, and functions to update and query it.
module Cooked.MockChain.MockChainState
  ( MockChainState (..),
    mcstParamsL,
    mcstLedgerStateL,
    mcstOutputsL,
    mcstConstitutionL,
    mcstToUtxoState,
    addOutput,
    removeOutput,
  )
where

import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Cooked.MockChain.UtxoState
import Cooked.Skeleton
import Data.Default
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Ledger.Orphans ()
import Optics.Core
import Optics.TH
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | The state used to run the simulation in 'Cooked.MockChain.Direct'
data MockChainState = MockChainState
  { mcstParams :: Emulator.Params,
    mcstLedgerState :: Emulator.EmulatedLedgerState,
    -- | Associates to each 'Api.TxOutRef' the 'TxSkelOut' that produced it,
    -- alongside a boolean to state whether this UTxO is still present in the
    -- index ('True') or has already been consumed ('False').
    mcstOutputs :: Map Api.TxOutRef (TxSkelOut, Bool),
    -- | The constitution script to be used with proposals
    mcstConstitution :: Maybe (Script.Versioned Script.Script)
  }
  deriving (Show)

-- | A lens to set or get the parameters of the 'MockChainState'
makeLensesFor [("mcstParams", "mcstParamsL")] ''MockChainState

-- | A lens to set or get the ledger state of the 'MockChainState'
makeLensesFor [("mcstLedgerState", "mcstLedgerStateL")] ''MockChainState

-- | A lens to set or get the outputs of the 'MockChainState'
makeLensesFor [("mcstOutputs", "mcstOutputsL")] ''MockChainState

-- | A lens to set or get the constitution script of the 'MockChainState'
makeLensesFor [("mcstConstitution", "mcstConstitutionL")] ''MockChainState

instance Default MockChainState where
  def = MockChainState def (Emulator.initialState def) Map.empty Nothing

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

-- | Stores an output in a 'MockChainState'
addOutput :: Api.TxOutRef -> TxSkelOut -> MockChainState -> MockChainState
addOutput oRef txSkelOut = over mcstOutputsL (Map.insert oRef (txSkelOut, True))

-- | Removes an output from the 'MockChainState'
removeOutput :: Api.TxOutRef -> MockChainState -> MockChainState
removeOutput oRef = over mcstOutputsL (Map.update (\(output, _) -> Just (output, False)) oRef)
