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
    mockChainState0From,
    mockChainState0,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Api.Shelley qualified as Cardano
import Cardano.Ledger.Shelley.API qualified as Shelley
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Control.Lens qualified as Lens
import Control.Monad.Except
import Cooked.InitialDistribution
import Cooked.MockChain.BlockChain
import Cooked.MockChain.GenerateTx
import Cooked.MockChain.GenerateTx.Output
import Cooked.MockChain.MinAda
import Cooked.MockChain.UtxoState
import Cooked.Skeleton
import Data.Default
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Ledger.Index qualified as Ledger
import Ledger.Orphans ()
import Ledger.Tx qualified as Ledger
import Ledger.Tx.CardanoAPI qualified as Ledger
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
      let newAddress = view txSkelOutAddressG txSkelOut
          newPayloadSet =
            UtxoPayloadSet
              [ UtxoPayload
                  txOutRef
                  (view (txSkelOutValueL % txSkelOutValueContentL) txSkelOut)
                  ( case txSkelOut ^. txSkelOutDatumL of
                      NoTxSkelOutDatum -> NoUtxoPayloadDatum
                      SomeTxSkelOutDatum content kind -> SomeUtxoPayloadDatum content (kind /= Inline)
                  )
                  (preview (txSkelOutReferenceScriptL % txSkelOutReferenceScriptHashAF) txSkelOut)
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

-- | This creates the initial 'MockChainState' from an initial distribution by
-- submitting an initial transaction with the appropriate content. The genesis
-- key hash has been taken from
-- https://github.com/input-output-hk/cardano-node/blob/543b267d75d3d448e1940f9ec04b42bd01bbb16b/cardano-api/test/Test/Cardano/Api/Genesis.hs#L60
mockChainState0From :: (MonadBlockChainBalancing m) => InitialDistribution -> m MockChainState
mockChainState0From (InitialDistribution initDist) = do
  params <- getParams
  let genesisKeyHash = Cardano.GenesisUTxOKeyHash $ Shelley.KeyHash "23d51e91ae5adc7ae801e9de4cd54175fb7464ec2680b25686bbb194"
      inputs = [(Cardano.genesisUTxOPseudoTxIn (Emulator.pNetworkId params) genesisKeyHash, Cardano.BuildTxWith $ Cardano.KeyWitness Cardano.KeyWitnessForSpending)]
  outputsMinAda <- mapM toTxSkelOutWithMinAda initDist
  outputs <- mapM toCardanoTxOut outputsMinAda
  cardanoTx <-
    Ledger.CardanoEmulatorEraTx . txSignersAndBodyToCardanoTx []
      <$> either
        (throwError . MCEToCardanoError "generateTx :")
        return
        (Emulator.createTransactionBody params $ Ledger.CardanoBuildTx (Ledger.emptyTxBodyContent {Cardano.txOuts = outputs, Cardano.txIns = inputs}))
  let index = Ledger.fromPlutusIndex $ Ledger.initialise [[Emulator.unsafeMakeValid cardanoTx]]
      outputsMap =
        Map.fromList $
          zipWith
            (\x y -> (x, (y, True)))
            (Ledger.fromCardanoTxIn . snd <$> Ledger.getCardanoTxOutRefs cardanoTx)
            outputsMinAda
  return $ MockChainState def (Lens.set Emulator.elsUtxoL index (Emulator.initialState def)) outputsMap Nothing

-- | Same as 'mockChainState0From' with the default 'InitialDistribution'
mockChainState0 :: (MonadBlockChainBalancing m) => m MockChainState
mockChainState0 = mockChainState0From def
