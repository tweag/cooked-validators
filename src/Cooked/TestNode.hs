-- module Cooked umbrella ()
-- rename effect stacks to include MockChain
-- signatory first input from pk ?

module Cooked.TestNode (nodeRun, emulatorRun) where

import Control.Monad
import Cooked
import Plutus.Script.Utils.Value qualified as Script
import Polysemy

alice, bob, carrie, david :: Wallet
alice = wallet 1
bob = wallet 2
carrie = wallet 3
david = wallet 4

initDist :: InitialDistribution
initDist = (alice `receives`) . Value . Script.ada <$> [30, 100]

blockChainRun :: (Members DirectBlockChainEffs effs) => Sem effs ()
blockChainRun = do
  utxos <- utxosAt alice
  validateTxSkel_ $
    txSkelEmulatorTemplate
      { txSkelInputs = emptyTxSkelRedeemer <$ utxos,
        txSkelOutputs = [bob `receives` Value (Script.ada 40)],
        txSkelSignatories = [signatoryWallet alice]
      }

nodeRun :: IO ()
nodeRun = runBlockChainFromConfTemplate @DirectBlockChainEffs undefined blockChainRun

emulatorRun :: [MockChainReturn ()]
emulatorRun = runMockChainFromInitDist @StagedMockChainEffs initDist $ do
  withTweak blockChainRun $
    msum $
      insertFirstTweak txSkelOutputsL . (`receives` Value (Script.ada 20)) <$> [carrie, david]
