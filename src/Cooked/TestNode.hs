-- module Cooked umbrella ()
-- rename effect stacks to include MockChain
-- signatory first input from pk ?

module Cooked.TestNode (nodeRun, emulatorRun) where

import Cooked
import Plutus.Script.Utils.Value qualified as Script
import Polysemy

alice, bob, carrie :: Wallet
alice = wallet 1
bob = wallet 2
carrie = wallet 3

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
emulatorRun = runMockChainFromInitDist @StagedEffs initDist $ do
  withTweak
    blockChainRun
    (insertFirstTweak txSkelOutputsL (carrie `receives` Value (Script.ada 20)))
