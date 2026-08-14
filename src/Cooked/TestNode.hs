-- module Cooked umbrella ()
-- rename effect stacks to include MockChain
-- signatory first input from pk ?

module Cooked.TestNode (nodeRun, emulatorRun) where

import Cardano.Api qualified as Cardano
import Cardano.Crypto.Wallet qualified as Crypto
import Control.Monad
import Cooked
import Data.ByteString.Char8 qualified as BS8
import Data.Map qualified as Map
import Optics.Core
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import Polysemy

faucetPKH :: Api.PubKeyHash
faucetPKH = Api.PubKeyHash undefined

faucetPrivKey :: Crypto.XPrv
faucetPrivKey =
  either error id $
    Crypto.xprv $
      BS8.pack $
        BS8.readFile undefined -- "<128 bytes: 64-byte private key ++ 32-byte public key ++ 32-byte chain code>"

faucetSignatory :: TxSkelSignatory
faucetSignatory = TxSkelSignatory faucetPKH $ Just faucetPrivKey

alice, bob, carrie, david :: Wallet
alice = wallet 1
bob = wallet 2
carrie = wallet 3
david = wallet 4

initDist :: InitialDistribution
initDist = receives alice . Value . Script.ada <$> [30, 100, 40, 28]

fromFaucetUtxo :: (Members DirectBlockChainEffs effs) => Sem effs ()
fromFaucetUtxo = do
  faucetUtxo <-
    utxosAt faucetPKH
      >>= retrieve (fst . Map.elemAt 0)
  validateTxSkel_ $
    txSkelEmulatorTemplate
      { txSkelInputs = Map.singleton faucetUtxo emptyTxSkelRedeemer,
        txSkelOutputs = initDist,
        txSkelSignatories = [signatoryWallet alice]
      }

blockChainRun :: (Members DirectBlockChainEffs effs) => Sem effs ()
blockChainRun =
  validateTxSkel_ $
    txSkelEmulatorTemplate
      { txSkelOutputs = [bob `receives` Value (Script.ada 40)],
        txSkelSignatories = [signatoryWallet alice]
      }

nodeConfig :: Cardano.LocalNodeConnectInfo
nodeConfig =
  Cardano.LocalNodeConnectInfo
    { Cardano.localConsensusModeParams = Cardano.CardanoModeParams (Cardano.EpochSlots 86_400),
      Cardano.localNodeNetworkId = Cardano.Testnet (Cardano.NetworkMagic undefined),
      Cardano.localNodeSocketPath = Cardano.File undefined
    }

nodeRun :: IO ()
nodeRun = runBlockChainFromConfTemplate @DirectBlockChainEffs nodeConfig $ fromFaucetUtxo >> blockChainRun

emulatorRun :: [MockChainReturn ()]
emulatorRun = runMockChainFromInitDist @StagedMockChainEffs initDist $ do
  withTweak blockChainRun $
    msum $
      insertFirstTweak txSkelOutputsL . (`receives` Value (Script.ada 20)) <$> [carrie, david]
