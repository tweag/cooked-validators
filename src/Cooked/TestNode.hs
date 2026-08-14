-- module Cooked umbrella ()
-- rename effect stacks to include MockChain
-- signatory first input from pk ?

module Cooked.TestNode (nodeRun, emulatorRun, exportSignatoryFiles) where

import Cardano.Api as Cardano
import Cardano.Crypto.Wallet qualified as Crypto
import Control.Monad
import Cooked
import Data.Map qualified as Map
import Data.Text.IO qualified as T
import Plutus.Script.Utils.Value qualified as Script
import Polysemy

--------------------------------------------------------------------------------
-- Export Wallets
--------------------------------------------------------------------------------

exportSignatoryFiles :: NetworkId -> FilePath -> Crypto.XPrv -> IO ()
exportSignatoryFiles netId filePrefix xprv = do
  -- 1. Get raw 128 bytes from XPrv (Matches PaymentExtendedKey format directly)
  let raw128 = Crypto.unXPrv xprv

  -- 2. Deserialise directly into PaymentExtendedKey
  case deserialiseFromRawBytes (AsSigningKey AsPaymentExtendedKey) raw128 of
    Left err -> error $ "Failed to deserialise XPrv: " ++ show err
    Right (skey :: SigningKey PaymentExtendedKey) -> do
      -- Derive Verification Key (Extended & Standard)
      let vkeyExtended = getVerificationKey skey
          vkeyStandard = castVerificationKey vkeyExtended :: VerificationKey PaymentKey

      -- Derive Address
      let pkh = verificationKeyHash vkeyStandard
          addr = makeShelleyAddress netId (PaymentCredentialByKey pkh) NoStakeAddress

      -- File paths
      let skeyFile = filePrefix ++ ".skey"
          vkeyFile = filePrefix ++ ".vkey"
          addrFile = filePrefix ++ ".addr"

      -- 3. Write files to disk
      _ <- writeFileTextEnvelope (File skeyFile) Nothing skey
      _ <- writeFileTextEnvelope (File vkeyFile) Nothing vkeyStandard
      T.writeFile addrFile (serialiseAddress addr)

      putStrLn $ "Successfully generated:\n  - " ++ skeyFile ++ "\n  - " ++ vkeyFile ++ "\n  - " ++ addrFile

exportAlice :: IO ()
exportAlice = exportSignatoryFiles (Testnet (NetworkMagic 42)) "/Users/adithya/Desktop/Prog/cardano-peras/testnet/wallets/alice" (walletSK alice)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

alice, bob, carrie, david :: Wallet
alice = wallet 1
bob = wallet 2
carrie = wallet 3
david = wallet 4

initDist :: InitialDistribution
initDist = receives alice . Value . Script.ada <$> [30, 100, 40, 28]

aliceToAlice :: (Members DirectBlockChainEffs effs) => Sem effs ()
aliceToAlice =
  -- do
  -- utxos <- utxosAt alice
  validateTxSkel_ $
    txSkelEmulatorTemplate
      { -- txSkelInputs = emptyTxSkelRedeemer <$ utxos,
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
      Cardano.localNodeNetworkId = Cardano.Testnet (Cardano.NetworkMagic 42),
      Cardano.localNodeSocketPath = Cardano.File "/Users/adithya/Desktop/Prog/cardano-peras/testnet/devnet-env/socket/node2/sock"
    }

nodeRun :: IO ()
nodeRun = do
  runBlockChainFromConfTemplate @DirectBlockChainEffs nodeConfig $ do
    alice <- define "Alice" $ wallet 1
    bob <- define "Bob" $ wallet 2

    noteS "Start: Alice"
    aliceToAlice
    -- It waits here??
    -- Is the scheduling off?

    noteS "End: Alice"
    noteS "Start: Bob"
    blockChainRun
    noteS "End: Bob"
    bobUtxos <- utxosAt bob
    noteS "Start: Wait"
    -- Wait bug
    waitNSlots 5
    noteS "End: Wait"
    -- waitTillExists
    noteW (length bobUtxos)
    -- prettyPrintBug
    assertW "Bob has 5 utxos" $ length bobUtxos == 5

emulatorRun :: [MockChainReturn ()]
emulatorRun = runMockChainFromInitDist @StagedMockChainEffs initDist $ do
  withTweak blockChainRun $
    msum $
      insertFirstTweak txSkelOutputsL . (`receives` Value (Script.ada 20)) <$> [carrie, david]
