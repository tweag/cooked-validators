-- module Cooked umbrella ()
-- rename effect stacks to include MockChain
-- signatory first input from pk ?

module Cooked.TestNode
  ( exportSignatoryFiles,
    exportAlice,
    exportBob,
    fetchFrom,
    alice,
    bob,
    carrie,
    david,
    fetchAll,
    defineAll,
    exportAddressFile,
    runInIO,
    pays,
    nodeRun,
  )
where

import Cardano.Api as Cardano
import Cardano.Crypto.Wallet qualified as Crypto
import Cooked hiding (Member)
import Cooked.Utilities.UtxoSearch
import Data.Map qualified as Map
import Data.Text.IO qualified as T
import Plutus.Script.Utils.Value qualified as Script
import Polysemy

--------------------------------------------------------------------------------
-- Export Wallets
--------------------------------------------------------------------------------

alice, bob, carrie, david :: Wallet
alice = wallet 1
bob = wallet 2
carrie = wallet 3
david = wallet 4

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

exportAddressFile :: String -> Wallet -> IO ()
exportAddressFile name =
  exportSignatoryFiles
    (Testnet (NetworkMagic 42))
    ("/home/monsieuro/tweag/cardano-peras/testnet/wallets/" <> name)
    . walletSK

exportAlice :: IO ()
exportAlice = exportAddressFile "alice" alice

exportBob :: IO ()
exportBob = exportAddressFile "bob" bob

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

nodeConfig :: Cardano.LocalNodeConnectInfo
nodeConfig =
  Cardano.LocalNodeConnectInfo
    { Cardano.localConsensusModeParams = Cardano.CardanoModeParams (Cardano.EpochSlots 86_400),
      Cardano.localNodeNetworkId = Cardano.Testnet (Cardano.NetworkMagic 42),
      Cardano.localNodeSocketPath = Cardano.File "/home/monsieuro/tweag/cardano-peras/testnet/devnet-env/socket/node2/sock"
    }

defineAll :: (Member Misc effs) => Sem effs ()
defineAll = do
  define_ "alice" alice
  define_ "bob" bob
  define_ "carrie" carrie
  define_ "david" david

fetchFrom :: (Members '[Misc, Query] effs) => Wallet -> String -> Sem effs ()
fetchFrom wal title =
  utxosAt wal
    >>= retrieveUtxos
    >>= retrieve Map.toList
    >>= retrieve (fmap snd)
    >>= noteL title

fetchAll :: (Members '[Misc, Query] effs) => Sem effs ()
fetchAll =
  allUtxos
    >>= retrieveUtxos
    >>= retrieve Map.toList
    >>= retrieve (fmap snd)
    >>= noteL "All Utxos"

runInIO :: (Show a) => DirectBlockChain a -> IO ()
runInIO = runBlockChainFromConfTemplate nodeConfig

pays :: Wallet -> Wallet -> Integer -> DirectBlockChain ()
pays from to ada =
  validateTxSkel_ $
    txSkelNodeTemplate
      { txSkelSignatories = txSkelSignatoriesFromList [from],
        txSkelOutputs = [to `receives` Value (Script.ada ada)]
      }

nodeRun :: IO ()
nodeRun = runInIO $ do
  defineAll
  noteS "Alice pays to Bob"
  pays alice bob 10
  noteS "Waiting 60 slots"
  _ <- waitNSlots 60
  fetchFrom bob "Bob's Utxos"
  noteS "Bob pays to David"
  pays bob david 5
  noteS "Waiting 60 slots"
  _ <- waitNSlots 60
  fetchFrom david "David's Utxos"
