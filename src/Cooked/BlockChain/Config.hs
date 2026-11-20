-- | This module exposes the configuration elements required to execute a block
-- run. This includes the initial parameters and a way of processing the results
-- of a run.
module Cooked.BlockChain.Config
  ( -- * Initial blockchain configuration
    ChainIndexSource (..),
    BlockChainConf (..),
    blockChainConfDefaultTemplate,
    blockChainConfWithGivenRegistryTemplate,
    blockChainConfWithExtraRegistryTemplate,
    blockChainConfWithDefaultRegistriesTemplate,
    chainIndexFromSource,

    -- * Blockchain return type
    RawBlockChainReturn,
    FunOnBlockChainResult,
    displayBlockChainResult,
  )
where

import Cardano.Api qualified as Cardano
import Control.Monad (when)
import Cooked.Pretty
import Cooked.Runtime
import Data.Default
import Data.Foldable.Extra (notNull)
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP

-- | Raw return type of running a blockchain
type RawBlockChainReturn a =
  (PrettyCookedOpts, (ChainIndex, Either ChainError a))

-- | The type of function handling the raw blockchain return within an IO
-- context.
type FunOnBlockChainResult a b = RawBlockChainReturn a -> IO b

-- | A simple function handling the result of a blockchain run by displaying on
-- IO the returned value and resulting blockchain state.
displayBlockChainResult :: (Show a) => FunOnBlockChainResult a ()
displayBlockChainResult (opts, (chainIndexToUtxoState -> UtxoState available consumed, res)) = do
  when (pcOptPrintReturnedValue opts) $
    PP.putDoc $ case res of
      Left err -> "🔴 Error:" <+> prettyCookedOpt opts err <> PP.line
      Right a -> "🟢 Success with returned value:" <+> PP.viaShow a <> PP.line
  when (pcOptPrintConsumedUTxOs opts && notNull consumed) $
    PP.putDoc $
      "🗑️" <+> prettyCookedOpt opts consumed <> PP.line
  when (pcOptPrintRemainingUTxOs opts && notNull available) $
    PP.putDoc $
      "💰" <+> prettyCookedOpt opts available <> PP.line

-- | The source of the initial chain index for a blockchain run.
data ChainIndexSource where
  -- | The initial chain index is provided directly from a previous run
  DirectChainIndex :: ChainIndex -> ChainIndexSource
  -- | The initial chain index is loaded from a file
  ChainIndexFromFile ::
    -- | File path to load the initial chain index from
    FilePath ->
    -- | Type registries to support deserialization of custom types hidden
    -- within the outputs of the chain index.
    Registries ->
    -- | Whether to continue if the file is missing
    Bool ->
    -- | Whether to continue if the file is malformed
    Bool ->
    ChainIndexSource

instance Default ChainIndexSource where
  def = DirectChainIndex def

-- | Loads the initial chain index from a file, or returns the one provided directly.
chainIndexFromSource :: ChainIndexSource -> IO ChainIndex
chainIndexFromSource (DirectChainIndex ci) = return ci
chainIndexFromSource (ChainIndexFromFile filePath registries allowMissing allowMalformed) =
  loadChainIndex filePath registries allowMissing allowMalformed

-- | Configuration from which to run a blockchain
data BlockChainConf a b where
  BlockChainConf ::
    { -- | Connection info to the node
      bccConnectInfo :: Cardano.LocalNodeConnectInfo,
      -- | Initial chain index to use for the run
      bccChainIndexSource :: ChainIndexSource,
      -- | Optional file path to store the final chain index after the run.
      -- This file may contain private keys in plain text (see
      -- 'Cooked.Runtime.State.saveChainIndex').
      bccChainIndexTarget :: Maybe FilePath,
      -- | Pretty options to use during and after the run
      bccPrettyOpts :: PrettyCookedOpts,
      -- | Function to handle the result of the run
      bccFunOnResult :: RawBlockChainReturn a -> IO b
    } ->
    BlockChainConf a b

-- | A basic template for a 'BlockChainConf'. It takes a connection info as a
-- parameter, and displays the result of the run directly in IO. The pretty
-- options are set to their default values. The chain index is initialized to an
-- empty state, and is not persisted to a file after the run.
blockChainConfDefaultTemplate ::
  (Show a) =>
  Cardano.LocalNodeConnectInfo ->
  BlockChainConf a ()
blockChainConfDefaultTemplate connectInfo =
  BlockChainConf connectInfo def Nothing def displayBlockChainResult

-- | A template for a 'BlockChainConf' that loads the initial chain index from a
-- file and stores it back to the same file after the run. It takes the type
-- registries as a parameter to support deserialization of custom types hidden
-- within the outputs of the chain index. Otherwise, this configuration template
-- behaves like 'blockChainConfDefaultTemplate'. Beware that the file may contain
-- private keys in plain text (see 'Cooked.Runtime.State.saveChainIndex').
blockChainConfWithGivenRegistryTemplate ::
  (Show a) =>
  Cardano.LocalNodeConnectInfo ->
  FilePath ->
  Registries ->
  BlockChainConf a ()
blockChainConfWithGivenRegistryTemplate connectInfo filePath registries =
  (blockChainConfDefaultTemplate connectInfo)
    { bccChainIndexSource = ChainIndexFromFile filePath registries True False,
      bccChainIndexTarget = Just filePath
    }

-- | Same as 'blockChainConfWithGivenRegistryTemplate', but the type registries
-- in parameters are merged with the default registries, so that the default
-- types are still supported.
blockChainConfWithExtraRegistryTemplate ::
  (Show a) =>
  Cardano.LocalNodeConnectInfo ->
  FilePath ->
  Registries ->
  BlockChainConf a ()
blockChainConfWithExtraRegistryTemplate connectInfo filePath registries =
  blockChainConfWithGivenRegistryTemplate connectInfo filePath (defaultRegistries <> registries)

-- | Same as 'blockChainConfWithGivenRegistryTemplate', but uses the default
-- registries.
blockChainConfWithDefaultRegistriesTemplate ::
  (Show a) =>
  Cardano.LocalNodeConnectInfo ->
  FilePath ->
  BlockChainConf a ()
blockChainConfWithDefaultRegistriesTemplate connectInfo filePath =
  blockChainConfWithExtraRegistryTemplate connectInfo filePath emptyRegistries
