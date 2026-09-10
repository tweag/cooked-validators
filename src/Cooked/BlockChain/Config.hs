-- | This module exposes the configuration elements required to execute a block
-- run. This includes the initial parameters and a way of processing the results
-- of a run.
module Cooked.BlockChain.Config
  ( -- * Initial blockchain configuration
    BlockChainConf (..),
    blockChainConfTemplate,

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

-- | Configuration from which to run a blockchain
data BlockChainConf a b where
  BlockChainConf ::
    { bccConnectInfo :: Cardano.LocalNodeConnectInfo,
      bccInitialChainIndex :: ChainIndex,
      bccPrettyOpts :: PrettyCookedOpts,
      bccFunOnResult :: RawBlockChainReturn a -> IO b
    } ->
    BlockChainConf a b

-- | A basic template for a 'BlockChainConf'. It takes a connection info as a
-- parameter, and displays the result of the run directly in IO.
blockChainConfTemplate :: (Show a) => Cardano.LocalNodeConnectInfo -> BlockChainConf a ()
blockChainConfTemplate connectInfo = BlockChainConf connectInfo def def displayBlockChainResult
