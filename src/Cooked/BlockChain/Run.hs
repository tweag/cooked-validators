-- | This module exposes the infrastructure to execute blockchain runs against a
-- real node backend, mirroring 'Cooked.MockChain.Run' which targets the
-- emulated chain.
module Cooked.BlockChain.Run
  ( -- * Running blockchains
    RunnableBlockChain (..),
    runBlockChainFromConf,
    runBlockChainFromConfTemplate,
  )
where

import Cardano.Api qualified as Cardano
import Control.Monad (forM_)
import Cooked.BlockChain.Config
import Cooked.Pretty.Options
import Cooked.Runtime.State
import Polysemy

-- | The class of effects that represent a blockchain run
class RunnableBlockChain effs where
  -- | Runs a blockchain computation
  runBlockChain :: PrettyCookedOpts -> ChainIndex -> Cardano.LocalNodeConnectInfo -> Sem effs a -> IO (RawBlockChainReturn a)

-- | Runs a 'RunnableBlockChain' from an initial 'BlockChainConf'
runBlockChainFromConf ::
  (RunnableBlockChain effs) =>
  BlockChainConf a b ->
  Sem effs a ->
  IO b
runBlockChainFromConf BlockChainConf {..} comp = do
  -- we fetch the initial chain index.
  chainIndex <- chainIndexFromSource bccChainIndexSource
  -- we run the actual computation, retrieving the new chain index.
  result@(_, (newChainIndex, _)) <- runBlockChain bccPrettyOpts chainIndex bccConnectInfo comp
  -- we persist the new chain index when a target file is specified.
  forM_ bccChainIndexTarget (`saveChainIndex` newChainIndex)
  -- we call the result handler function.
  bccFunOnResult result

-- | Runs a 'RunnableBlockChain' from an initial default 'BlockChainConf'
runBlockChainFromConfTemplate ::
  ( RunnableBlockChain effs,
    Show a
  ) =>
  Cardano.LocalNodeConnectInfo ->
  Sem effs a ->
  IO ()
runBlockChainFromConfTemplate =
  runBlockChainFromConf . blockChainConfDefaultTemplate
