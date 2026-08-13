-- | This module exposes the infrastructure to execute blockchain runs against a
-- real node backend, mirroring 'Cooked.MockChain.Runnable' which targets the
-- emulated chain.
module Cooked.BlockChain.Run
  ( -- * Running blockchains
    RunnableBlockChain (..),
    runBlockChainFromConf,
    runBlockChainFromConfTemplate,
  )
where

import Cardano.Api qualified as Cardano
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
runBlockChainFromConf (BlockChainConf connectInfo chainIndex prettyOpts fun) comp =
  runBlockChain prettyOpts chainIndex connectInfo comp >>= fun

-- | Runs a 'RunnableBlockChain' from an initial default 'BlockChainConf'
runBlockChainFromConfTemplate ::
  ( RunnableBlockChain effs,
    Show a
  ) =>
  Cardano.LocalNodeConnectInfo ->
  Sem effs a ->
  IO ()
runBlockChainFromConfTemplate nodeInfo =
  runBlockChainFromConf (blockChainConfTemplate nodeInfo)
