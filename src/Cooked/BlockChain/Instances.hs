-- | This module exposes the concrete instances to run a blockchain against a
-- real node backend, mirroring 'Cooked.MockChain.Instances' which targets the
-- emulated chain.
module Cooked.BlockChain.Instances
  ( FullBlockChainEffs,
    FullBlockChain,
  )
where

import Cardano.Api qualified as Cardano
import Cooked.Effect
import Cooked.Runtime
import Ledger qualified as P.Ledger
import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.Reader
import Polysemy.State

-- | The most direct stack of effects to run a mockchain
type FullBlockChainEffs =
  '[ Validate,
     Query,
     Time,
     Misc,
     Fail,
     Params,
     Error Cardano.UnsupportedNtcVersionError,
     Error Cardano.EraMismatch,
     Error Cardano.AcquiringFailure,
     Error ChainError,
     Error P.Ledger.ToCardanoError,
     Error Cardano.PastHorizonException,
     Reader Cardano.LocalNodeConnectInfo,
     State ChainIndex,
     Embed IO
   ]

-- | A mockchain computation built on top of the `DirectEffs` stack of effects
type FullBlockChain a = Sem FullBlockChainEffs a
