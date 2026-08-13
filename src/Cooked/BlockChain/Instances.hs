{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exposes the concrete instances to run a blockchain against a
-- real node backend, mirroring 'Cooked.MockChain.Instances' which targets the
-- emulated chain.
module Cooked.BlockChain.Instances
  ( -- * Direct, simple blockchain instance
    DirectBlockChainEffs,
    DirectBlockChain,

    -- * Blockchain instance with all effects
    FullBlockChainEffs,
    FullBlockChain,
  )
where

import Cardano.Api qualified as Cardano
import Cooked.BlockChain.Run
import Cooked.Effect
import Cooked.Pretty.Options
import Cooked.Runtime
import Ledger qualified as P.Ledger
import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.Reader
import Polysemy.State

-- | The most simple, straightforward stack of effects allowing to express
-- blockchain runs. This should be the preferred way of writing such runs.
type DirectBlockChainEffs =
  '[ Validate,
     Query,
     Time,
     Misc
   ]

-- | A blockchain computation built on top of the 'DirectBlockChainEffs' stack
-- of effects.
type DirectBlockChain a = Sem DirectBlockChainEffs a

instance RunnableBlockChain DirectBlockChainEffs where
  runBlockChain pcOpts index nodeConnectInfo =
    runFinal
      . embedToFinal
      . failToEmbed
      . runState pcOpts
      . runState index
      . runError
      . mapError CEToCardanoError
      . mapError CETooFarAway
      . mapError CEAcquiringFailure
      . mapError CEEraMismatch
      . mapError CENodeToClientVersionError
      . runReader nodeConnectInfo
      . runBlockChainMisc
      . runBlockChainLog
      . runBlockChainParams
      . runBlockChainTime
      . runBlockChainQuery
      . runBlockChainSubmit
      . runChainValidate
      . insertAt @7
        @'[ Reader Cardano.LocalNodeConnectInfo,
            Error Cardano.UnsupportedNtcVersionError,
            Error Cardano.EraMismatch,
            Error Cardano.AcquiringFailure,
            Error Cardano.PastHorizonException,
            Error P.Ledger.ToCardanoError,
            Error ChainError,
            State ChainIndex,
            State PrettyCookedOpts,
            Fail,
            Embed IO,
            Final IO
          ]
      . insertAt @4
        @'[ Params,
            Log
          ]
      . insertAt @1
        @'[ Submit
          ]

-- | The full stack of effects required to run a blockchain, including
-- sub-effects usually invisible to the user.
type FullBlockChainEffs =
  '[ Validate,
     Submit,
     Query,
     Time,
     Params,
     Log,
     Misc,
     Reader Cardano.LocalNodeConnectInfo,
     Error Cardano.UnsupportedNtcVersionError,
     Error Cardano.EraMismatch,
     Error Cardano.AcquiringFailure,
     Error Cardano.PastHorizonException,
     Error P.Ledger.ToCardanoError,
     Error ChainError,
     State ChainIndex,
     State PrettyCookedOpts,
     Fail,
     Embed IO,
     Final IO
   ]

-- | A blockchain computation built on top of the `FullBlockChainEffs` stack of effects
type FullBlockChain a = Sem FullBlockChainEffs a

instance RunnableBlockChain FullBlockChainEffs where
  runBlockChain pcOpts index nodeConnectInfo =
    runFinal
      . embedToFinal
      . failToEmbed
      . runState pcOpts
      . runState index
      . runError
      . mapError CEToCardanoError
      . mapError CETooFarAway
      . mapError CEAcquiringFailure
      . mapError CEEraMismatch
      . mapError CENodeToClientVersionError
      . runReader nodeConnectInfo
      . runBlockChainMisc
      . runBlockChainLog
      . runBlockChainParams
      . runBlockChainTime
      . runBlockChainQuery
      . runBlockChainSubmit
      . runChainValidate
