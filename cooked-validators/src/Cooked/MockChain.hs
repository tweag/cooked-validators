{-# Options_GHC -Wno-dodgy-exports #-}

-- | Exports all the machinery necessary for using 'MonadBlockChain' and
--  'MonadMockChain' for writing and testing contracts.
module Cooked.MockChain
  ( module Cooked.MockChain.Constraints,
    module Cooked.MockChain.Time,
    module Cooked.MockChain.UtxoPredicate,
    module Cooked.MockChain.UtxoState,
    module Cooked.MockChain.UtxoState.Testing,
    module Cooked.MockChain.Wallet,
    module Cooked.MockChain.Monad.Staged,
    module Cooked.MockChain.Monad.Direct,
    module Cooked.MockChain.Monad.Contract, -- you're wrong GHC, it exports an important instance.
    module Cooked.MockChain.Monad,
    module Cooked.MockChain.RawUPLC,
    module Cooked.MockChain.Testing,
    SpendableOut,
  )
where

import Cooked.MockChain.Constraints
import Cooked.MockChain.Monad
import Cooked.MockChain.Monad.Contract ()
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.Monad.Staged
import Cooked.MockChain.RawUPLC
import Cooked.MockChain.Testing
import Cooked.MockChain.Time
import Cooked.MockChain.UtxoPredicate
import Cooked.MockChain.UtxoState
import Cooked.MockChain.UtxoState.Testing
import Cooked.MockChain.Wallet
import Cooked.Tx.Constraints.Type (SpendableOut)
