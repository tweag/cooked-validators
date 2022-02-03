{-# OPTIONS_GHC -Wno-dodgy-exports #-}

-- | Exports all the machinery necessary for using 'MonadBlockChain' and
--  'MonadMockChain' for writing and testing contracts.
module Cooked.MockChain
  ( module Cooked.MockChain.Time,
    module Cooked.MockChain.UtxoState,
    module Cooked.MockChain.Wallet,
    module Cooked.MockChain.Monad.Staged,
    module Cooked.MockChain.Monad.Direct,
    module Cooked.MockChain.Monad.Contract, -- you're wrong GHC, it exports an important instance.
    module Cooked.MockChain.Monad,
    module Cooked.MockChain.RawUPLC,
    module Cooked.MockChain.Testing,
    SpendableOut,
    spentByPK,
  )
where

import Control.Arrow (second)
import Cooked.MockChain.Monad
import Cooked.MockChain.Monad.Contract ()
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.Monad.Staged
import Cooked.MockChain.RawUPLC
import Cooked.MockChain.Testing
import Cooked.MockChain.Time
import Cooked.MockChain.UtxoState
import Cooked.MockChain.Wallet
import Cooked.Tx.Balance
import Cooked.Tx.Constraints (Constraint (..), SpendableOut)
import qualified Ledger as Pl

-- | Spends some value from a pubkey by selecting the needed utxos belonging
--  to that pubkey and returning the leftover to the same pubkey.
--  This function is here to avoid an import cycle.
spentByPK :: MonadBlockChain m => Pl.PubKeyHash -> Pl.Value -> m [Constraint]
spentByPK pkh val = do
  -- TODO: maybe turn spentByPK into a pure function: spentByPK val <$> pkUtxos
  allOuts <- pkUtxos pkh
  let (toSpend, leftOver, _) = spendValueFrom val $ map (second Pl.toTxOut) allOuts
  (PaysPK pkh leftOver :) . map SpendsPK <$> mapM spendableRef toSpend
