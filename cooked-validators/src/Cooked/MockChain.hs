module Cooked.MockChain
  ( module Cooked.MockChain.Time,
    module Cooked.MockChain.UtxoState,
    module Cooked.MockChain.Wallet,
    module Cooked.MockChain.Monad.Staged,
    module Cooked.MockChain.Monad.Direct,
    module Cooked.MockChain.Monad,
    module Cooked.MockChain.QuickCheck,
    module Cooked.MockChain.HUnit,
    SpendableOut,
    spentByPK,
  )
where

import Control.Arrow (second)
import Cooked.MockChain.HUnit
import Cooked.MockChain.Monad
import Cooked.MockChain.Monad.Direct
import Cooked.MockChain.Monad.Staged
import Cooked.MockChain.QuickCheck
import Cooked.MockChain.Time
import Cooked.MockChain.UtxoState
import Cooked.MockChain.Wallet
import Cooked.Tx.Balance
import Cooked.Tx.Constraints (Constraint (..), SpendableOut)
import qualified Ledger as Pl

-- | Spends some value from a pubkey by selecting the needed utxos belonging
--  to that pubkey and returning the leftover to the same pubkey.
--  This function is here to avoid an import cycle.
spentByPK :: MonadMockChain m => Pl.PubKeyHash -> Pl.Value -> m [Constraint]
spentByPK pkh val = do
  allOuts <- pkUtxos pkh
  let (toSpend, leftOver) = spendValueFrom val $ map (second Pl.toTxOut) allOuts
  (PaysPK pkh leftOver :) . map SpendsPK <$> mapM spendableRef toSpend

{-

-- TODO: Maybe split spentByPK into a pure function, for exaple:

-- | Spends some value from a pubkey by selecting the needed utxos belonging
--  to that pubkey and returning the leftover to the same pubkey.
--  This function is here to avoid an import cycle.
spentByPK :: Pl.Value -> [(Pl.TxOutRef, Pl.TxOut)] -> [Constraint]
spentByPK allOuts val = do
  let (toSpend, leftOver) = spendValueFrom val $ map (second Pl.toTxOut) allOuts
  (PaysPK pkh leftOver :) . map SpendsPK <$> mapM spendableRef toSpend

-- Then, at call sites:
spentByPK val <$> pkUtxos pk
-}
