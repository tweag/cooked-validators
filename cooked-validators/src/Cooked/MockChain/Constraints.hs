module Cooked.MockChain.Constraints where

import Control.Arrow (second)
import Cooked.MockChain.Monad
import Cooked.Tx.Balance (spendValueFrom)
import Cooked.Tx.Constraints.Type
import qualified Ledger as Pl hiding (singleton, unspentOutputs)
import qualified Ledger.TimeSlot as Pl

-- ** Some supplementary constraints, relying on being in the monad.

-- | Spends some value from a pubkey by selecting the needed utxos belonging
--  to that pubkey and returning the leftover to the same pubkey.
spentByPK :: MonadBlockChain m => Pl.PubKeyHash -> Pl.Value -> m [Constraint]
spentByPK pkh val = do
  -- TODO: maybe turn spentByPK into a pure function: spentByPK val <$> pkUtxos
  allOuts <- pkUtxos pkh
  let (toSpend, leftOver, _) = spendValueFrom val $ map (second Pl.toTxOut) allOuts
  (paysPK pkh leftOver :) . map SpendsPK <$> mapM spendableRef toSpend

-- | Enforces the transaction to be vadiated at a precise slot.
-- It requires to be in the mock chain monad, since slots can be translated to an explicit time range
-- only after inspecting the slot configuration.
validateAtSlot :: MonadMockChain m => Pl.Slot -> m Constraint
validateAtSlot s = do
  slotConf <- slotConfig
  return $ ValidateIn $ Pl.slotToPOSIXTimeRange slotConf s

-- | Validates the transaction in the current time slot of the mock chain.
validateNow :: MonadMockChain m => m Constraint
validateNow =
  validateAtSlot =<< currentSlot
