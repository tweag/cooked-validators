module Cooked.MockChain.AutoFillWithdrawals where

import Control.Monad
import Cooked.MockChain.BlockChain
import Cooked.Skeleton
import Data.Maybe
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script

-- | Goes through all the withdrawals of the input skeleton and attempts to fill
-- out the withdrawn amount based on the associated user rewards. Does not
-- tamper with an existing specify amount in such withdrawals. Logs an event
-- when an amount has been successfully auto-filled.
toTxSkelWithAutoFilledWithdrawalAmounts :: (MonadBlockChainWithoutValidation m) => TxSkel -> m TxSkel
toTxSkelWithAutoFilledWithdrawalAmounts txSkel = do
  let withdrawals = view (txSkelWithdrawalsL % txSkelWithdrawalsListI) txSkel
  newWithdrawals <- forM withdrawals $ \withdrawal -> do
    currentReward <- getCurrentReward $ view withdrawalUserL withdrawal
    let (changed, newWithdrawal) = case currentReward of
          Nothing -> (False, withdrawal)
          Just reward -> (isn't withdrawalAmountAT withdrawal, fillAmount reward withdrawal)
    when changed $
      logEvent $
        MCLogAutoFilledWithdrawalAmount
          (view (withdrawalUserL % to Script.toCredential) newWithdrawal)
          (fromJust (preview withdrawalAmountAT newWithdrawal))
    return newWithdrawal
  return $ txSkel & txSkelWithdrawalsL % txSkelWithdrawalsListI .~ newWithdrawals
