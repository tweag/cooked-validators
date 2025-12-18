module Cooked.MockChain.AutoFillWithdrawals where

import Control.Monad
import Cooked.MockChain.BlockChain
import Cooked.Skeleton
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
    case currentReward of
      Nothing -> return withdrawal
      Just reward -> do
        let newWithdrawal = autoFillAmount reward withdrawal
        when (view withdrawalMAmountL withdrawal == view withdrawalMAmountL newWithdrawal) $
          logEvent $
            MCLogAutoFilledWithdrawalAmount (view (withdrawalUserL % to Script.toCredential) withdrawal) reward
        return newWithdrawal
  return $ txSkel & txSkelWithdrawalsL % txSkelWithdrawalsListI .~ newWithdrawals
