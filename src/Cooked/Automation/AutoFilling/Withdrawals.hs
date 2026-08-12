-- | This module exposes a function to automatically fill the withdrawn amounts
-- of a 'Cooked.Skeleton.TxSkel' based on the current state of the blockchain.
module Cooked.Automation.AutoFilling.Withdrawals
  ( autoFillWithdrawalAmounts,
  )
where

import Cooked.Effect.Log
import Cooked.Effect.Query
import Cooked.Skeleton
import Cooked.Tweak.Common
import Cooked.Tweak.Update
import Optics.Core
import Plutus.Script.Utils.Address qualified as Script
import Polysemy

-- * Auto filling withdrawal amounts

-- | Goes through all the withdrawals of the input skeleton and attempts to fill
-- out the withdrawn amount based on the associated user rewards. Does not
-- tamper with an existing specified amount in such withdrawals. Logs an event
-- when an amount has been successfully auto-filled.
autoFillWithdrawalAmounts ::
  (Members '[Query, Tweak, Log] effs) =>
  Sem effs ()
autoFillWithdrawalAmounts = do
  traverseTweak (txSkelWithdrawalsL % txSkelWithdrawalsListI % traversed) $ \withdrawal -> do
    currentReward <- getCurrentReward $ view withdrawalUserL withdrawal
    case currentReward of
      Just reward | isn't withdrawalAmountAT withdrawal -> do
        let newWithdrawal = fillAmount reward withdrawal
        logEvent $
          MCLogAutoFilledWithdrawalAmount
            (view (withdrawalUserL % to Script.toCredential) newWithdrawal)
            reward
        return newWithdrawal
      _ -> return withdrawal
