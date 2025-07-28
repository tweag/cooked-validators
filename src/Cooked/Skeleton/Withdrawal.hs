{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exposes the notion of Withdrawal within a
-- 'Cooked.Skeleton.TxSkel'
module Cooked.Skeleton.Withdrawal
  ( TxSkelWithdrawals,
    pkWithdrawal,
    scriptWithdrawal,
  )
where

import Cooked.Skeleton.Redeemer
import Cooked.Skeleton.Scripts
import Data.Map (Map)
import Data.Map qualified as Map
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Withdrawals associate either a script or a private key with a redeemer and
-- a certain amount of ada.
type TxSkelWithdrawals = Map (User IsEither Redemption) Api.Lovelace

-- | Creates a 'TxSkelWithdrawals' from a private key hash and lovelace amount
pkWithdrawal :: (Script.ToPubKeyHash pkh) => pkh -> Integer -> TxSkelWithdrawals
pkWithdrawal pkh = Map.singleton (UserPubKeyHash pkh) . Api.Lovelace

-- | Creates a 'TxSkelWithdrawals' from a redeemed script and lovelace amount
scriptWithdrawal :: (ToVScript script) => script -> TxSkelRedeemer -> Integer -> TxSkelWithdrawals
scriptWithdrawal script red = Map.singleton (UserRedeemedScript script red) . Api.Lovelace

-- | Retrieves the total value withdrawn is this 'TxSkelWithdrawals'
instance {-# OVERLAPPING #-} Script.ToValue TxSkelWithdrawals where
  toValue = foldl (\val -> (val <>) . Script.toValue) mempty
