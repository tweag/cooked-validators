-- | This module exposes the notion of Withdrawal within a
-- 'Cooked.Skeleton.TxSkel'
module Cooked.Skeleton.Withdrawal
  ( TxSkelWithdrawals,
    pkWithdrawal,
    scriptWithdrawal,
  )
where

import Cooked.Skeleton.Redeemer
import Data.Map (Map)
import Data.Map qualified as Map
import Plutus.Script.Utils.Address qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Withdrawals associate either a script or a private key with a redeemer and
-- a certain amount of ada. Note that the redeemer will be ignored in the case
-- of a private key.
type TxSkelWithdrawals =
  Map
    (Either (Script.Versioned Script.Script) Api.PubKeyHash)
    (TxSkelRedeemer, Api.Lovelace)

-- | Creates a 'TxSkelWithdrawals' from a private key hash and amount
pkWithdrawal :: (Script.ToPubKeyHash pkh) => pkh -> Integer -> TxSkelWithdrawals
pkWithdrawal pkh amount = Map.singleton (Right $ Script.toPubKeyHash pkh) (emptyTxSkelRedeemer, Api.Lovelace amount)

-- | Creates a 'TxSkelWithdrawals' from a script, redeemer and amount
scriptWithdrawal :: (Script.ToVersioned Script.Script script) => script -> TxSkelRedeemer -> Integer -> TxSkelWithdrawals
scriptWithdrawal script red amount = Map.singleton (Left $ Script.toVersioned script) (red, Api.Lovelace amount)
