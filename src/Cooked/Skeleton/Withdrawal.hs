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

pkWithdrawal :: (Script.ToPubKeyHash pkh) => pkh -> Api.Lovelace -> TxSkelWithdrawals
pkWithdrawal pkh amount = Map.singleton (Right $ Script.toPubKeyHash pkh) (emptyTxSkelRedeemer, amount)

scriptWithdrawal :: (Script.ToVersioned Script.Script script) => script -> TxSkelRedeemer -> Api.Lovelace -> TxSkelWithdrawals
scriptWithdrawal script red amount = Map.singleton (Left $ Script.toVersioned script) (red, amount)
