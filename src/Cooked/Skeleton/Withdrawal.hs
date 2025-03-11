module Cooked.Skeleton.Withdrawal
  ( TxSkelWithdrawals,
    pkWithdrawal,
    scriptWithdrawal,
  )
where

import Cooked.Conversion
import Cooked.Skeleton.Redeemer as X
import Data.Map (Map)
import Data.Map qualified as Map
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Withdrawals associate either a script or a private key with a redeemer and
-- a certain amount of ada. Note that the redeemer will be ignored in the case
-- of a private key.
type TxSkelWithdrawals =
  Map
    (Either (Script.Versioned Script.Script) Api.PubKeyHash)
    (TxSkelRedeemer, Script.Ada)

pkWithdrawal :: (ToPubKeyHash pkh) => pkh -> Script.Ada -> TxSkelWithdrawals
pkWithdrawal pkh amount = Map.singleton (Right $ toPubKeyHash pkh) (emptyTxSkelRedeemer, amount)

scriptWithdrawal :: (ToVersionedScript script) => script -> TxSkelRedeemer -> Script.Ada -> TxSkelWithdrawals
scriptWithdrawal script red amount = Map.singleton (Left $ toVersionedScript script) (red, amount)
