-- | Objects from which a value can be extracted
module Cooked.Conversion.ToValue where

import Cardano.Api.Ledger qualified as Cardano
import Plutus.Script.Utils.Ada qualified as Script
import PlutusLedgerApi.V3 qualified as Api

class ToValue a where
  toValue :: a -> Api.Value

instance ToValue Api.Value where
  toValue = id

instance ToValue Script.Ada where
  toValue = Script.toValue

instance ToValue Cardano.Coin where
  toValue (Cardano.Coin x) = toValue (Script.Lovelace x)

instance ToValue Api.Lovelace where
  toValue (Api.Lovelace lv) = toValue (Script.Lovelace lv)
