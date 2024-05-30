-- | Objects from which addresses can be extracted
module Cooked.Conversion.ToAddress where

import Cooked.Wallet
import Plutus.Script.Utils.Typed qualified as Script
import PlutusLedgerApi.V3 qualified as Api

class ToAddress a where
  toAddress :: a -> Api.Address

instance ToAddress Wallet where
  toAddress = walletAddress

instance ToAddress Api.Address where
  toAddress = id

instance ToAddress (Script.TypedValidator a) where
  toAddress = Script.validatorAddress
