-- | This module provides an interface for plutus elements that can be
-- hashed. This is mostly used to provide alias for hashes when pretty printing
-- those elements.
module Cooked.Conversion.ToHash where

import Cooked.Conversion.ToScriptHash
import Cooked.Wallet
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Typed qualified as Script
import PlutusLedgerApi.V3 qualified as Api

class ToHash a where
  toHash :: a -> Api.BuiltinByteString

instance ToHash Api.CurrencySymbol where
  toHash = Api.unCurrencySymbol

instance ToHash Api.PubKeyHash where
  toHash = Api.getPubKeyHash

instance ToHash Wallet where
  toHash = toHash . walletPKHash

instance ToHash (Script.Versioned Script.MintingPolicy) where
  toHash = toHash . Script.scriptCurrencySymbol

instance ToHash (Script.Versioned Script.Script) where
  toHash = toHash . toScriptHash

instance ToHash Script.ScriptHash where
  toHash = Script.getScriptHash

instance ToHash Script.ValidatorHash where
  toHash = Script.getValidatorHash

instance ToHash (Script.TypedValidator a) where
  toHash = toHash . Script.tvValidatorHash

instance ToHash Api.DatumHash where
  toHash (Api.DatumHash hash) = hash

instance ToHash Api.TxId where
  toHash = Api.getTxId

instance ToHash Api.Address where
  toHash (Api.Address (Api.PubKeyCredential pkh) _) = toHash pkh
  toHash (Api.Address (Api.ScriptCredential vh) _) = toHash vh
