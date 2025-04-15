-- | This module provides an interface for plutus elements that can be
-- hashed. This is used to provide aliases for hashes when pretty printing those
-- elements.
module Cooked.Pretty.Hashable where

import Cooked.Wallet
import Plutus.Script.Utils.Data qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.V1.Typed qualified as Script
import Plutus.Script.Utils.V3.Typed qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- | Hashable elements can be transformed to 'Api.BuiltinByteString'
class ToHash a where
  toHash :: a -> Api.BuiltinByteString

instance ToHash Api.BuiltinByteString where
  toHash = id

instance ToHash Api.CurrencySymbol where
  toHash = Api.unCurrencySymbol

instance ToHash Api.TokenName where
  toHash = Api.unTokenName

instance ToHash Api.PubKeyHash where
  toHash = Api.getPubKeyHash

instance ToHash Wallet where
  toHash = toHash . walletPKHash

instance ToHash (Script.Versioned Script.MintingPolicy) where
  toHash = toHash . Script.toCurrencySymbol

instance ToHash (Script.Versioned Script.Script) where
  toHash = toHash . Script.toScriptHash

instance ToHash Script.ScriptHash where
  toHash = Script.getScriptHash

instance ToHash Script.ValidatorHash where
  toHash = Script.getValidatorHash

instance ToHash (Script.TypedValidator a) where
  toHash = toHash . Script.tvValidatorHash

instance ToHash Api.DatumHash where
  toHash (Api.DatumHash hash) = hash

instance ToHash Api.Datum where
  toHash = toHash . Script.datumHash

instance ToHash Api.BuiltinData where
  toHash = toHash . Script.dataHash

instance ToHash Api.TxId where
  toHash = Api.getTxId

instance ToHash (Script.MultiPurposeScript a) where
  toHash = toHash . Script.toVersioned @Script.Script
