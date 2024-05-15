-- | This module provides an interface for plutus elements that can be
-- hashed. This is mostly used to provide alias for hashes when pretty printing
-- those elements.
module Cooked.Pretty.Hashable where

import Cooked.Wallet
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Typed qualified as Script
import PlutusLedgerApi.V3 qualified as Api

class Hashable a where
  toHash :: a -> Api.BuiltinByteString

instance Hashable Api.CurrencySymbol where
  toHash = Api.unCurrencySymbol

instance Hashable Api.PubKeyHash where
  toHash = Api.getPubKeyHash

instance Hashable Wallet where
  toHash = toHash . walletPKHash

instance Hashable (Script.Versioned Script.MintingPolicy) where
  toHash = Script.getMintingPolicyHash . Script.mintingPolicyHash

instance Hashable Script.ScriptHash where
  toHash = Script.getScriptHash

instance Hashable Script.ValidatorHash where
  toHash = Script.getValidatorHash

instance Hashable (Script.TypedValidator a) where
  toHash = toHash . Script.tvValidatorHash

instance Hashable Api.DatumHash where
  toHash (Api.DatumHash hash) = hash

instance Hashable Api.TxId where
  toHash = Api.getTxId

instance Hashable Api.Address where
  toHash (Api.Address (Api.PubKeyCredential pkh) _) = toHash pkh
  toHash (Api.Address (Api.ScriptCredential vh) _) = toHash vh
