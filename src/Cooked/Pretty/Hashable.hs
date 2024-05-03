module Cooked.Pretty.Hashable where

import Cooked.Wallet
import Plutus.Script.Utils.Scripts qualified as Pl
import Plutus.Script.Utils.Typed qualified as Pl
import Plutus.Script.Utils.Value qualified as Pl
import PlutusLedgerApi.V3 qualified as Pl

class Hashable a where
  toHash :: a -> Pl.BuiltinByteString

instance Hashable Pl.CurrencySymbol where
  toHash = Pl.unCurrencySymbol

instance Hashable Pl.PubKeyHash where
  toHash = Pl.getPubKeyHash

instance Hashable Wallet where
  toHash = toHash . walletPKHash

instance Hashable (Pl.Versioned Pl.MintingPolicy) where
  toHash mintingPolicy =
    let Pl.MintingPolicyHash hash = Pl.mintingPolicyHash mintingPolicy
     in hash

instance Hashable Pl.ScriptHash where
  toHash = Pl.getScriptHash

instance Hashable Pl.ValidatorHash where
  toHash (Pl.ValidatorHash hash) = hash

instance Hashable (Pl.TypedValidator a) where
  toHash = toHash . Pl.validatorAddress

instance Hashable Pl.DatumHash where
  toHash (Pl.DatumHash hash) = hash

instance Hashable Pl.TxId where
  toHash = Pl.getTxId

instance Hashable Pl.Address where
  toHash (Pl.Address (Pl.PubKeyCredential pkh) _) = toHash pkh
  toHash (Pl.Address (Pl.ScriptCredential vh) _) = toHash vh
