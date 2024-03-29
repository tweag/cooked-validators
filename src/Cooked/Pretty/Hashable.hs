{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cooked.Pretty.Hashable where

import Cooked.Wallet (Wallet, walletPKHash)
import qualified Plutus.Script.Utils.Scripts as Pl
import qualified Plutus.Script.Utils.Typed as Pl
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Pl
import qualified Plutus.Script.Utils.Value as Pl
import qualified Plutus.V1.Ledger.Tx as Pl
import qualified Plutus.V2.Ledger.Api as Pl

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
