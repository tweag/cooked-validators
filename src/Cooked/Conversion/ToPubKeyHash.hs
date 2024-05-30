-- | Objects from which a public key hash can be extracted
module Cooked.Conversion.ToPubKeyHash where

import Cooked.Wallet
import PlutusLedgerApi.V3 qualified as Api

class ToPubKeyHash a where
  toPubKeyHash :: a -> Api.PubKeyHash

instance ToPubKeyHash Api.PubKeyHash where
  toPubKeyHash = id

instance ToPubKeyHash Wallet where
  toPubKeyHash = walletPKHash
