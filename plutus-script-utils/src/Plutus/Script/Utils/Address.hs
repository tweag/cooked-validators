module Plutus.Script.Utils.Address
  ( ToCredential (..),
    ToAddress (..),
    ToPubKeyHash (..),
    ToMaybeStakingCredential (..),
  )
where

import PlutusLedgerApi.V1 qualified as Api

class ToCredential a where
  toCredential :: a -> Api.Credential

instance ToCredential Api.Credential where
  toCredential = id

instance ToCredential Api.ScriptHash where
  toCredential = Api.ScriptCredential

instance ToCredential Api.PubKeyHash where
  toCredential = Api.PubKeyCredential

instance ToCredential Api.Address where
  toCredential = Api.addressCredential

instance (ToCredential a, ToCredential b) => ToCredential (Either a b) where
  toCredential = either toCredential toCredential

-- | 'ToAddress' is more contentious than other similar type classes and should
-- be used with caution because most of the instances will just ignore the
-- staking credential. One can see 'ToAdress' as a synonym of
-- 'ToAddressWithEmptyStakingCredential', which would be a (much too long) more
-- accurate name.
class ToAddress a where
  toAddress :: a -> Api.Address

instance ToAddress Api.ScriptHash where
  toAddress = (`Api.Address` Nothing) . toCredential

instance ToAddress Api.PubKeyHash where
  toAddress = (`Api.Address` Nothing) . toCredential

instance ToAddress Api.Credential where
  toAddress = (`Api.Address` Nothing)

instance ToAddress Api.Address where
  toAddress = id

class ToPubKeyHash a where
  toPubKeyHash :: a -> Api.PubKeyHash

instance ToPubKeyHash Api.PubKeyHash where
  toPubKeyHash = id

-- | Addresses contain an optional staking credential, so it makes more sense to
-- have a class which returns it.
class ToMaybeStakingCredential a where
  toMaybeStakingCredential :: a -> Maybe Api.StakingCredential

instance ToMaybeStakingCredential Api.StakingCredential where
  toMaybeStakingCredential = Just

instance ToMaybeStakingCredential Api.Address where
  toMaybeStakingCredential = Api.addressStakingCredential

instance ToMaybeStakingCredential (Maybe Api.StakingCredential) where
  toMaybeStakingCredential = id
