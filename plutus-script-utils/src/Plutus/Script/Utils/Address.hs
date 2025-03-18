module Plutus.Script.Utils.Address
  ( ToCredential (..),
    ToAddress (..),
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
  toCredential (Api.Address cred _) = cred

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
