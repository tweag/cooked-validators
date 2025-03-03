module Cooked.Conversion.ToStakingCredential where

import Cooked.Wallet
import PlutusLedgerApi.V1.Address qualified as Api
import PlutusLedgerApi.V3 qualified as Api

class ToMaybeStakingCredential a where
  toMaybeStakingCredential :: a -> Maybe Api.StakingCredential

instance ToMaybeStakingCredential Api.StakingCredential where
  toMaybeStakingCredential = Just

instance ToMaybeStakingCredential (Maybe Api.StakingCredential) where
  toMaybeStakingCredential = id

instance ToMaybeStakingCredential Api.Address where
  toMaybeStakingCredential = Api.stakingCredential

instance ToMaybeStakingCredential Wallet where
  toMaybeStakingCredential = walletStakingCredential
