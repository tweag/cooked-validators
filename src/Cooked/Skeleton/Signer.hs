-- | This module exposes the notion of signer for out 'Cooked.Skeleton.TxSkel'
module Cooked.Skeleton.Signer
  ( -- * Data types
    TxSkelSigner,

    -- * Smart constructors
    signer,
    txSkelSignersFromList,
  )
where

import Cooked.Skeleton.User
import Data.Typeable
import Plutus.Script.Utils.V3 qualified as Script

-- | An alias used to reprensent signers of skeletons
type TxSkelSigner = User 'IsPubKey 'Allocation

-- | Builds a signer
signer :: (Script.ToPubKeyHash pkh, Typeable pkh) => pkh -> TxSkelSigner
signer = UserPubKey

-- | Builds a list of signers
txSkelSignersFromList :: (Script.ToPubKeyHash pkh, Typeable pkh) => [pkh] -> [TxSkelSigner]
txSkelSignersFromList = map signer
