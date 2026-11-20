{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module provides convenient wrappers for mock stake pools, keeping
-- Cooked's interface independent of the corresponding plutus-ledger API.
module Cooked.Utilities.Pools
  ( Pool,
    knownPools,
    pool,
    poolId,
    poolVRFHash,
    poolIdToNumber,
    poolIdToPool,
    poolIdBech32,
    poolColdPK,
    poolColdSK,
    poolVRFPK,
    poolVRFSK,
    poolKESPK,
    poolKESSK,
  )
where

import Cardano.Api qualified as Cardano
import Data.Function (on)
import Data.List (elemIndex)
import Data.Text (Text)
import Ledger.CardanoPool qualified as P.Ledger
import PlutusLedgerApi.V3 qualified as Api

-- | A 'Pool' is a 'P.Ledger.MockPool' from plutus-ledger.
type Pool = P.Ledger.MockPool

instance Eq Pool where
  (==) = (==) `on` P.Ledger.mpPoolId

instance Ord Pool where
  compare = compare `on` P.Ledger.mpPoolId

-- | The ten pools used by default in mock-chain simulations.
knownPools :: [Pool]
knownPools = P.Ledger.knownMockPools

-- | A deterministic mock pool corresponding to any pool number.
pool :: Int -> Pool
pool = P.Ledger.genPool . P.Ledger.PoolNumber

-- | The pool ID as a Plutus public-key hash, as expected by Cooked's
-- pool-registration and delegation APIs.
poolId :: Pool -> Api.PubKeyHash
poolId =
  Api.PubKeyHash
    . Api.toBuiltin
    . Cardano.serialiseToRawBytes
    . P.Ledger.mpPoolId

-- | The pool's VRF verification-key hash as a Plutus public-key hash.
poolVRFHash :: Pool -> Api.PubKeyHash
poolVRFHash =
  Api.PubKeyHash
    . Api.toBuiltin
    . Cardano.serialiseToRawBytes
    . Cardano.verificationKeyHash
    . poolVRFPK

-- | The number of a known pool corresponding to a pool ID.
poolIdToNumber :: Api.PubKeyHash -> Maybe Int
poolIdToNumber = (succ <$>) . flip elemIndex (poolId <$> knownPools)

-- | The known pool corresponding to a pool ID.
poolIdToPool :: Api.PubKeyHash -> Maybe Pool
poolIdToPool = (pool <$>) . poolIdToNumber

-- | The Bech32 representation of a pool ID.
poolIdBech32 :: Pool -> Text
poolIdBech32 = P.Ledger.poolIdBech32 . P.Ledger.mpPoolId

-- | The pool's cold public key.
poolColdPK :: Pool -> Cardano.VerificationKey Cardano.StakePoolKey
poolColdPK = Cardano.getVerificationKey . poolColdSK

-- | The pool's cold signing key.
poolColdSK :: Pool -> Cardano.SigningKey Cardano.StakePoolKey
poolColdSK = P.Ledger.mpkStakePoolKey . P.Ledger.mpPoolKeys

-- | The pool's VRF public key.
poolVRFPK :: Pool -> Cardano.VerificationKey Cardano.VrfKey
poolVRFPK = Cardano.getVerificationKey . poolVRFSK

-- | The pool's VRF signing key.
poolVRFSK :: Pool -> Cardano.SigningKey Cardano.VrfKey
poolVRFSK = P.Ledger.mpkVrfKey . P.Ledger.mpPoolKeys

-- | The pool's KES public key.
poolKESPK :: Pool -> Cardano.VerificationKey Cardano.KesKey
poolKESPK = Cardano.getVerificationKey . poolKESSK

-- | The pool's KES signing key.
poolKESSK :: Pool -> Cardano.SigningKey Cardano.KesKey
poolKESSK = P.Ledger.mpkKesKey . P.Ledger.mpPoolKeys
