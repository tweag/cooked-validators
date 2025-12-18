-- | This module provides a convenient way to spread assets between wallets and
-- scripts at the initialization of the mock chain. These initial assets can be
-- accompanied by datums, staking credentials and reference scripts.
module Cooked.InitialDistribution
  ( InitialDistribution (..),
    distributionFromList,
  )
where

import Cooked.Skeleton
import Cooked.Wallet
import Data.Default
import Data.List (foldl')
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api

-- * Initial distribution of funds

-- | Describes the initial distribution of UTxOs per user.
--
--  The following specifies a starting state where @wallet 1@ owns two UTxOs,
--  one with 42 Ada and one with 2 Ada and one "TOK" token; @wallet 2@ owns a
--  single UTxO with 10 Ada and @wallet 3@ has 10 Ada and a permanent value
--
--  > i0 = distributionFromList $
--  >        [ (wallet 1 , [ ada 42 , ada 2 <> quickValue "TOK" 1 ]
--  >        , (wallet 2 , [ ada 10 ])
--  >        , (wallet 3 , [ ada 10 <> permanentValue "XYZ" 10])
--  >        ]
--
-- Note that payment issued through an initial distribution will be attached
-- enough ADA to sustain themselves.
data InitialDistribution where
  InitialDistribution ::
    {unInitialDistribution :: [TxSkelOut]} ->
    InitialDistribution

-- | 4 UTxOs with 100 Ada each, for each of the first 4 'knownWallets'
instance Default InitialDistribution where
  def = distributionFromList . zip (take 4 knownWallets) . repeat . replicate 4 $ Script.ada 100

instance Semigroup InitialDistribution where
  i <> j = InitialDistribution (unInitialDistribution i <> unInitialDistribution j)

instance Monoid InitialDistribution where
  mempty = InitialDistribution mempty

-- | Creating a initial distribution with simple values assigned to owners
distributionFromList :: (IsTxSkelOutAllowedOwner owner) => [(owner, [Api.Value])] -> InitialDistribution
distributionFromList = InitialDistribution . foldl' (\x (user, values) -> x <> map (receives user . Value) values) []
