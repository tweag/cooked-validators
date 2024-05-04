-- | This module provides a convenient way to spread assets between
-- wallets and scripts at the initialization of the mock chain. These
-- initial assets can be accompanied by datums and reference scripts.
module Cooked.InitialDistribution
  ( InitialDistribution (..),
    distributionFromList,
  )
where

import Cooked.Skeleton
import Cooked.ValueUtils
import Cooked.Wallet
import Data.Default
import Data.List
import Plutus.Script.Utils.Value qualified as Script

-- * Initial distribution of funds

-- | Describes the initial distribution of UTxOs per wallet. This is
-- important since transaction validation must specify a
-- /collateral/. Hence, wallets must have more than one UTxO to begin
-- with in order to execute a transaction and have some collateral
-- option. The @txCollateral@ is transferred to the node operator in
-- case the transaction fails to validate.
--
--  The following specifies a starting state where @wallet 1@ owns two
--  UTxOs, one with 42 Ada and one with 2 Ada and one "TOK" token;
--  @wallet 2@ owns a single UTxO with 10 Ada and @wallet 3@ has 10
--  Ada and a permanent value. See "Cooked.Currencies" for more
--  information on quick and permanent values.
--
--  > i0 = distributionFromList $
--  >        [ (wallet 1 , [ ada 42 , ada 2 <> quickValue "TOK" 1 ]
--  >        , (wallet 2 , [ ada 10 ])
--  >        , (wallet 3 , [ ada 10 <> permanentValue "XYZ" 10])
--  >        ]
newtype InitialDistribution = InitialDistribution {unInitialDistribution :: [TxSkelOut]}

-- | 5 UTxOs with 100 Ada each, for each of the 'knownWallets',
-- without any datum nor scripts
instance Default InitialDistribution where
  def =
    distributionFromList
      . zip knownWallets
      . repeat
      . replicate 5
      $ ada 100

instance Semigroup InitialDistribution where
  i <> j = InitialDistribution $ unInitialDistribution i <> unInitialDistribution j

instance Monoid InitialDistribution where
  mempty = InitialDistribution mempty

distributionFromList :: [(Wallet, [Script.Value])] -> InitialDistribution
distributionFromList = InitialDistribution . foldl' (\x (user, values) -> x <> map (paysPK (walletPKHash user)) values) []
