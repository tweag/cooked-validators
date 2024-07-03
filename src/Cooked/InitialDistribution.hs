-- | This module provides a convenient way to spread assets between wallets and
-- scripts at the initialization of the mock chain. These initial assets can be
-- accompanied by datums, staking credentials and reference scripts.
module Cooked.InitialDistribution
  ( InitialDistribution (..),
    distributionFromList,
    toInitDistWithMinAda,
  )
where

import Control.Monad
import Cooked.MockChain.MinAda
import Cooked.Skeleton
import Cooked.ValueUtils
import Cooked.Wallet
import Data.Default
import Data.List
import PlutusLedgerApi.V3 qualified as Api

-- * Initial distribution of funds

-- | Describes the initial distribution of UTxOs per wallet. This is important
-- since transaction validation must specify a /collateral/. Hence, wallets must
-- have more than one UTxO to begin with in order to execute a transaction and
-- have some collateral option. The @txCollateral@ is transferred to the node
-- operator in case the transaction fails to validate.
--
--  The following specifies a starting state where @wallet 1@ owns two UTxOs,
--  one with 42 Ada and one with 2 Ada and one "TOK" token; @wallet 2@ owns a
--  single UTxO with 10 Ada and @wallet 3@ has 10 Ada and a permanent value. See
--  "Cooked.Currencies" for more information on quick and permanent values.
--
--  > i0 = distributionFromList $
--  >        [ (wallet 1 , [ ada 42 , ada 2 <> quickValue "TOK" 1 ]
--  >        , (wallet 2 , [ ada 10 ])
--  >        , (wallet 3 , [ ada 10 <> permanentValue "XYZ" 10])
--  >        ]
--
-- Note that initial distribution can lead to payments that would not be
-- accepted if part of an actual transaction, such as payment without enough ada
-- to sustain themselves.
data InitialDistribution where
  InitialDistribution ::
    {unInitialDistribution :: [TxSkelOut]} ->
    InitialDistribution

-- | 5 UTxOs with 100 Ada each, for each of the 'knownWallets'
instance Default InitialDistribution where
  def = distributionFromList . zip knownWallets . repeat . replicate 5 $ ada 100

instance Semigroup InitialDistribution where
  i <> j = InitialDistribution (unInitialDistribution i <> unInitialDistribution j)

instance Monoid InitialDistribution where
  mempty = InitialDistribution mempty

-- | Transform a given initial distribution by ensuring each payment has enough
-- ada so that the resulting outputs can sustain themselves.
toInitDistWithMinAda :: InitialDistribution -> InitialDistribution
toInitDistWithMinAda (InitialDistribution initDist) =
  case forM initDist $ toTxSkelOutWithMinAda def of
    Left err -> error $ show err
    Right initDist' -> InitialDistribution initDist'

toPaysList :: [(Wallet, [Api.Value])] -> [TxSkelOut]
toPaysList = foldl' (\x (user, values) -> x <> map (paysPK user) values) []

-- | Creating a initial distribution with simple values assigned to wallets
distributionFromList :: [(Wallet, [Api.Value])] -> InitialDistribution
distributionFromList = InitialDistribution . toPaysList
