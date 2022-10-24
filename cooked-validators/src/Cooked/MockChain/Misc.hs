module Cooked.MockChain.Misc where

import qualified Cardano.Api as Api

theNetworkId :: Api.NetworkId
theNetworkId = Api.Testnet $ Api.NetworkMagic 42 -- TODO PORT what's magic?
