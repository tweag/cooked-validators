module Spec.Pools where

import Cardano.Api qualified as Cardano
import Cooked.Utilities.Pools
import PlutusLedgerApi.V3 qualified as Api
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@=?))

tests :: TestTree
tests =
  testGroup
    "Pools"
    [ testCase "pool ID is the hash of the cold verification key" $
        let p = pool 3
            expected =
              Api.PubKeyHash $
                Api.toBuiltin $
                  Cardano.serialiseToRawBytes $
                    Cardano.verificationKeyHash (poolColdPK p)
         in expected @=? poolId p,
      testCase "VRF hash is the hash of the VRF verification key" $
        let p = pool 3
            expected =
              Api.PubKeyHash $
                Api.toBuiltin $
                  Cardano.serialiseToRawBytes $
                    Cardano.verificationKeyHash (poolVRFPK p)
         in expected @=? poolVRFHash p,
      testCase "known pool IDs map to their pool numbers" $
        Just 3 @=? poolIdToNumber (poolId $ pool 3),
      testCase "non-known pool IDs do not map to a known pool" $
        Nothing @=? poolIdToNumber (poolId $ pool 11)
    ]
