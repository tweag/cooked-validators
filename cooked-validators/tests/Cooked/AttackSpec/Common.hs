module Cooked.AttackSpec.Common (tests) where

import Cooked.Attack.Common
import Cooked.MockChain
import Cooked.MockChain.Testing
import Cooked.TestUtils
import Cooked.Tx.Constraints.Optics
import Cooked.Tx.Constraints.Type
import Data.Default
import qualified Ledger.Value as L
import Optics.Core
import qualified Plutus.V1.Ledger.Ada as L
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "building blocks for attacks"
    [ testGroup "mkSelectAttack" $
        let skel =
              txSkel
                [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 123),
                  paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 234),
                  paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 345)
                ]
         in [ testCase "fail if no applicable modifications" $ -- this one is a regression test
                []
                  @=? getAttack
                    ( mkSelectAttack
                        (paysPKWithDatumConstraintsT % valueL)
                        (\_mcst _value -> Nothing)
                        (const True)
                    )
                    def
                    skel,
              testCase "select applied modification by index" $
                [ ( txSkel
                      [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 123),
                        paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 234),
                        paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789)
                      ],
                    [L.lovelaceValueOf 345]
                  )
                ]
                  @=? getAttack
                    ( mkSelectAttack
                        (paysPKWithDatumConstraintsT % valueL)
                        ( \_mcst value ->
                            if value `L.geq` L.lovelaceValueOf 200
                              then Just $ L.lovelaceValueOf 789
                              else Nothing
                        )
                        (== 1)
                    )
                    def
                    skel,
              testCase "return unmodified foci in the right order" $
                [ ( txSkel
                      [ paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789),
                        paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 234),
                        paysPK (walletPKHash $ wallet 1) (L.lovelaceValueOf 789)
                      ],
                    [ L.lovelaceValueOf 123,
                      L.lovelaceValueOf 345
                    ]
                  )
                ]
                  @=? getAttack
                    ( mkSelectAttack
                        (paysPKWithDatumConstraintsT % valueL)
                        (\_mcst _value -> Just $ L.lovelaceValueOf 789)
                        (`elem` [0, 2])
                    )
                    def
                    skel
            ]
    ]
