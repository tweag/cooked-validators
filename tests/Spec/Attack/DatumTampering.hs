-- | Tests for 'Cooked.Attack.DatumTampering'.
module Spec.Attack.DatumTampering where

import Cooked
import Data.Set qualified as Set
import Optics.Core
import Plutus.Script.Utils.Value qualified as Script
import PlutusTx qualified
import Polysemy
import Polysemy.NonDet
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

alice :: Wallet
alice = wallet 1

datumTamperingAttackTest :: TestTree
datumTamperingAttackTest =
  testCase "datumTamperingAttack" $
    [ txSkelEmulatorTemplate
        { txSkelLabels = Set.singleton $ TxSkelLabel $ DatumTamperingLabel [(52 :: Integer, 53 :: Integer)],
          txSkelOutputs =
            [ alice `receives` VisibleHashedDatum (52 :: Integer, 54 :: Integer),
              alice `receives` Value (Script.lovelace 234),
              alice `receives` VisibleHashedDatum (76 :: Integer, 77 :: Integer)
            ]
        }
    ]
      @=? (run . runNonDet)
        ( execTweak
            txSkelEmulatorTemplate
              { txSkelOutputs =
                  [ alice `receives` VisibleHashedDatum (52 :: Integer, 53 :: Integer),
                    alice `receives` Value (Script.lovelace 234),
                    alice `receives` VisibleHashedDatum (76 :: Integer, 77 :: Integer)
                  ]
              }
            ( datumTamperingAttack $
                allDatumTamperingParams @(Integer, Integer)
                  OneBranchForAllFoci
                  (\(x, y) -> if y == 77 then Nothing else Just (x, y + 1))
            )
        )

malformDatumAttackTest :: TestTree
malformDatumAttackTest =
  testCase "malformDatumAttack" $
    let allBuiltinData :: TxSkel -> [PlutusTx.BuiltinData]
        allBuiltinData = toListOf (txSkelOutputsL % traversed % txSkelOutDatumL % txSkelOutDatumTypedAT)

        txSkelWithDatums1And4 :: (PlutusTx.ToData a, PlutusTx.ToData b) => a -> b -> [PlutusTx.BuiltinData]
        txSkelWithDatums1And4 datum1 datum4 =
          [ PlutusTx.toBuiltinData datum1,
            PlutusTx.toBuiltinData (76 :: Integer, 77 :: Integer),
            PlutusTx.toBuiltinData datum4
          ]
     in assertSameSets
          [ txSkelWithDatums1And4 (52 :: Integer, ()) (84 :: Integer, 85 :: Integer), -- datum1 changed, datum4 untouched
            txSkelWithDatums1And4 False (84 :: Integer, 85 :: Integer), -- datum1 changed, datum4 untouched
            txSkelWithDatums1And4 (52 :: Integer, 53 :: Integer) (84 :: Integer, ()), -- datum1 untouched, datum4 changed
            txSkelWithDatums1And4 (52 :: Integer, 53 :: Integer) False -- datum1 untouched, datum4 changed
          ]
          ( (fmap allBuiltinData . run . runNonDet)
              ( execTweak
                  ( txSkelEmulatorTemplate
                      { txSkelOutputs =
                          [ alice `receives` VisibleHashedDatum (52 :: Integer, 53 :: Integer),
                            alice `receives` Value (Script.lovelace 234),
                            alice `receives` VisibleHashedDatum (76 :: Integer, 77 :: Integer),
                            alice `receives` VisibleHashedDatum (84 :: Integer, 85 :: Integer)
                          ]
                      }
                  )
                  ( datumTamperingAttack $
                      allDatumTamperingParams @(Integer, Integer)
                        OneBranchPerFoci
                        ( \(x, y) ->
                            if y == 77
                              then []
                              else
                                [ PlutusTx.toBuiltinData (x, ()),
                                  PlutusTx.toBuiltinData False
                                ]
                        )
                  )
              )
          )

tests :: TestTree
tests =
  testGroup
    "Tamper datum tweaks"
    [ datumTamperingAttackTest,
      malformDatumAttackTest
    ]
