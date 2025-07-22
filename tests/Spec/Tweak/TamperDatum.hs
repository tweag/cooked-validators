{-# OPTIONS_GHC -Wno-orphans #-}

-- | Tests for 'Cooked.Tweak.TamperDatum'.
module Spec.Tweak.TamperDatum where

import Cooked
import Data.Either
import Data.Set qualified as Set
import Optics.Core
import Plutus.Script.Utils.Value qualified as Script
import PlutusTx qualified
import Prettyprinter (viaShow)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

instance PrettyCooked (Integer, Integer) where
  prettyCookedOpt _ = viaShow

alice :: Wallet
alice = wallet 1

tamperDatumTweakTest :: TestTree
tamperDatumTweakTest =
  testCase "tamperDatumTweak" $
    [ Right
        ( [(52, 53)],
          txSkelTemplate
            { txSkelLabel = Set.singleton $ TxSkelLabel TamperDatumLbl,
              txSkelOuts =
                [ alice `receives` VisibleHashedDatum (52 :: Integer, 54 :: Integer),
                  alice `receives` Value (Script.lovelace 234),
                  alice `receives` VisibleHashedDatum (76 :: Integer, 77 :: Integer)
                ]
            }
        )
    ]
      @=? mcrValue
        <$> runTweak
          ( tamperDatumTweak @(Integer, Integer)
              (\(x, y) -> if y == 77 then Nothing else Just (x, y + 1))
          )
          ( txSkelTemplate
              { txSkelOuts =
                  [ alice `receives` VisibleHashedDatum (52 :: Integer, 53 :: Integer),
                    alice `receives` Value (Script.lovelace 234),
                    alice `receives` VisibleHashedDatum (76 :: Integer, 77 :: Integer)
                  ]
              }
          )

malformDatumTweakTest :: TestTree
malformDatumTweakTest =
  testCase "malformDatumTweak" $
    let allBuiltinData :: TxSkel -> [PlutusTx.BuiltinData]
        allBuiltinData = toListOf (txSkelOutsL % traversed % txSkelOutDatumL % txSkelOutDatumTypedAT)

        txSkelWithDatums1And4 :: (PlutusTx.ToData a, PlutusTx.ToData b) => a -> b -> [PlutusTx.BuiltinData]
        txSkelWithDatums1And4 datum1 datum4 =
          [ PlutusTx.toBuiltinData datum1,
            PlutusTx.toBuiltinData (76 :: Integer, 77 :: Integer),
            PlutusTx.toBuiltinData datum4
          ]
     in assertSameSets
          [ txSkelWithDatums1And4 (52 :: Integer, ()) (84 :: Integer, 85 :: Integer), -- datum1 changed, datum4 untouched
            txSkelWithDatums1And4 False (84 :: Integer, 85 :: Integer), -- datum1 changed, datum4 untouched
            txSkelWithDatums1And4 (52 :: Integer, ()) (84 :: Integer, ()), -- datum1 changed, datum4 as well
            txSkelWithDatums1And4 False False, -- datum1 changed, datum4 as well
            txSkelWithDatums1And4 (52 :: Integer, ()) False, -- datum1 changed, datum4 as well
            txSkelWithDatums1And4 False (84 :: Integer, ()), -- datum1 changed, datum4 as well
            txSkelWithDatums1And4 (52 :: Integer, 53 :: Integer) (84 :: Integer, ()), -- datum1 untouched, datum4 changed
            txSkelWithDatums1And4 (52 :: Integer, 53 :: Integer) False -- datum1 untouched, datum4 changed
          ]
          ( fmap (allBuiltinData . snd) . rights $
              mcrValue
                <$> runTweak
                  ( malformDatumTweak @(Integer, Integer)
                      ( \(x, y) ->
                          if y == 77
                            then []
                            else
                              [ PlutusTx.toBuiltinData (x, ()),
                                PlutusTx.toBuiltinData False
                              ]
                      )
                  )
                  ( txSkelTemplate
                      { txSkelOuts =
                          [ alice `receives` VisibleHashedDatum (52 :: Integer, 53 :: Integer),
                            alice `receives` Value (Script.lovelace 234),
                            alice `receives` VisibleHashedDatum (76 :: Integer, 77 :: Integer),
                            alice `receives` VisibleHashedDatum (84 :: Integer, 85 :: Integer)
                          ]
                      }
                  )
          )

tests :: TestTree
tests =
  testGroup
    "Tamper datum tweaks"
    [ tamperDatumTweakTest,
      malformDatumTweakTest
    ]
