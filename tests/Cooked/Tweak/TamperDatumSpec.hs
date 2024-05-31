{-# OPTIONS_GHC -Wno-orphans #-}

-- | Tests for 'Cooked.Tweak.TamperDatum'.
module Cooked.Tweak.TamperDatumSpec where

import Cooked
import Data.Set qualified as Set
import Plutus.Script.Utils.Ada qualified as Script
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
            { txSkelLabel = Set.singleton $ TxLabel TamperDatumLbl,
              txSkelOuts =
                [ paysPK alice (Script.lovelaceValueOf 789) `withDatum` (52 :: Integer, 54 :: Integer),
                  paysPK alice (Script.lovelaceValueOf 234) `withDatum` (),
                  paysPK alice (Script.lovelaceValueOf 567) `withDatum` (76 :: Integer, 77 :: Integer)
                ]
            }
        )
    ]
      @=? runTweak
        ( tamperDatumTweak @(Integer, Integer)
            (\(x, y) -> if y == 77 then Nothing else Just (x, y + 1))
        )
        ( txSkelTemplate
            { txSkelOuts =
                [ paysPK alice (Script.lovelaceValueOf 789) `withDatum` (52 :: Integer, 53 :: Integer),
                  paysPK alice (Script.lovelaceValueOf 234) `withDatum` (),
                  paysPK alice (Script.lovelaceValueOf 567) `withDatum` (76 :: Integer, 77 :: Integer)
                ]
            }
        )

malformDatumTweakTest :: TestTree
malformDatumTweakTest =
  testCase "malformDatumTweak" $
    let txSkelWithDatums1And4 :: (PlutusTx.ToData a, PlutusTx.ToData b) => a -> b -> Either MockChainError ((), TxSkel)
        txSkelWithDatums1And4 datum1 datum4 =
          Right
            ( (),
              txSkelTemplate
                { txSkelLabel = Set.singleton $ TxLabel MalformDatumLbl,
                  txSkelOuts =
                    [ paysPK alice (Script.lovelaceValueOf 789) `withDatum` PlutusTx.toBuiltinData datum1,
                      paysPK alice (Script.lovelaceValueOf 234) `withDatum` (),
                      paysPK alice (Script.lovelaceValueOf 567) `withDatum` (76 :: Integer, 77 :: Integer),
                      paysPK alice (Script.lovelaceValueOf 567) `withDatum` PlutusTx.toBuiltinData datum4
                    ]
                }
            )
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
          ( runTweak
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
                      [ paysPK alice (Script.lovelaceValueOf 789) `withDatum` (52 :: Integer, 53 :: Integer),
                        paysPK alice (Script.lovelaceValueOf 234) `withDatum` (),
                        paysPK alice (Script.lovelaceValueOf 567) `withDatum` (76 :: Integer, 77 :: Integer),
                        paysPK alice (Script.lovelaceValueOf 567) `withDatum` (84 :: Integer, 85 :: Integer)
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
