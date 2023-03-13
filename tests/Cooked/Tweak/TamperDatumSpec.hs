{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

--

module Cooked.Tweak.TamperDatumSpec where

import Cooked
import Cooked.MockChain.Staged (runTweak)
import Cooked.TestUtils (assertSameSets)
import qualified Data.Set as Set
import qualified Plutus.Script.Utils.Ada as Pl
import qualified PlutusTx
import Prettyprinter (viaShow)
import Test.Tasty
import Test.Tasty.HUnit

instance PrettyCooked (Integer, Integer) where
  prettyCookedOpt _ = viaShow

tests :: TestTree
tests =
  testGroup
    "Tamper datum tweaks"
    [ testCase "tamperDatumTweak" $
        [ Right
            ( [(52, 53)],
              txSkelTemplate
                { txSkelLabel = Set.singleton $ TxLabel TamperDatumLbl,
                  txSkelOuts =
                    [ paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 789) `withDatum` (52 :: Integer, 54 :: Integer),
                      paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 234) `withDatum` (),
                      paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 567) `withDatum` (76 :: Integer, 77 :: Integer)
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
                    [ paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 789) `withDatum` (52 :: Integer, 53 :: Integer),
                      paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 234) `withDatum` (),
                      paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 567) `withDatum` (76 :: Integer, 77 :: Integer)
                    ]
                }
            ),
      testCase "malformDatumTweak" $
        let txSkelWithDatums1And4 :: (PlutusTx.ToData a, PlutusTx.ToData b) => a -> b -> Either MockChainError ((), TxSkel)
            txSkelWithDatums1And4 datum1 datum4 =
              Right
                ( (),
                  txSkelTemplate
                    { txSkelLabel = Set.singleton $ TxLabel MalformDatumLbl,
                      txSkelOuts =
                        [ paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 789) `withDatum` PlutusTx.toBuiltinData datum1,
                          paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 234) `withDatum` (),
                          paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 567) `withDatum` (76 :: Integer, 77 :: Integer),
                          paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 567) `withDatum` PlutusTx.toBuiltinData datum4
                        ]
                    }
                )
         in assertSameSets
              [ txSkelWithDatums1And4 (52 :: Integer, ()) (84 :: Integer, 85 :: Integer),
                txSkelWithDatums1And4 False (84 :: Integer, 85 :: Integer),
                txSkelWithDatums1And4 (52 :: Integer, ()) (84 :: Integer, ()),
                txSkelWithDatums1And4 False False,
                txSkelWithDatums1And4 (52 :: Integer, ()) False,
                txSkelWithDatums1And4 False (84 :: Integer, ()),
                txSkelWithDatums1And4 (52 :: Integer, 53 :: Integer) (84 :: Integer, ()),
                txSkelWithDatums1And4 (52 :: Integer, 53 :: Integer) False
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
                          [ paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 789) `withDatum` (52 :: Integer, 53 :: Integer),
                            paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 234) `withDatum` (),
                            paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 567) `withDatum` (76 :: Integer, 77 :: Integer),
                            paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 567) `withDatum` (84 :: Integer, 85 :: Integer)
                          ]
                      }
                  )
              )
    ]
