{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

--

module Cooked.Tweak.TamperDatumSpec where

import Cooked
import Cooked.MockChain.Staged (runTweak)
import qualified Data.Set as Set
import qualified Plutus.Script.Utils.Ada as Pl
import Prettyprinter (viaShow)
import Test.Tasty
import Test.Tasty.HUnit

instance PrettyCooked (Integer, Integer) where
  prettyCookedOpt _ = viaShow

tests :: TestTree
tests =
  testGroup
    "Tamper datum tweaks"
    ( let skel =
            txSkelTemplate
              { txSkelOuts =
                  [ paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 789) `withDatum` (52 :: Integer, 53 :: Integer),
                    paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 234) `withDatum` (),
                    paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 567) `withDatum` (76 :: Integer, 77 :: Integer)
                  ]
              }
       in [ testCase "tamperDatumTweak" $
              [ Right
                  ( [ (52, 53),
                      (76, 77)
                    ],
                    txSkelTemplate
                      { txSkelLabel = Set.singleton $ TxLabel TamperDatumLbl,
                        txSkelOuts =
                          [ paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 789) `withDatum` (52 :: Integer, 54 :: Integer),
                            paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 234) `withDatum` (),
                            paysPK (walletPKHash $ wallet 1) (Pl.lovelaceValueOf 567) `withDatum` (76 :: Integer, 78 :: Integer)
                          ]
                      }
                  )
              ]
                @=? runTweak
                  ( tamperDatumTweak @(Integer, Integer)
                      (\(x, y) -> Just (x, y + 1))
                  )
                  skel
          ]
    )
