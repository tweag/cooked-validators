{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.Attack.DupToken (tests) where

import Cooked
import Data.Set qualified as Set
import Optics.Core
import Plutus.Attack.DupToken
import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import Test.Tasty
import Test.Tasty.HUnit

dupTokenTrace :: (MonadBlockChain m) => Script.Versioned Script.MintingPolicy -> Api.TokenName -> Integer -> Wallet -> m ()
dupTokenTrace pol tName amount recipient = validateTxSkel_ skel
  where
    skel =
      let mints = review txSkelMintsListI [mint pol () tName amount]
          mintedValue = Script.toValue mints
       in txSkelTemplate
            { txSkelMints = mints,
              txSkelOuts = [recipient `receives` Value mintedValue],
              txSkelSigners = txSkelSignatoriesFromList [wallet 3]
            }

tests :: TestTree
tests =
  testGroup
    "token duplication attack"
    [ testGroup "unit tests on a 'TxSkel'" $
        let attacker = wallet 6
            tName1 = Api.TokenName "MockToken1"
            tName2 = Api.TokenName "MockToken2"
            pol1 = carefulPolicy tName1 1
            pol2 = carelessPolicy
            ac1 = Api.assetClass (Script.toCurrencySymbol pol1) tName1
            ac2 = Api.assetClass (Script.toCurrencySymbol pol2) tName2
            skelIn =
              txSkelTemplate
                { txSkelMints =
                    review
                      txSkelMintsListI
                      [ mint pol1 () tName1 5,
                        mint pol2 () tName2 7
                      ],
                  txSkelOuts =
                    [ wallet 1 `receives` Value (Api.assetClassValue ac1 1 <> Script.lovelace 1234),
                      wallet 2 `receives` Value (Api.assetClassValue ac2 2)
                    ],
                  txSkelSigners = txSkelSignatoriesFromList [wallet 3]
                }
            skelOut select = runTweak (dupTokenAttack select attacker) skelIn
            skelExpected v1 v2 =
              let increment = Api.assetClassValue ac1 (v1 - 5) <> Api.assetClassValue ac2 (v2 - 7)
               in [ Right
                      ( increment,
                        txSkelTemplate
                          { txSkelLabel = Set.singleton $ TxSkelLabel DupTokenLbl,
                            txSkelMints =
                              review
                                txSkelMintsListI
                                [ mint pol1 () tName1 v1,
                                  mint pol2 () tName2 v2
                                ],
                            txSkelOuts =
                              [ wallet 1 `receives` Value (Api.assetClassValue ac1 1 <> Script.lovelace 1234),
                                wallet 2 `receives` Value (Api.assetClassValue ac2 2),
                                attacker `receives` Value increment
                              ],
                            txSkelSigners = txSkelSignatoriesFromList [wallet 3]
                          }
                      )
                  ]
         in [ testCase "add one token in every asset class" $
                skelExpected 6 8 @=? mcrValue <$> skelOut (\_ _ n -> n + 1),
              testCase "no modified transaction if no increase in value specified" $
                [] @=? mcrValue <$> skelOut (\_ _ n -> n),
              testCase "add tokens depending on the asset class" $
                skelExpected 10 7 @=? mcrValue <$> skelOut (\mp tk n -> if Api.assetClass (Script.toCurrencySymbol mp) tk == ac1 then n + 5 else n)
            ],
      testCooked "careful minting policy" $
        let tName = Api.TokenName "MockToken"
            pol = carefulPolicy tName 1
         in mustFailInPhase2Test $
              somewhere
                (dupTokenAttack (\_ _ n -> n + 1) (wallet 6))
                (dupTokenTrace pol tName 1 (wallet 1)),
      testCooked "careless minting policy" $
        mustSucceedTest $
          somewhere
            (dupTokenAttack (\_ _ n -> n + 1) (wallet 6))
            (dupTokenTrace carelessPolicy (Api.TokenName "MockToken") 1 (wallet 1)),
      testCase "pre-existing tokens are left alone" $
        let attacker = wallet 6
            pol = carelessPolicy
            tName1 = Api.TokenName "mintedToken"
            ac1 = Api.assetClass (Script.toCurrencySymbol pol) tName1
            ac2 = Api.assetClass (Script.toCurrencySymbol Script.trueMintingMPScript) (Api.TokenName "preExistingToken")
            skelIn =
              txSkelTemplate
                { txSkelMints = review txSkelMintsListI [mint pol () tName1 1],
                  txSkelOuts = [wallet 1 `receives` Value (Api.assetClassValue ac1 1 <> Api.assetClassValue ac2 2)],
                  txSkelSigners = txSkelSignatoriesFromList [wallet 2]
                }
            skelExpected =
              [ Right
                  ( Api.assetClassValue ac1 1,
                    txSkelTemplate
                      { txSkelLabel = Set.singleton $ TxSkelLabel DupTokenLbl,
                        txSkelMints = review txSkelMintsListI [mint pol () tName1 2],
                        txSkelOuts =
                          [ wallet 1 `receives` Value (Api.assetClassValue ac1 1 <> Api.assetClassValue ac2 2),
                            attacker `receives` Value (Api.assetClassValue ac1 1)
                          ],
                        txSkelSigners = txSkelSignatoriesFromList [wallet 2]
                      }
                  )
              ]
            skelOut = runTweak (dupTokenAttack (\_ _ i -> i + 1) attacker) skelIn
         in skelExpected @=? mcrValue <$> skelOut
    ]
