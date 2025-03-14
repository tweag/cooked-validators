module Cooked.Attack.DupTokenSpec (tests) where

import Control.Monad
import Cooked
import Data.Set qualified as Set
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Typed qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusLedgerApi.V3.Contexts qualified as Api
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx
import Test.Tasty
import Test.Tasty.HUnit

{-# INLINEABLE mkCarefulPolicy #-}
mkCarefulPolicy :: Api.TokenName -> Integer -> () -> Api.ScriptContext -> Bool
mkCarefulPolicy tName allowedAmount _ ctx
  | amnt PlutusTx.== Just allowedAmount = True
  | otherwise = PlutusTx.trace "tried to mint wrong amount" False
  where
    txi = Api.scriptContextTxInfo ctx

    amnt :: Maybe Integer
    amnt = case Script.flattenValue (Api.txInfoMint txi) of
      [(cs, tn, a)] | cs PlutusTx.== Api.ownCurrencySymbol ctx && tn PlutusTx.== tName -> Just a
      _ -> Nothing

carefulPolicy :: Api.TokenName -> Integer -> Script.Versioned Script.MintingPolicy
carefulPolicy tName allowedAmount =
  case ($$(PlutusTx.compile [||\n x -> Script.mkUntypedMintingPolicy (mkCarefulPolicy n x)||]) `PlutusTx.applyCode` PlutusTx.liftCodeDef tName)
    >>= (`PlutusTx.applyCode` PlutusTx.liftCodeDef allowedAmount) of
    Left s -> error $ "Can't apply parameters in carefulPolicy: " ++ show s
    Right code -> flip Script.Versioned Script.PlutusV3 . Script.mkMintingPolicyScript $ code

{-# INLINEABLE mkCarelessPolicy #-}
mkCarelessPolicy :: () -> Api.ScriptContext -> Bool
mkCarelessPolicy _ _ = True

carelessPolicy :: Script.Versioned Script.MintingPolicy
carelessPolicy =
  flip Script.Versioned Script.PlutusV3 $
    Script.mkMintingPolicyScript
      $$(PlutusTx.compile [||Script.mkUntypedMintingPolicy mkCarelessPolicy||])

dupTokenTrace :: (MonadBlockChain m) => Script.Versioned Script.MintingPolicy -> Api.TokenName -> Integer -> Wallet -> m ()
dupTokenTrace pol tName amount recipient = void $ validateTxSkel skel
  where
    skel =
      let mints = txSkelMintsFromList [(pol, emptyTxSkelRedeemer, tName, amount)]
          mintedValue = txSkelMintsValue mints
       in txSkelTemplate
            { txSkelMints = mints,
              txSkelOuts = [recipient `receives` AdjustableValue mintedValue],
              txSkelSigners = [wallet 3]
            }

tests :: TestTree
tests =
  testGroup
    "token duplication attack"
    [ testGroup "unit tests on a 'TxSkel'" $
        let attacker = wallet 6
            tName1 = Script.tokenName "MockToken1"
            tName2 = Script.tokenName "MockToken2"
            pol1 = carefulPolicy tName1 1
            pol2 = carelessPolicy
            ac1 = Script.assetClass (Script.mpsSymbol $ Script.mintingPolicyHash pol1) tName1
            ac2 = Script.assetClass (Script.mpsSymbol $ Script.mintingPolicyHash pol2) tName2
            skelIn =
              txSkelTemplate
                { txSkelMints =
                    txSkelMintsFromList
                      [ (pol1, emptyTxSkelRedeemer, tName1, 5),
                        (pol2, emptyTxSkelRedeemer, tName2, 7)
                      ],
                  txSkelOuts =
                    [ wallet 1 `receives` Value (Script.assetClassValue ac1 1 <> Script.lovelace 1234),
                      wallet 2 `receives` Value (Script.assetClassValue ac2 2)
                    ],
                  txSkelSigners = [wallet 3]
                }
            skelOut select = runTweak (dupTokenAttack select attacker) skelIn
            skelExpected v1 v2 =
              let increment = Script.assetClassValue ac1 (v1 - 5) <> Script.assetClassValue ac2 (v2 - 7)
               in [ Right
                      ( increment,
                        txSkelTemplate
                          { txSkelLabel = Set.singleton $ TxLabel DupTokenLbl,
                            txSkelMints =
                              txSkelMintsFromList
                                [ (pol1, emptyTxSkelRedeemer, tName1, v1),
                                  (pol2, emptyTxSkelRedeemer, tName2, v2)
                                ],
                            txSkelOuts =
                              [ wallet 1 `receives` Value (Script.assetClassValue ac1 1 <> Script.lovelace 1234),
                                wallet 2 `receives` Value (Script.assetClassValue ac2 2),
                                attacker `receives` Value increment
                              ],
                            txSkelSigners = [wallet 3]
                          }
                      )
                  ]
         in [ testCase "add one token in every asset class" $
                skelExpected 6 8 @=? fst <$> skelOut (\_ n -> n + 1),
              testCase "no modified transaction if no increase in value specified" $
                [] @=? fst <$> skelOut (\_ n -> n),
              testCase "add tokens depending on the asset class" $
                skelExpected 10 7 @=? fst <$> skelOut (\ac n -> if ac == ac1 then n + 5 else n)
            ],
      testCase "careful minting policy" $
        let tName = Script.tokenName "MockToken"
            pol = carefulPolicy tName 1
         in testFailsInPhase2 $
              somewhere
                (dupTokenAttack (\_ n -> n + 1) (wallet 6))
                (dupTokenTrace pol tName 1 (wallet 1)),
      testCase "careless minting policy" $
        let tName = Script.tokenName "MockToken"
            pol = carelessPolicy
         in testSucceeds $
              somewhere
                (dupTokenAttack (\_ n -> n + 1) (wallet 6))
                (dupTokenTrace pol tName 1 (wallet 1)),
      testCase "pre-existing tokens are left alone" $
        let attacker = wallet 6
            pol = carelessPolicy
            tName1 = Script.tokenName "mintedToken"
            ac1 = Script.assetClass (Script.mpsSymbol $ Script.mintingPolicyHash pol) tName1
            ac2 = quickAssetClass "preExistingToken"
            skelIn =
              txSkelTemplate
                { txSkelMints = txSkelMintsFromList [(pol, emptyTxSkelRedeemer, tName1, 1)],
                  txSkelOuts = [wallet 1 `receives` Value (Script.assetClassValue ac1 1 <> Script.assetClassValue ac2 2)],
                  txSkelSigners = [wallet 2]
                }
            skelExpected =
              [ Right
                  ( Script.assetClassValue ac1 1,
                    txSkelTemplate
                      { txSkelLabel = Set.singleton $ TxLabel DupTokenLbl,
                        txSkelMints = txSkelMintsFromList [(pol, emptyTxSkelRedeemer, tName1, 2)],
                        txSkelOuts =
                          [ wallet 1 `receives` Value (Script.assetClassValue ac1 1 <> Script.assetClassValue ac2 2),
                            attacker `receives` Value (Script.assetClassValue ac1 1)
                          ],
                        txSkelSigners = [wallet 2]
                      }
                  )
              ]
            skelOut = runTweak (dupTokenAttack (\_ i -> i + 1) attacker) skelIn
         in skelExpected @=? fst <$> skelOut
    ]
