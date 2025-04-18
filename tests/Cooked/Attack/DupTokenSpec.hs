module Cooked.Attack.DupTokenSpec (tests) where

import Control.Monad
import Cooked
import Data.Set qualified as Set
import Plutus.Script.Utils.V3 qualified as Script
import PlutusCore.Version qualified as PlutusTx
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx
import Test.Tasty
import Test.Tasty.HUnit

{-# INLINEABLE carefulPolicyMintingPurpose #-}
carefulPolicyMintingPurpose :: Api.TokenName -> Integer -> Script.MintingPurposeType ()
carefulPolicyMintingPurpose tn n cs _ (Api.TxInfo {txInfoMint}) =
  case Api.flattenValue (Script.toValue txInfoMint) of
    [(cs', tn', n')] -> cs' PlutusTx.== cs && tn' PlutusTx.== tn && n' PlutusTx.== n
    _ -> PlutusTx.trace "tried to mint wrong amount" False

carefulPolicyCompiled :: PlutusTx.CompiledCode (Api.TokenName -> Integer -> PlutusTx.BuiltinData -> PlutusTx.BuiltinUnit)
carefulPolicyCompiled = $$(PlutusTx.compile [||script||])
  where
    script tn n =
      Script.mkMultiPurposeScript $
        Script.falseTypedMultiPurposeScript `Script.withMintingPurpose` carefulPolicyMintingPurpose tn n

carefulPolicy :: Api.TokenName -> Integer -> Script.Versioned Script.MintingPolicy
carefulPolicy tName allowedAmount =
  Script.toVersioned $
    Script.MultiPurposeScript @() $
      Script.toScript $
        carefulPolicyCompiled
          `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PlutusTx.plcVersion110 tName
          `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PlutusTx.plcVersion110 allowedAmount

carelessPolicy :: Script.Versioned Script.MintingPolicy
carelessPolicy = Script.toVersioned Script.trueMintingMPScript

dupTokenTrace :: (MonadBlockChain m) => Script.Versioned Script.MintingPolicy -> Api.TokenName -> Integer -> Wallet -> m ()
dupTokenTrace pol tName amount recipient = void $ validateTxSkel skel
  where
    skel =
      let mints = txSkelMintsFromList [mint pol emptyTxSkelRedeemer tName amount]
          mintedValue = txSkelMintsValue mints
       in txSkelTemplate
            { txSkelMints = mints,
              txSkelOuts = [recipient `receives` Value mintedValue],
              txSkelSigners = [wallet 3]
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
                    txSkelMintsFromList
                      [ mint pol1 emptyTxSkelRedeemer tName1 5,
                        mint pol2 emptyTxSkelRedeemer tName2 7
                      ],
                  txSkelOuts =
                    [ wallet 1 `receives` Value (Api.assetClassValue ac1 1 <> Script.lovelace 1234),
                      wallet 2 `receives` Value (Api.assetClassValue ac2 2)
                    ],
                  txSkelSigners = [wallet 3]
                }
            skelOut select = runTweak (dupTokenAttack select attacker) skelIn
            skelExpected v1 v2 =
              let increment = Api.assetClassValue ac1 (v1 - 5) <> Api.assetClassValue ac2 (v2 - 7)
               in [ Right
                      ( increment,
                        txSkelTemplate
                          { txSkelLabel = Set.singleton $ TxLabel DupTokenLbl,
                            txSkelMints =
                              txSkelMintsFromList
                                [ mint pol1 emptyTxSkelRedeemer tName1 v1,
                                  mint pol2 emptyTxSkelRedeemer tName2 v2
                                ],
                            txSkelOuts =
                              [ wallet 1 `receives` Value (Api.assetClassValue ac1 1 <> Script.lovelace 1234),
                                wallet 2 `receives` Value (Api.assetClassValue ac2 2),
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
        let tName = Api.TokenName "MockToken"
            pol = carefulPolicy tName 1
         in testFailsInPhase2 $
              somewhere
                (dupTokenAttack (\_ n -> n + 1) (wallet 6))
                (dupTokenTrace pol tName 1 (wallet 1)),
      testCase "careless minting policy" $
        let tName = Api.TokenName "MockToken"
            pol = carelessPolicy
         in testSucceeds $
              somewhere
                (dupTokenAttack (\_ n -> n + 1) (wallet 6))
                (dupTokenTrace pol tName 1 (wallet 1)),
      testCase "pre-existing tokens are left alone" $
        let attacker = wallet 6
            pol = carelessPolicy
            tName1 = Api.TokenName "mintedToken"
            ac1 = Api.assetClass (Script.toCurrencySymbol pol) tName1
            ac2 = Api.assetClass (Script.toCurrencySymbol Script.trueMintingMPScript) (Api.TokenName "preExistingToken")
            skelIn =
              txSkelTemplate
                { txSkelMints = txSkelMintsFromList [mint pol emptyTxSkelRedeemer tName1 1],
                  txSkelOuts = [wallet 1 `receives` Value (Api.assetClassValue ac1 1 <> Api.assetClassValue ac2 2)],
                  txSkelSigners = [wallet 2]
                }
            skelExpected =
              [ Right
                  ( Api.assetClassValue ac1 1,
                    txSkelTemplate
                      { txSkelLabel = Set.singleton $ TxLabel DupTokenLbl,
                        txSkelMints = txSkelMintsFromList [mint pol emptyTxSkelRedeemer tName1 2],
                        txSkelOuts =
                          [ wallet 1 `receives` Value (Api.assetClassValue ac1 1 <> Api.assetClassValue ac2 2),
                            attacker `receives` Value (Api.assetClassValue ac1 1)
                          ],
                        txSkelSigners = [wallet 2]
                      }
                  )
              ]
            skelOut = runTweak (dupTokenAttack (\_ i -> i + 1) attacker) skelIn
         in skelExpected @=? fst <$> skelOut
    ]
