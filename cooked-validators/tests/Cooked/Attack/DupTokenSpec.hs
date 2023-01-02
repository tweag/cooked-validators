{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.Attack.DupTokenSpec (tests) where

import Control.Monad
import Cooked.Attack
import Cooked.Currencies
import Cooked.Ltl
import Cooked.MockChain
import Cooked.Tx.Constraints.Type
import Data.Default
import qualified Data.Set as Set
import qualified Ledger.Ada as L
import qualified Ledger.Scripts as L
import qualified Ledger.Typed.Scripts as L
import qualified Ledger.Value as L
import qualified Plutus.Script.Utils.Scripts as Pl
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import qualified Plutus.V2.Ledger.Contexts as Pl
import qualified PlutusTx as Pl
import qualified PlutusTx.Prelude as Pl
import Test.QuickCheck.Modifiers (NonZero (..))
import Test.Tasty
import Test.Tasty.HUnit

{-# INLINEABLE mkCarefulPolicy #-}
mkCarefulPolicy :: Pl.TokenName -> Integer -> () -> Pl.ScriptContext -> Bool
mkCarefulPolicy tName allowedAmount _ ctx
  | amnt Pl.== Just allowedAmount = True
  | otherwise = Pl.trace "tried to mint wrong amount" False
  where
    txi = Pl.scriptContextTxInfo ctx

    amnt :: Maybe Integer
    amnt = case L.flattenValue (Pl.txInfoMint txi) of
      [(cs, tn, a)] | cs Pl.== Pl.ownCurrencySymbol ctx && tn Pl.== tName -> Just a
      _ -> Nothing

carefulPolicy :: Pl.TokenName -> Integer -> Pl.Versioned Pl.MintingPolicy
carefulPolicy tName allowedAmount =
  flip Pl.Versioned Pl.PlutusV2 . Pl.mkMintingPolicyScript $
    $$(Pl.compile [||\n x -> Pl.mkUntypedMintingPolicy (mkCarefulPolicy n x)||])
      `Pl.applyCode` Pl.liftCode tName
      `Pl.applyCode` Pl.liftCode allowedAmount

{-# INLINEABLE mkCarelessPolicy #-}
mkCarelessPolicy :: () -> Pl.ScriptContext -> Bool
mkCarelessPolicy _ _ = True

carelessPolicy :: Pl.Versioned Pl.MintingPolicy
carelessPolicy =
  flip Pl.Versioned Pl.PlutusV2 $
    Pl.mkMintingPolicyScript
      $$(Pl.compile [||Pl.mkUntypedMintingPolicy mkCarelessPolicy||])

dupTokenTrace :: MonadBlockChain m => Pl.Versioned Pl.MintingPolicy -> Pl.TokenName -> NonZero Integer -> Wallet -> m ()
dupTokenTrace pol tName amount recipient = void $ validateTxSkel' skel
  where
    skel =
      let mints = txSkelMintsFromList [(pol, NoMintsRedeemer, tName, amount)]
          mintedValue = txSkelMintsValue mints
       in mempty
            { txSkelOpts = def {adjustUnbalTx = True},
              txSkelMints = mints,
              txSkelOuts = [paysPK (walletPKHash recipient) mintedValue]
            }

tests :: TestTree
tests =
  testGroup
    "token duplication attack"
    [ testGroup "unit tests on a 'TxSkel'" $
        let attacker = wallet 6
            tName1 = L.tokenName "MockToken1"
            tName2 = L.tokenName "MockToken2"
            pol1 = carefulPolicy tName1 1
            pol2 = carelessPolicy
            ac1 = L.assetClass (L.mpsSymbol $ Pl.mintingPolicyHash pol1) tName1
            ac2 = L.assetClass (L.mpsSymbol $ Pl.mintingPolicyHash pol2) tName2
            skelIn =
              mempty
                { txSkelMints =
                    txSkelMintsFromList
                      [ (pol1, NoMintsRedeemer, tName1, NonZero 5),
                        (pol2, NoMintsRedeemer, tName2, NonZero 7)
                      ],
                  txSkelOuts =
                    [ paysPK (walletPKHash (wallet 1)) (L.assetClassValue ac1 1 <> L.lovelaceValueOf 1234),
                      paysPK (walletPKHash (wallet 2)) (L.assetClassValue ac2 2)
                    ]
                }
            skelOut select = getTweak (dupTokenAttack select attacker) def skelIn
            skelExpected v1 v2 =
              let increment = L.assetClassValue ac1 (v1 - 5) <> L.assetClassValue ac2 (v2 - 7)
               in [ ( mempty
                        { txSkelLabel = Set.singleton $ TxLabel DupTokenLbl,
                          txSkelMints =
                            txSkelMintsFromList
                              [ (pol1, NoMintsRedeemer, tName1, NonZero v1),
                                (pol2, NoMintsRedeemer, tName2, NonZero v2)
                              ],
                          txSkelOuts =
                            [ paysPK (walletPKHash (wallet 1)) (L.assetClassValue ac1 1 <> L.lovelaceValueOf 1234),
                              paysPK (walletPKHash (wallet 2)) (L.assetClassValue ac2 2),
                              paysPK (walletPKHash attacker) increment
                            ]
                        },
                      increment
                    )
                  ]
         in [ testCase "add one token in every asset class" $
                skelExpected 6 8 @=? skelOut (\_ n -> n + 1),
              testCase "no modified transaction if no increase in value specified" $
                [] @=? skelOut (\_ n -> n),
              testCase "add tokens depending on the asset class" $
                skelExpected 10 7 @=? skelOut (\ac n -> if ac == ac1 then n + 5 else n)
            ],
      testCase "careful minting policy" $
        let tName = L.tokenName "MockToken"
            pol = carefulPolicy tName 1
         in testFailsFrom'
              isCekEvaluationFailure
              def
              ( somewhere
                  (dupTokenAttack (\_ n -> n + 1) (wallet 6))
                  (dupTokenTrace pol tName (NonZero 1) (wallet 1))
              ),
      testCase "careless minting policy" $
        let tName = L.tokenName "MockToken"
            pol = carelessPolicy
         in testSucceeds $
              somewhere
                (dupTokenAttack (\_ n -> n + 1) (wallet 6))
                (dupTokenTrace pol tName (NonZero 1) (wallet 1)),
      testCase "pre-existing tokens are left alone" $
        let attacker = wallet 6
            pol = carelessPolicy
            tName1 = L.tokenName "mintedToken"
            ac1 = L.assetClass (L.mpsSymbol $ Pl.mintingPolicyHash pol) tName1
            ac2 = quickAssetClass "preExistingToken"
            skelIn =
              mempty
                { txSkelMints = txSkelMintsFromList [(pol, NoMintsRedeemer, tName1, NonZero 1)],
                  txSkelOuts =
                    [ paysPK
                        (walletPKHash (wallet 1))
                        (L.assetClassValue ac1 1 <> L.assetClassValue ac2 2)
                    ]
                }
            skelExpected =
              [ ( mempty
                    { txSkelLabel = Set.singleton $ TxLabel DupTokenLbl,
                      txSkelMints = txSkelMintsFromList [(pol, NoMintsRedeemer, tName1, NonZero 2)],
                      txSkelOuts =
                        [ paysPK
                            (walletPKHash (wallet 1))
                            (L.assetClassValue ac1 1 <> L.assetClassValue ac2 2),
                          paysPK
                            (walletPKHash attacker)
                            (L.assetClassValue ac1 1)
                        ]
                    },
                  L.assetClassValue ac1 1
                )
              ]
            skelOut = getTweak (dupTokenAttack (\_ i -> i + 1) attacker) def skelIn
         in skelExpected @=? skelOut
    ]
