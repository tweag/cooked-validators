{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.AttackSpec.DupToken (tests) where

import Control.Monad
import Cooked.Attack.Common
import Cooked.Attack.DupToken
import Cooked.AttackSpec.Util
import Cooked.Currencies
import Cooked.Ltl
import Cooked.MockChain
import Cooked.Tx.Constraints
import Data.Default
import qualified Ledger.Ada as L
import qualified Ledger.Scripts as L
import qualified Ledger.Typed.Scripts as L
import qualified Ledger.Value as L
import qualified Plutus.Script.Utils.V1.Scripts as L
import qualified Plutus.V1.Ledger.Contexts as L
import qualified PlutusTx as Pl
import qualified PlutusTx.Prelude as Pl
import Test.Tasty
import Test.Tasty.HUnit

{-# INLINEABLE mkCarefulPolicy #-}
mkCarefulPolicy :: L.TokenName -> Integer -> () -> L.ScriptContext -> Bool
mkCarefulPolicy tName allowedAmount _ ctx
  | amnt Pl.== Just allowedAmount = True
  | otherwise = Pl.trace "tried to mint wrong amount" False
  where
    txi = L.scriptContextTxInfo ctx

    amnt :: Maybe Integer
    amnt = case L.flattenValue (L.txInfoMint txi) of
      [(cs, tn, a)] | cs Pl.== L.ownCurrencySymbol ctx && tn Pl.== tName -> Just a
      _ -> Nothing

carefulPolicy :: L.TokenName -> Integer -> L.MintingPolicy
carefulPolicy tName allowedAmount =
  L.mkMintingPolicyScript $
    $$(Pl.compile [||\n x -> L.mkUntypedMintingPolicy (mkCarefulPolicy n x)||])
      `Pl.applyCode` Pl.liftCode tName
      `Pl.applyCode` Pl.liftCode allowedAmount

{-# INLINEABLE mkCarelessPolicy #-}
mkCarelessPolicy :: () -> L.ScriptContext -> Bool
mkCarelessPolicy _ _ = True

carelessPolicy :: L.MintingPolicy
carelessPolicy =
  L.mkMintingPolicyScript
    $$(Pl.compile [||L.mkUntypedMintingPolicy mkCarelessPolicy||])

dupTokenTrace :: MonadBlockChain m => L.MintingPolicy -> L.TokenName -> Integer -> Wallet -> m ()
dupTokenTrace pol tName amount recipient = void $ validateTxSkel skel
  where
    skel =
      txSkelOpts (def {adjustUnbalTx = True}) $
        let minted = L.singleton (L.scriptCurrencySymbol pol) tName amount
         in [Mints (Nothing @()) [pol] minted]
              :=>: [paysPK (walletPKHash recipient) minted]

tests :: TestTree
tests =
  testGroup
    "token duplication attack"
    [ testCase "unit test on a 'TxSkel'" $
        let attacker = wallet 6
            tName1 = L.tokenName "MockToken1"
            tName2 = L.tokenName "MockToken2"
            pol1 = carefulPolicy tName1 1
            pol2 = carelessPolicy
            ac1 = L.assetClass (L.scriptCurrencySymbol pol1) tName1
            ac2 = L.assetClass (L.scriptCurrencySymbol pol2) tName2
            skelIn =
              txSkel
                ( [ Mints (Nothing @()) [pol1, pol2] (L.assetClassValue ac1 1 <> L.assetClassValue ac2 1),
                    Mints (Nothing @()) [pol2] (L.assetClassValue ac2 3),
                    Mints (Nothing @()) [pol1] (L.assetClassValue ac1 7)
                  ]
                    :=>: [ paysPK (walletPKHash (wallet 1)) (L.assetClassValue ac1 1 <> L.lovelaceValueOf 1234),
                           paysPK (walletPKHash (wallet 2)) (L.assetClassValue ac2 2)
                         ]
                )
            skelOut select =
              case dupTokenAttack select attacker of
                Attack f -> f def skelIn
            skelExpected v1 v2 v3 v4 =
              Just
                ( txSkelLbl
                    DupTokenLbl
                    ( [ Mints (Nothing @()) [pol1, pol2] (L.assetClassValue ac1 v1 <> L.assetClassValue ac2 v2),
                        Mints (Nothing @()) [pol2] (L.assetClassValue ac2 v3),
                        Mints (Nothing @()) [pol1] (L.assetClassValue ac1 v4)
                      ]
                        :=>: [ paysPK (walletPKHash (wallet 1)) (L.assetClassValue ac1 1 <> L.lovelaceValueOf 1234),
                               paysPK (walletPKHash (wallet 2)) (L.assetClassValue ac2 2),
                               paysPK
                                 (walletPKHash attacker)
                                 (L.assetClassValue ac1 ((v1 - 1) + (v4 - 7)) <> L.assetClassValue ac2 ((v2 - 1) + (v3 - 3)))
                             ]
                    ),
                  L.assetClassValue ac1 ((v1 - 1) + (v4 - 7)) <> L.assetClassValue ac2 ((v2 -1) + (v3 -3))
                )
         in (skelExpected 2 2 4 8 @=? skelOut (\_ n -> n + 1))
              .&&. (Nothing @=? skelOut (\_ n -> n))
              .&&. (skelExpected 6 1 3 12 @=? skelOut (\ac n -> if ac == ac1 then n + 5 else n)),
      testCase "careful minting policy" $
        let tName = L.tokenName "MockToken"
            pol = carefulPolicy tName 1
         in testFailsFrom'
              isCekEvaluationFailure
              def
              ( somewhere
                  (UntypedAttack $ dupTokenAttack (\_ n -> n + 1) (wallet 6))
                  (dupTokenTrace pol tName 1 (wallet 1))
              ),
      testCase "careless minting policy" $
        let tName = L.tokenName "MockToken"
            pol = carelessPolicy
         in testSucceeds $
              somewhere
                (UntypedAttack $ dupTokenAttack (\_ n -> n + 1) (wallet 6))
                (dupTokenTrace pol tName 1 (wallet 1)),
      testCase "pre-existing tokens are left alone" $
        let attacker = wallet 6
            pol = carelessPolicy
            ac1 = L.assetClass (L.scriptCurrencySymbol pol) (L.tokenName "mintedToken")
            ac2 = quickAssetClass "preExistingToken"
            skelIn =
              txSkel
                ( [Mints (Nothing @()) [pol] (L.assetClassValue ac1 1)]
                    :=>: [ paysPK
                             (walletPKHash (wallet 1))
                             (L.assetClassValue ac1 1 <> L.assetClassValue ac2 2)
                         ]
                )
            skelExpected =
              Just
                ( txSkelLbl
                    DupTokenLbl
                    ( [Mints (Nothing @()) [pol] (L.assetClassValue ac1 2)]
                        :=>: [ paysPK
                                 (walletPKHash (wallet 1))
                                 (L.assetClassValue ac1 1 <> L.assetClassValue ac2 2),
                               paysPK
                                 (walletPKHash attacker)
                                 (L.assetClassValue ac1 1)
                             ]
                    ),
                  L.assetClassValue ac1 1
                )
            skelOut =
              case dupTokenAttack (\_ i -> i + 1) attacker of
                Attack f -> f def skelIn
         in skelExpected @=? skelOut
    ]
