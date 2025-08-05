{-# OPTIONS_GHC -Wno-orphans #-}

module Spec.MultiPurpose where

import Cooked
import Data.Map qualified as HMap
import Optics.Core
import Plutus.MultiPurpose
import Plutus.Script.Utils.V3 qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V3 qualified as Api
import Prettyprinter qualified as PP
import Test.Tasty

instance PrettyCooked MintingRed where
  prettyCooked = PP.viaShow

instance PrettyCooked SpendingRed where
  prettyCooked = PP.viaShow

alice, bob :: Wallet
alice = wallet 1
bob = wallet 2

runScript :: (MonadModalBlockChain m) => m ()
runScript = do
  [oRef@(Api.TxOutRef txId _), oRef', oRef''] <-
    validateTxSkel' $
      txSkelTemplate
        { txSkelOuts =
            [ alice `receives` Value (Script.ada 3),
              alice `receives` Value (Script.ada 5)
            ],
          txSkelSigners = [bob]
        }

  script <- define "My multipurpose script" $ mpScript txId
  let (mintSkel1, _, tn1) = mkMintSkel alice oRef script
      (mintSkel2, mintValue2, tn2) = mkMintSkel alice oRef' script
      (mintSkel3, mintValue3, tn3) = mkMintSkel bob oRef'' script

  (oRefScript : _) <- validateTxSkel' mintSkel1
  (oRefScript1 : _) <- validateTxSkel' mintSkel2
  (oRefScript2 : _) <- validateTxSkel' mintSkel3

  (oRefScript1' : oRefScript2' : _) <-
    validateTxSkel' $
      txSkelTemplate
        { txSkelSigners = [alice],
          txSkelIns =
            HMap.fromList
              [ (oRefScript, someTxSkelRedeemer Close),
                (oRefScript1, someTxSkelRedeemer Step),
                (oRefScript2, someTxSkelRedeemer Step)
              ],
          txSkelOuts =
            [ script `receives` (InlineDatum (0 :: Integer) <&&> Value mintValue2),
              script `receives` (InlineDatum (1 :: Integer) <&&> Value mintValue3)
            ],
          txSkelMints = review txSkelMintsListI [burn script BurnToken tn1 1]
        }

  (oRefScript2'' : _) <-
    validateTxSkel' $
      txSkelTemplate
        { txSkelSigners = [bob],
          txSkelIns =
            HMap.fromList
              [ (oRefScript1', someTxSkelRedeemer Close),
                (oRefScript2', someTxSkelRedeemer Step)
              ],
          txSkelOuts =
            [ script `receives` (InlineDatum (0 :: Integer) <&&> Value mintValue3)
            ],
          txSkelMints = review txSkelMintsListI [burn script BurnToken tn2 1]
        }

  validateTxSkel_ $
    txSkelTemplate
      { txSkelSigners = [alice],
        txSkelIns = HMap.singleton oRefScript2'' (someTxSkelRedeemer Close),
        txSkelMints = review txSkelMintsListI [burn script BurnToken tn3 1]
      }
  where
    mkMintSkel :: Wallet -> Api.TxOutRef -> Script.MultiPurposeScript MPTag -> (TxSkel, Api.Value, Api.TokenName)
    mkMintSkel signer oRef@(Api.TxOutRef _ index) script =
      let tn = txOutRefToToken oRef
          mints = review txSkelMintsListI [mint script (MintToken oRef) tn 1]
          mintValue = Script.toValue mints
       in ( txSkelTemplate
              { txSkelIns = HMap.singleton oRef emptyTxSkelRedeemer,
                txSkelMints = mints,
                txSkelOuts = [script `receives` (InlineDatum index <&&> Value mintValue)],
                txSkelSigners = [signer]
              },
            mintValue,
            tn
          )

tests :: TestTree
tests =
  testGroup
    "Multi purpose scripts"
    [ testCooked "Using a script as minting and spending in the same scenario" $ mustSucceedTest runScript,
      testGroup
        "The Spending purpose behaves properly"
        [ testCooked "We cannot redirect any output to a private key" $
            mustFailWithSizeTest 6 $
              somewhere (datumHijackingAttack $ scriptsDatumHijackingParams alice) runScript,
          testCooked "We cannot redirect any output to another script" $
            mustFailWithSizeTest 6 $
              somewhere (datumHijackingAttack $ scriptsDatumHijackingParams $ Script.trueSpendingMPScript @()) runScript
        ],
      testGroup
        "The Minting purpose behaves properly"
        [ testCooked "We cannot duplicate the tokens" $
            mustFailWithSizeTest 6 $
              somewhere (dupTokenAttack (\_ _ n -> n + 1) alice) runScript,
          testCooked "We cannot mint additional tokens" $
            mustFailWithSizeTest 6 $
              somewhere (addTokenAttack (const [(Api.TokenName "myToken", 1)]) alice) runScript
        ]
    ]
