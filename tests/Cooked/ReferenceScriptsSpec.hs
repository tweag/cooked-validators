{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Cooked.ReferenceScriptsSpec where

import Control.Monad
import Cooked
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Optics.Core
import Plutus.Script.Utils.V2 qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V2 qualified as Api
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx
import Test.Tasty
import Test.Tasty.HUnit

-- | This validator ensures that the given public key signs the
-- transaction.
requireSignerValidator :: Api.PubKeyHash -> Script.TypedValidator ()
requireSignerValidator =
  Script.mkTypedValidatorParam
    $$(PlutusTx.compile [||val||])
    $$(PlutusTx.compile [||wrap||])
  where
    val :: Api.PubKeyHash -> () -> () -> Api.ScriptContext -> Bool
    val pkh _ _ (Api.ScriptContext txInfo _) =
      PlutusTx.traceIfFalse "the required signer is missing"
        PlutusTx.$ PlutusTx.elem pkh (Api.txInfoSignatories txInfo)

    wrap = Script.mkUntypedValidator

-- | This validator ensures that there is a transaction input that has
-- a reference script with the given hash.
requireRefScriptValidator :: Api.ScriptHash -> Script.TypedValidator ()
requireRefScriptValidator =
  Script.mkTypedValidatorParam
    $$(PlutusTx.compile [||val||])
    $$(PlutusTx.compile [||wrap||])
  where
    val :: Api.ScriptHash -> () -> () -> Api.ScriptContext -> Bool
    val expectedScriptHash _ _ (Api.ScriptContext txInfo _) =
      PlutusTx.traceIfFalse "there is no reference input with the correct script hash"
        PlutusTx.$ PlutusTx.any
          ( \(Api.TxInInfo _ (Api.TxOut _ _ _ mRefScriptHash)) ->
              Just expectedScriptHash PlutusTx.== mRefScriptHash
          )
          (Api.txInfoReferenceInputs txInfo)

    wrap = Script.mkUntypedValidator

putRefScriptOnWalletOutput ::
  (MonadBlockChain m) =>
  Wallet ->
  Script.Versioned Script.Validator ->
  m V3.TxOutRef
putRefScriptOnWalletOutput recipient referenceScript =
  head
    <$> validateTxSkel'
      txSkelTemplate
        { txSkelOuts = [recipient `receives` ReferenceScript referenceScript],
          txSkelSigners = [wallet 1]
        }

putRefScriptOnScriptOutput ::
  (MonadBlockChain m) =>
  Script.Versioned Script.Validator ->
  Script.Versioned Script.Validator ->
  m V3.TxOutRef
putRefScriptOnScriptOutput recipient referenceScript =
  head
    <$> validateTxSkel'
      txSkelTemplate
        { txSkelOuts = [recipient `receives` ReferenceScript referenceScript],
          txSkelSigners = [wallet 1]
        }

retrieveRefScriptHash :: (MonadBlockChain m) => V3.TxOutRef -> m (Maybe Api.ScriptHash)
retrieveRefScriptHash = (maybe Nothing (^. outputReferenceScriptL) <$>) . txOutByRef

checkReferenceScriptOnOref ::
  (MonadBlockChain m) =>
  Api.ScriptHash ->
  V3.TxOutRef ->
  m ()
checkReferenceScriptOnOref expectedScriptHash refScriptOref = do
  oref : _ <-
    validateTxSkel'
      txSkelTemplate
        { txSkelOuts = [requireRefScriptValidator expectedScriptHash `receives` Value (Script.ada 42)],
          txSkelSigners = [wallet 1]
        }
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelIns = Map.singleton oref emptyTxSkelRedeemer,
          txSkelInsReference = Set.singleton refScriptOref,
          txSkelSigners = [wallet 1]
        }

useReferenceScript :: (MonadBlockChain m) => Wallet -> Script.Versioned Script.Validator -> m ()
useReferenceScript spendingSubmitter theScript = do
  scriptOref <- putRefScriptOnWalletOutput (wallet 3) theScript
  oref : _ <-
    validateTxSkel'
      txSkelTemplate
        { txSkelOuts = [theScript `receives` Value (Script.ada 42)],
          txSkelSigners = [wallet 1]
        }
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelIns = Map.singleton oref $ emptyTxSkelRedeemer `withReferenceInput` scriptOref,
          txSkelSigners = [spendingSubmitter]
        }

referenceMint :: (MonadBlockChain m) => Script.Versioned Script.MintingPolicy -> Script.Versioned Script.MintingPolicy -> Int -> Bool -> m ()
referenceMint mp1 mp2 n autoRefScript = do
  ((!! n) -> mpOutRef) <-
    validateTxSkel' $
      txSkelTemplate
        { txSkelOuts =
            [ wallet 1 `receives` (Value (Script.ada 2) <&&> ReferenceScript mp1),
              wallet 1 `receives` Value (Script.ada 10)
            ],
          txSkelSigners = [wallet 1]
        }
  void $
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList
              [ mint mp2 (if autoRefScript then emptyTxSkelRedeemer else emptyTxSkelRedeemer `withReferenceInput` mpOutRef) (Api.TokenName "banana") 3
              ],
          txSkelOuts = [wallet 1 `receives` Value (Script.ada 2 <> Api.assetClassValue (Api.AssetClass (Script.toCurrencySymbol mp2, Api.TokenName "banana")) 3)],
          txSkelSigners = [wallet 1]
        }

tests :: TestTree
tests =
  testGroup
    "Reference scripts"
    [ testGroup "putting reference scripts on chain and retrieving them" $
        let theRefScript = Script.alwaysSucceedValidatorVersioned
            theRefScriptHash = Script.toScriptHash theRefScript
         in [ testCase "on a public key output" $
                testToProp $
                  mustSucceedTest
                    ( putRefScriptOnWalletOutput (wallet 3) theRefScript
                        >>= retrieveRefScriptHash
                    )
                    `withValuePred` ( testCounterexample "the script hash on the retrieved output is wrong"
                                        . (Just theRefScriptHash .==.)
                                    ),
              testCase "on a script output" $
                testToProp $
                  mustSucceedTest
                    ( putRefScriptOnScriptOutput Script.alwaysSucceedValidatorVersioned theRefScript
                        >>= retrieveRefScriptHash
                    )
                    `withValuePred` ( testCounterexample "the script hash on the retrieved output is wrong"
                                        . (Just theRefScriptHash .==.)
                                    ),
              testCase "retrieving the complete script from its hash" $
                testToProp $
                  mustSucceedTest
                    ( putRefScriptOnWalletOutput (wallet 3) theRefScript
                        >>= fmap fromJust . txOutByRef
                        >>= resolveReferenceScript
                    )
                    `withValuePred` maybe testFailure ((Just (Script.toVersioned theRefScript) .==.) . (^. outputReferenceScriptL))
            ],
      testGroup
        "checking the presence of reference scripts on the TxInfo"
        [ testCase "fail if wrong reference script"
            $ testFailsInPhase2WithMsg
              (== "there is no reference input with the correct script hash")
            $ putRefScriptOnWalletOutput (wallet 3) Script.alwaysFailValidatorVersioned
              >>= checkReferenceScriptOnOref (Script.toScriptHash Script.alwaysSucceedValidatorVersioned),
          testCase "succeed if correct reference script" $
            testSucceeds $
              putRefScriptOnWalletOutput (wallet 3) Script.alwaysSucceedValidatorVersioned
                >>= checkReferenceScriptOnOref (Script.toScriptHash Script.alwaysSucceedValidatorVersioned)
        ],
      testGroup
        "using reference scripts"
        [ testCase "fail from transaction generation for missing reference scripts" $
            testToProp $
              mustFailTest
                ( do
                    (consumedOref, _) : _ <-
                      runUtxoSearch $
                        utxosAtSearch (wallet 1)
                          `filterWithPred` ((`Api.geq` Script.lovelace 42_000_000) . outputValue)
                    oref : _ <-
                      validateTxSkel'
                        txSkelTemplate
                          { txSkelOuts = [Script.alwaysSucceedValidatorVersioned `receives` Value (Script.ada 42)],
                            txSkelIns = Map.singleton consumedOref emptyTxSkelRedeemer,
                            txSkelSigners = [wallet 1]
                          }
                    void $
                      validateTxSkel
                        txSkelTemplate
                          { txSkelIns = Map.singleton oref (emptyTxSkelRedeemer `withReferenceInput` consumedOref),
                            txSkelSigners = [wallet 1]
                          }
                )
                `withErrorPred` \case
                  MCEGenerationError err -> err .==. GenerateTxErrorGeneral "toPlutusScriptOrReferenceInput: Can't resolve reference script utxo."
                  _ -> testFailure,
          testCase "fail from transaction generation for mismatching reference scripts" $
            testToProp $
              mustFailTest
                ( do
                    scriptOref <- putRefScriptOnWalletOutput (wallet 3) Script.alwaysFailValidatorVersioned
                    oref : _ <-
                      validateTxSkel'
                        txSkelTemplate
                          { txSkelOuts = [Script.alwaysSucceedValidatorVersioned `receives` Value (Script.ada 42)],
                            txSkelSigners = [wallet 1]
                          }
                    void $
                      validateTxSkel
                        txSkelTemplate
                          { txSkelIns = Map.singleton oref (emptyTxSkelRedeemer `withReferenceInput` scriptOref),
                            txSkelSigners = [wallet 1]
                          }
                )
                `withErrorPred` \case
                  MCEGenerationError err -> err .==. GenerateTxErrorGeneral "toPlutusScriptOrReferenceInput: Wrong reference script hash."
                  _ -> testFailure,
          testCase "phase 1 - fail if using a reference script with 'someRedeemer'" $
            testFailsInPhase1 $ do
              scriptOref <- putRefScriptOnWalletOutput (wallet 3) Script.alwaysSucceedValidatorVersioned
              oref : _ <-
                validateTxSkel'
                  txSkelTemplate
                    { txSkelOuts = [Script.alwaysSucceedValidatorVersioned `receives` Value (Script.ada 42)],
                      txSkelSigners = [wallet 1]
                    }
              void $
                validateTxSkel
                  txSkelTemplate
                    { txSkelIns = Map.singleton oref emptyTxSkelRedeemerNoAutoFill,
                      txSkelInsReference = Set.singleton scriptOref,
                      txSkelSigners = [wallet 1]
                    },
          testCase
            "fail if reference script's requirement is violated"
            $ testFailsInPhase2WithMsg (== "the required signer is missing")
            $ useReferenceScript (wallet 1) (Script.toVersioned $ requireSignerValidator $ walletPKHash $ wallet 2),
          testCase "succeed if reference script's requirement is met" $
            testSucceeds $
              useReferenceScript (wallet 1) (Script.toVersioned $ requireSignerValidator $ walletPKHash $ wallet 1)
        ],
      testGroup
        "referencing minting policies"
        [ testCase "succeed if given a reference minting policy" $
            testSucceeds $
              referenceMint Script.alwaysSucceedPolicyVersioned Script.alwaysSucceedPolicyVersioned 0 False,
          testCase "succeed if relying on automated finding of reference minting policy" $
            testToProp $
              mustSucceedTest (referenceMint Script.alwaysSucceedPolicyVersioned Script.alwaysSucceedPolicyVersioned 0 True)
                `withJournalPred` (testBool . any (\case MCLogAddedReferenceScript {} -> True; _ -> False)),
          testCase "fail if given the wrong reference minting policy" $
            testToProp $
              mustFailTest (referenceMint Script.alwaysFailPolicyVersioned Script.alwaysSucceedPolicyVersioned 0 False)
                `withErrorPred` \case
                  MCEGenerationError (GenerateTxErrorGeneral err) -> err .==. "toPlutusScriptOrReferenceInput: Wrong reference script hash."
                  _ -> testFailure,
          testCase "fail if referencing the wrong utxo" $
            testToProp $
              mustFailTest (referenceMint Script.alwaysSucceedPolicyVersioned Script.alwaysSucceedPolicyVersioned 1 False)
                `withErrorPred` \case
                  MCEGenerationError (GenerateTxErrorGeneral err) -> err .==. "toPlutusScriptOrReferenceInput: No reference script found in utxo."
                  _ -> testFailure
        ]
    ]
