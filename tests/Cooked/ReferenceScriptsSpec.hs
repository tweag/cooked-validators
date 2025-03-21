module Cooked.ReferenceScriptsSpec where

import Control.Monad
import Cooked
import Data.Default
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Optics.Core
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Typed qualified as Script
import Plutus.Script.Utils.V3.Typed.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx
import Test.Tasty
import Test.Tasty.HUnit

-- | This validator ensures that the given public key signs the
-- transaction.
requireSignerValidator :: Api.PubKeyHash -> Script.TypedValidator MockContract
requireSignerValidator =
  Script.mkTypedValidatorParam @MockContract
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
requireRefScriptValidator :: Api.ScriptHash -> Script.TypedValidator MockContract
requireRefScriptValidator =
  Script.mkTypedValidatorParam @MockContract
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
  Script.TypedValidator MockContract ->
  m Api.TxOutRef
putRefScriptOnWalletOutput recipient referenceScript =
  head
    <$> validateTxSkel'
      txSkelTemplate
        { txSkelOuts = [recipient `receives` ReferenceScript referenceScript],
          txSkelSigners = [wallet 1]
        }

putRefScriptOnScriptOutput ::
  (MonadBlockChain m) =>
  Script.TypedValidator MockContract ->
  Script.TypedValidator MockContract ->
  m Api.TxOutRef
putRefScriptOnScriptOutput recipient referenceScript =
  head
    <$> validateTxSkel'
      txSkelTemplate
        { txSkelOuts = [recipient `receives` (ReferenceScript referenceScript <&&> VisibleHashedDatum ())],
          txSkelSigners = [wallet 1]
        }

retrieveRefScriptHash :: (MonadBlockChain m) => Api.TxOutRef -> m (Maybe Api.ScriptHash)
retrieveRefScriptHash = (maybe Nothing (^. outputReferenceScriptL) <$>) . txOutByRef

checkReferenceScriptOnOref ::
  (MonadBlockChain m) =>
  Api.ScriptHash ->
  Api.TxOutRef ->
  m ()
checkReferenceScriptOnOref expectedScriptHash refScriptOref = do
  oref : _ <-
    validateTxSkel'
      txSkelTemplate
        { txSkelOuts = [requireRefScriptValidator expectedScriptHash `receives` (Value (Script.ada 42) <&&> VisibleHashedDatum ())],
          txSkelSigners = [wallet 1]
        }
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelIns = Map.singleton oref $ someTxSkelRedeemer (),
          txSkelInsReference = Set.singleton refScriptOref,
          txSkelSigners = [wallet 1]
        }

useReferenceScript :: (MonadBlockChain m) => Wallet -> Script.TypedValidator MockContract -> m ()
useReferenceScript spendingSubmitter theScript = do
  scriptOref <- putRefScriptOnWalletOutput (wallet 3) theScript
  oref : _ <-
    validateTxSkel'
      txSkelTemplate
        { txSkelOuts = [theScript `receives` (Value (Script.ada 42) <&&> VisibleHashedDatum ())],
          txSkelSigners = [wallet 1]
        }
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelIns = Map.singleton oref $ someTxSkelRedeemer () `withReferenceInput` scriptOref,
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
        { txSkelMints = txSkelMintsFromList [(mp2, if autoRefScript then emptyTxSkelRedeemer else emptyTxSkelRedeemer `withReferenceInput` mpOutRef, "banana", 3)],
          txSkelOuts = [wallet 1 `receives` Value (Script.ada 2 <> Script.assetClassValue (Script.AssetClass (Script.scriptCurrencySymbol mp2, "banana")) 3)],
          txSkelSigners = [wallet 1],
          txSkelOpts = def {txOptAutoReferenceScripts = autoRefScript}
        }

tests :: TestTree
tests =
  testGroup
    "Reference scripts"
    [ testGroup "putting reference scripts on chain and retrieving them" $
        let theRefScript = alwaysFalseValidator
            theRefScriptHash = toScriptHash theRefScript
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
                    ( putRefScriptOnScriptOutput alwaysTrueValidator theRefScript
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
                    `withValuePred` maybe testFailure ((Just (Script.vValidatorScript theRefScript) .==.) . (^. outputReferenceScriptL))
            ],
      testGroup
        "checking the presence of reference scripts on the TxInfo"
        [ testCase "fail if wrong reference script"
            $ testFailsInPhase2WithMsg
              (== "there is no reference input with the correct script hash")
            $ putRefScriptOnWalletOutput (wallet 3) alwaysFalseValidator
              >>= checkReferenceScriptOnOref (toScriptHash alwaysTrueValidator),
          testCase "succeed if correct reference script" $
            testSucceeds $
              putRefScriptOnWalletOutput (wallet 3) alwaysTrueValidator
                >>= checkReferenceScriptOnOref (toScriptHash alwaysTrueValidator)
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
                          `filterWithPred` ((`Script.geq` Script.lovelaceValueOf 42_000_000) . outputValue)
                    oref : _ <-
                      validateTxSkel'
                        txSkelTemplate
                          { txSkelOuts = [alwaysTrueValidator @MockContract `receives` (Value (Script.ada 42) <&&> VisibleHashedDatum ())],
                            txSkelIns = Map.singleton consumedOref emptyTxSkelRedeemer,
                            txSkelSigners = [wallet 1]
                          }
                    void $
                      validateTxSkel
                        txSkelTemplate
                          { txSkelIns = Map.singleton oref (someTxSkelRedeemer () `withReferenceInput` consumedOref),
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
                    scriptOref <- putRefScriptOnWalletOutput (wallet 3) alwaysFalseValidator
                    oref : _ <-
                      validateTxSkel'
                        txSkelTemplate
                          { txSkelOuts = [alwaysTrueValidator @MockContract `receives` (Value (Script.ada 42) <&&> VisibleHashedDatum ())],
                            txSkelSigners = [wallet 1]
                          }
                    void $
                      validateTxSkel
                        txSkelTemplate
                          { txSkelIns = Map.singleton oref (someTxSkelRedeemer () `withReferenceInput` scriptOref),
                            txSkelSigners = [wallet 1]
                          }
                )
                `withErrorPred` \case
                  MCEGenerationError err -> err .==. GenerateTxErrorGeneral "toPlutusScriptOrReferenceInput: Wrong reference script hash."
                  _ -> testFailure,
          testCase "phase 1 - fail if using a reference script with 'someRedeemer'" $
            testFailsInPhase1 $ do
              scriptOref <- putRefScriptOnWalletOutput (wallet 3) alwaysTrueValidator
              oref : _ <-
                validateTxSkel'
                  txSkelTemplate
                    { txSkelOuts = [alwaysTrueValidator @MockContract `receives` (Value (Script.ada 42) <&&> VisibleHashedDatum ())],
                      txSkelSigners = [wallet 1]
                    }
              void $
                validateTxSkel
                  txSkelTemplate
                    { txSkelIns = Map.singleton oref (someTxSkelRedeemer ()),
                      txSkelInsReference = Set.singleton scriptOref,
                      txSkelSigners = [wallet 1],
                      txSkelOpts = def {txOptAutoReferenceScripts = False}
                    },
          testCase
            "fail if reference script's requirement is violated"
            $ testFailsInPhase2WithMsg (== "the required signer is missing")
            $ useReferenceScript (wallet 1) (requireSignerValidator $ walletPKHash $ wallet 2),
          testCase "succeed if reference script's requirement is met" $
            testSucceeds $
              useReferenceScript (wallet 1) (requireSignerValidator $ walletPKHash $ wallet 1)
        ],
      testGroup
        "referencing minting policies"
        [ testCase "succeed if given a reference minting policy" $
            testSucceeds $
              referenceMint quickCurrencyPolicyV3 quickCurrencyPolicyV3 0 False,
          testCase "succeed if relying on automated finding of reference minting policy" $
            testToProp $
              mustSucceedTest (referenceMint quickCurrencyPolicyV3 quickCurrencyPolicyV3 0 True)
                `withJournalPred` (testBool . any (\case MCLogAddedReferenceScript {} -> True; _ -> False)),
          testCase "fail if given the wrong reference minting policy" $
            testToProp $
              mustFailTest (referenceMint permanentCurrencyPolicyV3 quickCurrencyPolicyV3 0 False)
                `withErrorPred` \case
                  MCEGenerationError (GenerateTxErrorGeneral err) -> err .==. "toPlutusScriptOrReferenceInput: Wrong reference script hash."
                  _ -> testFailure,
          testCase "fail if referencing the wrong utxo" $
            testToProp $
              mustFailTest (referenceMint quickCurrencyPolicyV3 quickCurrencyPolicyV3 1 False)
                `withErrorPred` \case
                  MCEGenerationError (GenerateTxErrorGeneral err) -> err .==. "toPlutusScriptOrReferenceInput: No reference script found in utxo."
                  _ -> testFailure
        ]
    ]
