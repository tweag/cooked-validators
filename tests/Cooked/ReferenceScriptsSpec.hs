module Cooked.ReferenceScriptsSpec where

import Control.Monad
import Cooked
import Cooked.MockChain.GenerateTx
import Data.Default
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Ledger.Index qualified as Ledger
import Ledger.Index qualified as Pl
import Optics.Core
import Plutus.Script.Utils.Ada qualified as Script
import Plutus.Script.Utils.Typed qualified as Script
import Plutus.Script.Utils.V3.Typed.Scripts qualified as Script
import Plutus.Script.Utils.Value qualified as Script
import PlutusLedgerApi.V3 qualified as Api
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter qualified as PP
import Test.Tasty
import Test.Tasty.HUnit

-- | This validator ensures that the given public key signs the
-- transaction.
requireSignerValidator :: (Api.PubKeyHash, PlutusTx.BuiltinString) -> Script.TypedValidator MockContract
requireSignerValidator =
  Script.mkTypedValidatorParam @MockContract
    $$(PlutusTx.compile [||val||])
    $$(PlutusTx.compile [||wrap||])
  where
    val :: (Api.PubKeyHash, PlutusTx.BuiltinString) -> () -> () -> Api.ScriptContext -> Bool
    val (pkh, bs) _ _ (Api.ScriptContext txInfo _) =
      PlutusTx.traceIfFalse bs -- FIXME, it used to be statical, "the
      -- required signer is missing", see
      -- https://github.com/IntersectMBO/plutus/issues/5949
        PlutusTx.$ PlutusTx.elem pkh (Api.txInfoSignatories txInfo)

    wrap = Script.mkUntypedValidator

-- | This validator ensures that there is a transaction input that has
-- a reference script with the given hash.
requireRefScriptValidator :: (Api.ScriptHash, PlutusTx.BuiltinString) -> Script.TypedValidator MockContract
requireRefScriptValidator =
  Script.mkTypedValidatorParam @MockContract
    $$(PlutusTx.compile [||val||])
    $$(PlutusTx.compile [||wrap||])
  where
    val :: (Api.ScriptHash, PlutusTx.BuiltinString) -> () -> () -> Api.ScriptContext -> Bool
    val (expectedScriptHash, bs) _ _ (Api.ScriptContext txInfo _) =
      PlutusTx.traceIfFalse bs -- FIXME same as above "there is no reference
      -- input with the correct script hash"
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
putRefScriptOnWalletOutput recipient referencedScript =
  head
    <$> validateTxSkel'
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelOuts = [paysPK recipient (Script.Lovelace 1) `withReferenceScript` referencedScript],
          txSkelSigners = [wallet 1]
        }

putRefScriptOnScriptOutput ::
  (MonadBlockChain m) =>
  Script.TypedValidator MockContract ->
  Script.TypedValidator MockContract ->
  m Api.TxOutRef
putRefScriptOnScriptOutput recipient referencedScript =
  head
    <$> validateTxSkel'
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelOuts = [paysScript recipient () (Script.Lovelace 1) `withReferenceScript` referencedScript],
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
        { txSkelOuts = [paysScript (requireRefScriptValidator (expectedScriptHash, "there is no reference input with the correct script hash")) () (42 :: Integer)],
          txSkelSigners = [wallet 1]
        }
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelIns = Map.singleton oref $ TxSkelRedeemerForScript (),
          txSkelInsReference = Set.singleton refScriptOref,
          txSkelSigners = [wallet 1]
        }

useReferenceScript :: (MonadBlockChain m) => Wallet -> Script.TypedValidator MockContract -> m ()
useReferenceScript spendingSubmitter theScript = do
  scriptOref <- putRefScriptOnWalletOutput (wallet 3) theScript
  oref : _ <-
    validateTxSkel'
      txSkelTemplate
        { txSkelOuts = [paysScript theScript () (42 :: Integer)],
          txSkelSigners = [wallet 1]
        }
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelIns = Map.singleton oref $ TxSkelRedeemerForReferencedScript scriptOref (),
          txSkelSigners = [spendingSubmitter]
        }

tests :: TestTree
tests =
  testGroup
    "Reference scripts"
    [ testGroup "putting reference scripts on chain and retreiving them" $
        let theRefScript = alwaysFalseValidator
            theRefScriptHash = toScriptHash theRefScript
         in [ testCase "on a public key output"
                $ testSucceedsFrom'
                  def
                  ( \mScriptHash _ ->
                      testCounterexample "the script hash on the retrieved output is wrong" $
                        Just theRefScriptHash .==. mScriptHash
                  )
                  def
                $ putRefScriptOnWalletOutput (wallet 3) theRefScript
                  >>= retrieveRefScriptHash,
              testCase "on a script output"
                $ testSucceedsFrom'
                  def
                  ( \mScriptHash _ ->
                      testCounterexample "the script hash on the retrieved output is wrong" $
                        Just theRefScriptHash .==. mScriptHash
                  )
                  def
                $ putRefScriptOnScriptOutput alwaysTrueValidator theRefScript
                  >>= retrieveRefScriptHash,
              testCase "retrieving the complete script from its hash"
                $ testSucceedsFrom'
                  def
                  ( \mOut _ -> case mOut of
                      Nothing -> testFailure
                      Just out -> Just (Script.vValidatorScript theRefScript) .==. out ^. outputReferenceScriptL
                  )
                  def
                $ putRefScriptOnWalletOutput (wallet 3) theRefScript
                  >>= fmap fromJust . txOutByRef
                  >>= resolveReferenceScript
            ],
      testGroup
        "checking the presence of reference scripts on the TxInfo"
        [ testCase "fail if wrong reference script"
            $ testFails
              def
              ( isCekEvaluationFailureWithMsg
                  def
                  (== "there is no reference input with the correct script hash")
              )
            $ putRefScriptOnWalletOutput (wallet 3) alwaysFalseValidator
              >>= checkReferenceScriptOnOref (toScriptHash alwaysTrueValidator),
          testCase "succeed if correct reference script" $
            testSucceeds def $
              putRefScriptOnWalletOutput (wallet 3) alwaysTrueValidator
                >>= checkReferenceScriptOnOref (toScriptHash alwaysTrueValidator)
        ],
      testGroup
        "using reference scripts"
        [ testCase "fail from transaction generation for missing reference scripts"
            $ testFailsFrom
              def
              ( \case
                  MCEUnknownOutRefError "lookupUtxos: unknown TxOutRef" _ -> testSuccess
                  MCECalcFee (MCEUnknownOutRefError "lookupUtxos: unknown TxOutRef" _) -> testSuccess
                  _ -> testFailure
              )
              def
            $ do
              (consumedOref, _) : _ <- runUtxoSearch $ utxosAtSearch (wallet 1) *+* pureBoolFilter ((`Script.geq` toValue (42 :: Integer)) . outputValue)
              oref : _ <-
                validateTxSkel'
                  txSkelTemplate
                    { txSkelOuts = [paysScript (alwaysTrueValidator @MockContract) () (42 :: Integer)],
                      txSkelIns = Map.singleton consumedOref TxSkelNoRedeemerForPK,
                      txSkelSigners = [wallet 1]
                    }
              void $
                validateTxSkel
                  txSkelTemplate
                    { txSkelIns = Map.singleton oref (TxSkelRedeemerForReferencedScript consumedOref ()),
                      txSkelSigners = [wallet 1]
                    },
          testCase "fail from transaction generation for mismatching reference scripts" $
            let expectedError = GenerateTxErrorGeneral "txSkelInToTxIn: Wrong reference script hash. Are you using the correct TxOutRef on your TxSkelRedeemerForReferencedScript?"
             in testFailsFrom
                  def
                  ( \case
                      MCEGenerationError err -> err .==. expectedError
                      MCECalcFee (MCEGenerationError err) -> err .==. expectedError
                      _ -> testFailure
                  )
                  def
                  $ do
                    scriptOref <- putRefScriptOnWalletOutput (wallet 3) alwaysFalseValidator
                    oref : _ <-
                      validateTxSkel'
                        txSkelTemplate
                          { txSkelOuts = [paysScript (alwaysTrueValidator @MockContract) () (42 :: Integer)],
                            txSkelSigners = [wallet 1]
                          }
                    void $
                      validateTxSkel
                        txSkelTemplate
                          { txSkelIns = Map.singleton oref (TxSkelRedeemerForReferencedScript scriptOref ()),
                            txSkelSigners = [wallet 1]
                          },
          testCase "phase 1 - fail if using a reference script with 'TxSkelRedeemerForScript'"
            $ testFailsFrom
              def
              ( \case
                  MCEValidationError Ledger.Phase1 _ -> testSuccess
                  MCECalcFee (MCEValidationError Ledger.Phase1 _) -> testSuccess
                  _ -> testFailure
              )
              def
            $ do
              scriptOref <- putRefScriptOnWalletOutput (wallet 3) alwaysTrueValidator
              oref : _ <-
                validateTxSkel'
                  txSkelTemplate
                    { txSkelOuts = [paysScript (alwaysTrueValidator @MockContract) () (42 :: Integer)],
                      txSkelSigners = [wallet 1]
                    }
              void $
                validateTxSkel
                  txSkelTemplate
                    { txSkelIns = Map.singleton oref (TxSkelRedeemerForScript ()),
                      txSkelInsReference = Set.singleton scriptOref,
                      txSkelSigners = [wallet 1]
                    },
          testCase
            "fail if referenced script's requirement is violated"
            $ testFailsFrom
              (def {pcOptPrintTxHashes = True})
              ( isCekEvaluationFailureWithMsg
                  def
                  (== "the required signer is missing")
              )
              def
            $ useReferenceScript (wallet 1) (requireSignerValidator (walletPKHash $ wallet 2, "the required signer is missing")),
          testCase "succeed if referenced script's requirement is met" $
            testSucceeds def $
              useReferenceScript (wallet 1) (requireSignerValidator (walletPKHash $ wallet 1, "the required signer is missing"))
        ]
    ]
