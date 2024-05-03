module Cooked.ReferenceScriptsSpec where

import Control.Monad
import Cooked
import Cooked.MockChain.GenerateTx
import Data.Default
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Ledger.Index qualified as Pl
import Optics.Core
import Plutus.Script.Utils.Ada qualified as Pl
import Plutus.Script.Utils.Typed qualified as Pl
import Plutus.Script.Utils.V3.Typed.Scripts qualified as Pl
import Plutus.Script.Utils.Value qualified as Value
import PlutusLedgerApi.V3 qualified as Pl
import PlutusTx qualified as Pl
import PlutusTx.Prelude qualified as Pl
import Prettyprinter qualified as PP
import Test.Tasty
import Test.Tasty.HUnit

-- | This validator ensures that the given public key signs the transaction.
requireSignerValidator :: (Pl.PubKeyHash, Pl.BuiltinString) -> Pl.TypedValidator MockContract
requireSignerValidator =
  Pl.mkTypedValidatorParam @MockContract
    $$(Pl.compile [||val||])
    $$(Pl.compile [||wrap||])
  where
    val :: (Pl.PubKeyHash, Pl.BuiltinString) -> () -> () -> Pl.ScriptContext -> Bool
    val (pkh, bs) _ _ (Pl.ScriptContext txInfo _) =
      Pl.traceIfFalse bs -- FIXME, it used to be statical, "the
      -- required signer is missing", see
      -- https://github.com/IntersectMBO/plutus/issues/5949
        Pl.$ Pl.elem pkh (Pl.txInfoSignatories txInfo)

    wrap = Pl.mkUntypedValidator

-- | This validator ensures that there is a transaction input that has a
-- reference script with the given hash.
requireRefScriptValidator :: (Pl.ScriptHash, Pl.BuiltinString) -> Pl.TypedValidator MockContract
requireRefScriptValidator =
  Pl.mkTypedValidatorParam @MockContract
    $$(Pl.compile [||val||])
    $$(Pl.compile [||wrap||])
  where
    val :: (Pl.ScriptHash, Pl.BuiltinString) -> () -> () -> Pl.ScriptContext -> Bool
    val (expectedScriptHash, bs) _ _ (Pl.ScriptContext txInfo _) =
      Pl.traceIfFalse bs -- FIXME same as above "there is no reference
      -- input with the correct script hash"
        Pl.$ Pl.any
          ( \(Pl.TxInInfo _ (Pl.TxOut _ _ _ mRefScriptHash)) ->
              Just expectedScriptHash Pl.== mRefScriptHash
          )
          (Pl.txInfoReferenceInputs txInfo)

    wrap = Pl.mkUntypedValidator

putRefScriptOnWalletOutput ::
  (MonadBlockChain m) =>
  Wallet ->
  Pl.TypedValidator MockContract ->
  m Pl.TxOutRef
putRefScriptOnWalletOutput recipient referencedScript =
  fst . head . utxosFromCardanoTx
    <$> validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelOuts =
            [ paysPK
                (walletPKHash recipient)
                (Pl.lovelaceValueOf 1)
                `withReferenceScript` referencedScript
            ],
          txSkelSigners = [wallet 1]
        }

putRefScriptOnScriptOutput ::
  (MonadBlockChain m) =>
  Pl.TypedValidator MockContract ->
  Pl.TypedValidator MockContract ->
  m Pl.TxOutRef
putRefScriptOnScriptOutput recipient referencedScript =
  fst . head . utxosFromCardanoTx
    <$> validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelOuts =
            [ paysScript
                recipient
                ()
                (Pl.lovelaceValueOf 1)
                `withReferenceScript` referencedScript
            ],
          txSkelSigners = [wallet 1]
        }

retrieveRefScriptHash :: (MonadBlockChain m) => Pl.TxOutRef -> m (Maybe Pl.ScriptHash)
retrieveRefScriptHash = (maybe Nothing (^. outputReferenceScriptL) <$>) . txOutByRef

checkReferenceScriptOnOref ::
  (MonadBlockChain m) =>
  Pl.ScriptHash ->
  Pl.TxOutRef ->
  m ()
checkReferenceScriptOnOref expectedScriptHash refScriptOref = do
  (oref, _) : _ <-
    utxosFromCardanoTx
      <$> validateTxSkel
        txSkelTemplate
          { txSkelOuts =
              [ paysScript
                  (requireRefScriptValidator (expectedScriptHash, "there is no reference input with the correct script hash"))
                  ()
                  (Pl.lovelaceValueOf 42_000_000)
              ],
            txSkelSigners = [wallet 1]
          }
  void $
    validateTxSkel
      txSkelTemplate
        { txSkelIns = Map.singleton oref $ TxSkelRedeemerForScript (),
          txSkelInsReference = Set.singleton refScriptOref,
          txSkelSigners = [wallet 1]
        }

useReferenceScript :: (MonadBlockChain m) => Wallet -> Pl.TypedValidator MockContract -> m ()
useReferenceScript spendingSubmitter theScript = do
  scriptOref <- putRefScriptOnWalletOutput (wallet 3) theScript
  (oref, _) : _ <-
    utxosFromCardanoTx
      <$> validateTxSkel
        txSkelTemplate
          { txSkelOuts =
              [ paysScript
                  theScript
                  ()
                  (Pl.lovelaceValueOf 42_000_000)
              ],
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
                      Just out -> Just (Pl.vValidatorScript theRefScript) .==. out ^. outputReferenceScriptL
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
              (consumedOref, _) : _ <-
                runUtxoSearch $
                  utxosAtSearch (walletAddress $ wallet 1)
                    `filterWithPred` ((`Value.geq` Pl.lovelaceValueOf 42_000_000) . outputValue)
              (oref, _) : _ <-
                utxosFromCardanoTx
                  <$> validateTxSkel
                    txSkelTemplate
                      { txSkelOuts =
                          [ paysScript
                              (alwaysTrueValidator @MockContract)
                              ()
                              (Pl.lovelaceValueOf 42_000_000)
                          ],
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
                    (oref, _) : _ <-
                      utxosFromCardanoTx
                        <$> validateTxSkel
                          txSkelTemplate
                            { txSkelOuts =
                                [ paysScript
                                    (alwaysTrueValidator @MockContract)
                                    ()
                                    (Pl.lovelaceValueOf 42_000_000)
                                ],
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
                  MCEValidationError Pl.Phase1 _ -> testSuccess
                  MCECalcFee (MCEValidationError Pl.Phase1 _) -> testSuccess
                  _ -> testFailure
              )
              def
            $ do
              scriptOref <- putRefScriptOnWalletOutput (wallet 3) alwaysTrueValidator
              (oref, _) : _ <-
                utxosFromCardanoTx
                  <$> validateTxSkel
                    txSkelTemplate
                      { txSkelOuts =
                          [ paysScript
                              (alwaysTrueValidator @MockContract)
                              ()
                              (Pl.lovelaceValueOf 42_000_000)
                          ],
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
