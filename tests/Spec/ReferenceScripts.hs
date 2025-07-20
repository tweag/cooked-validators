module Spec.ReferenceScripts where

import Cardano.Api.Internal.Tx.Body qualified as Cardano
import Cardano.Api.Internal.Tx.Sign qualified as Cardano
import Cooked
import Data.Map qualified as Map
import Data.Set qualified as Set
import Ledger.Tx qualified as Ledger
import Optics.Core
import Plutus.ReferenceScripts
import Plutus.Script.Utils.V2 qualified as Script
import PlutusLedgerApi.V1.Value qualified as Api
import PlutusLedgerApi.V2 qualified as Api
import PlutusLedgerApi.V3 qualified as V3
import Test.Tasty

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
retrieveRefScriptHash = previewByRef (txSkelOutReferenceScriptL % txSkelOutReferenceScriptHashAF)

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
  validateTxSkel_
    txSkelTemplate
      { txSkelIns = Map.singleton oref emptyTxSkelRedeemer,
        txSkelInsReference = Set.singleton refScriptOref,
        txSkelSigners = [wallet 1]
      }

-- The boolean states whether or not the input hosting the reference script
-- should be consumed in the transaction or not. If it should, then at
-- transaction generation no reference input should appear, as inputs also act
-- as reference inputs.
useReferenceScript :: (MonadBlockChain m) => Wallet -> Bool -> Script.Versioned Script.Validator -> m Ledger.CardanoTx
useReferenceScript spendingSubmitter consumeScriptOref theScript = do
  scriptOref <- putRefScriptOnWalletOutput (wallet 3) theScript
  oref : _ <-
    validateTxSkel'
      txSkelTemplate
        { txSkelOuts = [theScript `receives` Value (Script.ada 42)],
          txSkelSigners = [wallet 1]
        }
  validateTxSkel
    txSkelTemplate
      { txSkelIns =
          Map.fromList $
            (oref, emptyTxSkelRedeemer `withReferenceInput` scriptOref)
              : [(scriptOref, emptyTxSkelRedeemer) | consumeScriptOref],
        txSkelSigners = spendingSubmitter : [wallet 3 | consumeScriptOref]
      }

useReferenceScriptInInputs :: (MonadBlockChain m) => Wallet -> Script.Versioned Script.Validator -> m ()
useReferenceScriptInInputs spendingSubmitter theScript = do
  scriptOref <- putRefScriptOnWalletOutput (wallet 1) theScript
  oref : _ <-
    validateTxSkel'
      txSkelTemplate
        { txSkelOuts = [theScript `receives` Value (Script.ada 42)],
          txSkelSigners = [wallet 1]
        }
  validateTxSkel_
    txSkelTemplate
      { txSkelIns = Map.fromList [(oref, emptyTxSkelRedeemer `withReferenceInput` scriptOref), (scriptOref, emptyTxSkelRedeemer)],
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
  validateTxSkel_ $
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
         in [ testCooked "on a public key output" $
                mustSucceedTest
                  (putRefScriptOnWalletOutput (wallet 3) theRefScript >>= retrieveRefScriptHash)
                  `withResultProp` (testCounterexample "the script hash on the retrieved output is wrong" . (Just theRefScriptHash .==.)),
              testCooked "on a script output" $
                mustSucceedTest
                  (putRefScriptOnScriptOutput Script.alwaysSucceedValidatorVersioned theRefScript >>= retrieveRefScriptHash)
                  `withResultProp` (testCounterexample "the script hash on the retrieved output is wrong" . (Just theRefScriptHash .==.))
            ],
      testGroup
        "checking the presence of reference scripts on the TxInfo"
        [ testCooked "fail if wrong reference script" $
            mustFailInPhase2WithMsgTest "there is no reference input with the correct script hash" $
              putRefScriptOnWalletOutput (wallet 3) Script.alwaysFailValidatorVersioned
                >>= checkReferenceScriptOnOref (Script.toScriptHash Script.alwaysSucceedValidatorVersioned),
          testCooked "succeed if correct reference script" $
            mustSucceedTest $
              putRefScriptOnWalletOutput (wallet 3) Script.alwaysSucceedValidatorVersioned
                >>= checkReferenceScriptOnOref (Script.toScriptHash Script.alwaysSucceedValidatorVersioned)
        ],
      testGroup
        "using reference scripts"
        [ testCooked "fail from transaction generation for missing reference scripts" $
            mustFailTest
              ( do
                  (consumedOref, _) : _ <-
                    runUtxoSearch $
                      utxosOwnedBySearch (wallet 1)
                        `filterWithValuePred` (`Api.geq` Script.lovelace 42_000_000)
                  oref : _ <-
                    validateTxSkel'
                      txSkelTemplate
                        { txSkelOuts = [Script.alwaysSucceedValidatorVersioned `receives` Value (Script.ada 42)],
                          txSkelIns = Map.singleton consumedOref emptyTxSkelRedeemer,
                          txSkelSigners = [wallet 1]
                        }
                  validateTxSkel_
                    txSkelTemplate
                      { txSkelIns = Map.singleton oref (emptyTxSkelRedeemer `withReferenceInput` consumedOref),
                        txSkelSigners = [wallet 1]
                      }
              )
              `withErrorProp` \case
                MCEUnknownOutRef _ -> testSuccess
                _ -> testFailure,
          testCooked "fail from transaction generation for mismatching reference scripts" $
            mustFailTest
              ( do
                  scriptOref <- putRefScriptOnWalletOutput (wallet 3) Script.alwaysFailValidatorVersioned
                  oref : _ <-
                    validateTxSkel'
                      txSkelTemplate
                        { txSkelOuts = [Script.alwaysSucceedValidatorVersioned `receives` Value (Script.ada 42)],
                          txSkelSigners = [wallet 1]
                        }
                  validateTxSkel_
                    txSkelTemplate
                      { txSkelIns = Map.singleton oref (emptyTxSkelRedeemer `withReferenceInput` scriptOref),
                        txSkelSigners = [wallet 1]
                      }
              )
              `withErrorProp` \case
                MCEWrongReferenceScriptError {} -> testSuccess
                _ -> testFailure,
          testCooked "phase 1 - fail if using a reference script with 'someRedeemer'" $
            mustFailInPhase1Test $ do
              scriptOref <- putRefScriptOnWalletOutput (wallet 3) Script.alwaysSucceedValidatorVersioned
              oref : _ <-
                validateTxSkel'
                  txSkelTemplate
                    { txSkelOuts = [Script.alwaysSucceedValidatorVersioned `receives` Value (Script.ada 42)],
                      txSkelSigners = [wallet 1]
                    }
              validateTxSkel_
                txSkelTemplate
                  { txSkelIns = Map.singleton oref emptyTxSkelRedeemerNoAutoFill,
                    txSkelInsReference = Set.singleton scriptOref,
                    txSkelSigners = [wallet 1]
                  },
          testCooked "fail if reference script's requirement is violated" $
            mustFailInPhase2WithMsgTest "the required signer is missing" $
              useReferenceScript (wallet 1) False $
                Script.toVersioned $
                  requireSignerValidator $
                    Script.toPubKeyHash $
                      wallet 2,
          testCooked "succeed if reference script's requirement is met" $
            mustSucceedTest
              ( useReferenceScript (wallet 1) False $
                  Script.toVersioned $
                    requireSignerValidator $
                      Script.toPubKeyHash $
                        wallet 1
              )
              `withSuccessProp` \_ _ (Ledger.CardanoEmulatorEraTx (Cardano.Tx (Cardano.getTxBodyContent -> bodyContent) _)) _ ->
                case Cardano.txInsReference bodyContent of
                  Cardano.TxInsReference _ [_] _ -> testSuccess
                  _ -> testFailure,
          testCooked "succeed if the reference script is in one of the inputs" $
            mustSucceedTest
              ( useReferenceScript (wallet 1) True $
                  Script.toVersioned $
                    requireSignerValidator $
                      Script.toPubKeyHash $
                        wallet 1
              )
              `withSuccessProp` \_ _ (Ledger.CardanoEmulatorEraTx (Cardano.Tx (Cardano.getTxBodyContent -> bodyContent) _)) _ ->
                case Cardano.txInsReference bodyContent of
                  -- We get this from cardano-api instead of TxInsReferenceNone
                  Cardano.TxInsReference _ [] _ -> testSuccess
                  _ -> testFailure
        ],
      testGroup
        "referencing minting policies"
        [ testCooked "succeed if given a reference minting policy" $
            mustSucceedTest $
              referenceMint Script.alwaysSucceedPolicyVersioned Script.alwaysSucceedPolicyVersioned 0 False,
          testCooked "succeed if relying on automated finding of reference minting policy" $
            mustSucceedTest (referenceMint Script.alwaysSucceedPolicyVersioned Script.alwaysSucceedPolicyVersioned 0 True)
              `withJournalProp` happened "MCLogAddedReferenceScript",
          testCooked "fail if given the wrong reference minting policy" $
            mustFailTest (referenceMint Script.alwaysFailPolicyVersioned Script.alwaysSucceedPolicyVersioned 0 False)
              `withErrorProp` \case
                MCEWrongReferenceScriptError {} -> testSuccess
                _ -> testFailure,
          testCooked "fail if referencing the wrong utxo" $
            mustFailTest (referenceMint Script.alwaysSucceedPolicyVersioned Script.alwaysSucceedPolicyVersioned 1 False)
              `withErrorProp` \case
                MCEWrongReferenceScriptError {} -> testSuccess
                _ -> testFailure
        ]
    ]
