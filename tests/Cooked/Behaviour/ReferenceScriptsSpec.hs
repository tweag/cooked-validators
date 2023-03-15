{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.Behaviour.ReferenceScriptsSpec where

import Control.Monad
import Cooked
import qualified Cooked.Behaviour.Validators as Validators
import Data.Default
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Ledger.Index as Pl
import Optics.Core
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Plutus.Script.Utils.Typed as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import Test.Tasty
import Test.Tasty.HUnit

putRefScriptOnWalletOutput ::
  MonadBlockChain m =>
  Wallet ->
  Pl.TypedValidator Validators.Unit ->
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
  MonadBlockChain m =>
  Pl.TypedValidator Validators.Unit ->
  Pl.TypedValidator Validators.Unit ->
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

retrieveRefScriptHash :: MonadBlockChain m => Pl.TxOutRef -> m (Maybe Pl.ScriptHash)
retrieveRefScriptHash oref = do
  mOut <- txOutByRef oref
  case mOut of
    Nothing -> return Nothing
    Just out -> return $ out ^. outputReferenceScriptL

checkReferenceScriptOnOref ::
  MonadBlockChain m =>
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
                  (Validators.requireRefScript expectedScriptHash)
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

useReferenceScript :: MonadBlockChain m => Wallet -> Pl.TypedValidator Validators.Unit -> m ()
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
        { txSkelIns = Map.singleton oref $ TxSkelRedeemerForReferencedScript (),
          txSkelInsReference = Set.singleton scriptOref,
          txSkelSigners = [spendingSubmitter]
        }

tests :: TestTree
tests =
  testGroup
    "reference scripts"
    [ testGroup "putting reference scripts on chain and retreiving them" $
        let theRefScript = Validators.no
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
                $ putRefScriptOnScriptOutput Validators.yes theRefScript
                  >>= retrieveRefScriptHash,
              testCase "retreiving the complete script from its hash"
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
            $ putRefScriptOnWalletOutput (wallet 3) Validators.no
              >>= checkReferenceScriptOnOref (toScriptHash Validators.yes),
          testCase "succeed if correct reference script" $
            testSucceeds def $
              putRefScriptOnWalletOutput (wallet 3) Validators.yes
                >>= checkReferenceScriptOnOref (toScriptHash Validators.yes)
        ],
      testGroup
        "using reference scripts"
        [ testCase "fail from transaction generation for missing reference scripts"
            $ testFails
              def
              ( \case
                  MCEGenerationError _ -> testSuccess
                  MCECalcFee (MCEGenerationError _) -> testSuccess
                  _ -> testFailure
              )
            $ do
              (oref, _) : _ <-
                utxosFromCardanoTx
                  <$> validateTxSkel
                    txSkelTemplate
                      { txSkelOuts =
                          [ paysScript
                              Validators.yes
                              ()
                              (Pl.lovelaceValueOf 42_000_000)
                          ],
                        txSkelSigners = [wallet 1]
                      }
              void $
                validateTxSkel
                  txSkelTemplate
                    { txSkelIns = Map.singleton oref (TxSkelRedeemerForReferencedScript ()),
                      txSkelSigners = [wallet 1]
                    },
          testCase "phase 1 - fail if using a reference script with 'TxSkelRedeemerForScript'"
            $ testFails
              def
              ( \case
                  MCEValidationError (Pl.Phase1, _) -> testSuccess
                  MCECalcFee (MCEValidationError (Pl.Phase1, _)) -> testSuccess
                  _ -> testFailure
              )
            $ do
              scriptOref <- putRefScriptOnWalletOutput (wallet 3) Validators.yes
              (oref, _) : _ <-
                utxosFromCardanoTx
                  <$> validateTxSkel
                    txSkelTemplate
                      { txSkelOuts =
                          [ paysScript
                              Validators.yes
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
            $ testFails
              def
              ( isCekEvaluationFailureWithMsg
                  def
                  (== "the required signer is missing")
              )
            $ useReferenceScript (wallet 1) (Validators.requireSigner (walletPKHash $ wallet 2)),
          testCase "succeed if referenced script's requirement is met" $
            testSucceeds def $
              useReferenceScript (wallet 1) (Validators.requireSigner (walletPKHash $ wallet 1))
        ]
    ]
