{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.ReferenceScriptsSpec where

import Control.Monad
import Cooked
import Data.Default
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Ledger.Index as Pl
import Optics.Core
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Plutus.Script.Utils.Typed as Pl
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import qualified PlutusTx as Pl
import qualified PlutusTx.Prelude as Pl
import qualified Prettyprinter as PP
import Test.Tasty
import Test.Tasty.HUnit

data MockContract

instance Pl.ValidatorTypes MockContract where
  type RedeemerType MockContract = ()
  type DatumType MockContract = ()

-- | The validator that always agrees to the transaction
yesValidator :: Pl.TypedValidator MockContract
yesValidator =
  Pl.mkTypedValidator @MockContract
    $$(Pl.compile [||val||])
    $$(Pl.compile [||wrap||])
  where
    val :: () -> () -> Pl.ScriptContext -> Bool
    val _ _ _ = True

    wrap = Pl.mkUntypedValidator

-- | The validator that never agrees to the transaction
noValidator :: Pl.TypedValidator MockContract
noValidator =
  Pl.mkTypedValidator @MockContract
    $$(Pl.compile [||val||])
    $$(Pl.compile [||wrap||])
  where
    val :: () -> () -> Pl.ScriptContext -> Bool
    val _ _ _ = False

    wrap = Pl.mkUntypedValidator

-- | This validator ensures that the given public key signs the transaction.
requireSignerValidator :: Pl.PubKeyHash -> Pl.TypedValidator MockContract
requireSignerValidator =
  Pl.mkTypedValidatorParam @MockContract
    $$(Pl.compile [||val||])
    $$(Pl.compile [||wrap||])
  where
    val :: Pl.PubKeyHash -> () -> () -> Pl.ScriptContext -> Bool
    val pkh _ _ (Pl.ScriptContext txInfo _) =
      Pl.traceIfFalse "the required signer is missing" Pl.$
        Pl.elem pkh (Pl.txInfoSignatories txInfo)

    wrap = Pl.mkUntypedValidator

-- | This validator ensures that there is a transaction input that has a
-- reference script with the given hash.
requireRefScriptValidator :: Pl.ScriptHash -> Pl.TypedValidator MockContract
requireRefScriptValidator =
  Pl.mkTypedValidatorParam @MockContract
    $$(Pl.compile [||val||])
    $$(Pl.compile [||wrap||])
  where
    val :: Pl.ScriptHash -> () -> () -> Pl.ScriptContext -> Bool
    val expectedScriptHash _ _ (Pl.ScriptContext txInfo _) =
      Pl.traceIfFalse "there is no reference input with the correct script hash" Pl.$
        Pl.any
          ( \(Pl.TxInInfo _ (Pl.TxOut _ _ _ mRefScriptHash)) ->
              Just expectedScriptHash Pl.== mRefScriptHash
          )
          (Pl.txInfoReferenceInputs txInfo)

    wrap = Pl.mkUntypedValidator

putRefScriptOnWalletOutput ::
  MonadBlockChain m =>
  Wallet ->
  Pl.TypedValidator MockContract ->
  m Pl.TxOutRef
putRefScriptOnWalletOutput recipient referencedScript =
  fst . head . utxosFromCardanoTx
    <$> validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelOuts =
            [ paysPKWithReferenceScript
                (walletPKHash recipient)
                (Pl.lovelaceValueOf 1)
                referencedScript
            ],
          txSkelSigners = [wallet 1]
        }

putRefScriptOnScriptOutput ::
  MonadBlockChain m =>
  Pl.TypedValidator MockContract ->
  Pl.TypedValidator MockContract ->
  m Pl.TxOutRef
putRefScriptOnScriptOutput recipient referencedScript =
  fst . head . utxosFromCardanoTx
    <$> validateTxSkel
      txSkelTemplate
        { txSkelOpts = def {txOptEnsureMinAda = True},
          txSkelOuts =
            [ Pays $
                ConcreteOutput
                  recipient
                  Nothing
                  (Pl.lovelaceValueOf 1)
                  (TxSkelOutDatum ())
                  (Just referencedScript)
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
                  (requireRefScriptValidator expectedScriptHash)
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

useReferenceScript :: MonadBlockChain m => Wallet -> Pl.TypedValidator MockContract -> m ()
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
        let theRefScript = noValidator
            theRefScriptHash = toScriptHash theRefScript
         in [ testCase "on a public key output" $
                testSucceedsFrom'
                  def
                  ( \mScriptHash _ ->
                      testCounterexample "the script hash on the retrieved output is wrong" $
                        Just theRefScriptHash .==. mScriptHash
                  )
                  def
                  $ putRefScriptOnWalletOutput (wallet 3) theRefScript
                    >>= retrieveRefScriptHash,
              testCase "on a script output" $
                testSucceedsFrom'
                  def
                  ( \mScriptHash _ ->
                      testCounterexample "the script hash on the retrieved output is wrong" $
                        Just theRefScriptHash .==. mScriptHash
                  )
                  def
                  $ putRefScriptOnScriptOutput yesValidator theRefScript
                    >>= retrieveRefScriptHash,
              testCase "retreiving the complete script from its hash" $
                testSucceedsFrom'
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
        [ testCase "fail if wrong reference script" $
            testFailsFrom'
              def
              ( isCekEvaluationFailureWithMsg
                  def
                  (== "there is no reference input with the correct script hash")
              )
              def
              $ putRefScriptOnWalletOutput (wallet 3) noValidator
                >>= checkReferenceScriptOnOref (toScriptHash yesValidator),
          testCase "succeed if correct reference script" $
            testSucceeds def $
              putRefScriptOnWalletOutput (wallet 3) yesValidator
                >>= checkReferenceScriptOnOref (toScriptHash yesValidator)
        ],
      testGroup
        "using reference scripts"
        [ testCase "fail from transaction generation for missing reference scripts" $
            testFailsFrom'
              def
              ( \case
                  MCEGenerationError _ -> testSuccess
                  MCECalcFee (MCEGenerationError _) -> testSuccess
                  _ -> testFailure
              )
              def
              $ do
                (oref, _) : _ <-
                  utxosFromCardanoTx
                    <$> validateTxSkel
                      txSkelTemplate
                        { txSkelOuts =
                            [ paysScript
                                yesValidator
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
          testCase "phase 1 - fail if using a reference script with 'TxSkelRedeemerForScript'" $
            testFailsFrom'
              def
              ( \case
                  MCEValidationError (Pl.Phase1, _) -> testSuccess
                  MCECalcFee (MCEValidationError (Pl.Phase1, _)) -> testSuccess
                  _ -> testFailure
              )
              def
              $ do
                scriptOref <- putRefScriptOnWalletOutput (wallet 3) yesValidator
                (oref, _) : _ <-
                  utxosFromCardanoTx
                    <$> validateTxSkel
                      txSkelTemplate
                        { txSkelOuts =
                            [ paysScript
                                yesValidator
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
            $ testFailsFrom'
              def
              ( isCekEvaluationFailureWithMsg
                  def
                  (== "the required signer is missing")
              )
              def
              $ useReferenceScript (wallet 1) (requireSignerValidator (walletPKHash $ wallet 2)),
          testCase "succeed if referenced script's requirement is met" $
            testSucceeds def $
              useReferenceScript (wallet 1) (requireSignerValidator (walletPKHash $ wallet 1))
        ]
    ]
