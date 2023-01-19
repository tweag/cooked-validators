{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cooked.ReferenceScriptsSpec where

import Control.Monad
import Cooked
import Cooked.Tx.Constraints.Type
import Data.Default
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Ledger.Ada as Pl
import Optics.Core
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Pl
import qualified Plutus.V2.Ledger.Api as Pl
import qualified PlutusTx as Pl
import qualified PlutusTx.Prelude as Pl
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

    wrap = Pl.mkUntypedValidator @() @()

-- | The validator that never agrees to the transaction
noValidator :: Pl.TypedValidator MockContract
noValidator =
  Pl.mkTypedValidator @MockContract
    $$(Pl.compile [||val||])
    $$(Pl.compile [||wrap||])
  where
    val :: () -> () -> Pl.ScriptContext -> Bool
    val _ _ _ = False

    wrap = Pl.mkUntypedValidator @() @()

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

    wrap = Pl.mkUntypedValidator @() @()

-- | This validator ensures that there is a transaction input that has a
-- reference script with the given hash.
requireRefScriptValidator :: Pl.ScriptHash -> Pl.TypedValidator MockContract
requireRefScriptValidator =
  Pl.mkTypedValidatorParam @MockContract
    $$(Pl.compile [||val||])
    $$(Pl.compile [||wrap||])
  where
    val :: Pl.ScriptHash -> () -> () -> Pl.ScriptContext -> Bool
    val refScriptHash _ _ (Pl.ScriptContext txInfo _) =
      Pl.traceIfFalse "there is no reference input with the correct script hash" Pl.$
        Pl.any
          ( \(Pl.TxInInfo _ (Pl.TxOut _ _ _ mRefScriptHash)) ->
              Just refScriptHash Pl.== mRefScriptHash
          )
          (Pl.txInfoReferenceInputs txInfo)

    wrap = Pl.mkUntypedValidator @() @()

putRefScriptOnWalletOutput ::
  MonadBlockChain m =>
  Wallet ->
  Pl.TypedValidator MockContract ->
  m Pl.TxOutRef
putRefScriptOnWalletOutput recipient referencedScript =
  fst . head . utxosFromCardanoTx
    <$> validateTxSkel
      txSkelTemplate
        { txSkelOuts =
            [ paysPKWithReferenceScript
                (walletPKHash recipient)
                (Pl.lovelaceValueOf 20_000_000)
                referencedScript
            ]
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
        { txSkelOuts =
            [ Pays $
                ConcreteOutput
                  recipient
                  Nothing
                  (Pl.lovelaceValueOf 20_000_000)
                  (TxSkelOutDatum ())
                  (Just referencedScript)
            ]
        }

retrieveRefScriptHash :: MonadBlockChain m => Pl.TxOutRef -> m (Maybe Pl.ScriptHash)
retrieveRefScriptHash oref = do
  mOut <- txOutByRef oref
  case mOut of
    Nothing -> return Nothing
    Just out -> return $ out ^. outputReferenceScriptL

useReferenceScript ::
  MonadBlockChain m =>
  Wallet ->
  Pl.TypedValidator MockContract ->
  Pl.TxOutRef ->
  m ()
useReferenceScript submitter spendingValidator refScriptOref = do
  (oref, _) : _ <-
    utxosFromCardanoTx
      <$> validateTxSkel
        txSkelTemplate
          { txSkelOuts =
              [ paysScript
                  spendingValidator
                  ()
                  (Pl.lovelaceValueOf 42_000_000)
              ]
          }
  void $
    validateTxSkel
      (txSkelSubmittedBy submitter)
        { txSkelIns = Map.singleton oref $ TxSkelRedeemerForScript (),
          txSkelInsReference = Set.singleton refScriptOref
        }

tests :: TestTree
tests =
  testGroup
    "reference scripts"
    [ testGroup "putting reference scripts on chain" $
        let -- The actual reference script being put on chain does not matter
            -- here, as it's not being run
            theRefScript = noValidator
            theRefScriptHash = toScriptHash theRefScript
         in [ testCase "on a public key output" $
                testSucceedsFrom'
                  ( \mScriptHash _ ->
                      testCounterexample "the script hash on the retrieved output is wrong" $
                        Just theRefScriptHash .==. mScriptHash
                  )
                  def
                  $ putRefScriptOnWalletOutput (wallet 3) theRefScript
                    >>= retrieveRefScriptHash,
              testCase "on a script output" $
                testSucceedsFrom'
                  ( \mScriptHash _ ->
                      testCounterexample "the script hash on the retrieved output is wrong" $
                        Just theRefScriptHash .==. mScriptHash
                  )
                  def
                  $ putRefScriptOnScriptOutput yesValidator theRefScript
                    >>= retrieveRefScriptHash
            ],
      testGroup
        "using reference scripts"
        [ testCase "fail if the spending script's requirement is violated" $
            testFailsFrom'
              ( isCekEvaluationFailureWithMsg
                  (== "there is no reference input with the correct script hash")
              )
              def
              $ putRefScriptOnWalletOutput
                (wallet 3)
                noValidator
                >>= useReferenceScript
                  (wallet 1)
                  (requireRefScriptValidator $ toScriptHash yesValidator),
          testCase "fail if the referenced script's requirement is violated" $
            testFailsFrom'
              ( isCekEvaluationFailureWithMsg
                  (== "the required signer is missing")
              )
              def
              $ putRefScriptOnWalletOutput
                (wallet 3)
                (requireSignerValidator (walletPKHash $ wallet 1))
                >>= useReferenceScript
                  (wallet 2)
                  yesValidator,
          testCase "succeed if both the spending and the referenced script's requirements are met" $
            testSucceeds $
              let referencedValidator = requireSignerValidator (walletPKHash $ wallet 1)
                  spendingValidator = requireRefScriptValidator $ toScriptHash referencedValidator
               in putRefScriptOnWalletOutput
                    (wallet 3)
                    referencedValidator
                    >>= useReferenceScript
                      (wallet 1)
                      spendingValidator
        ]
    ]
