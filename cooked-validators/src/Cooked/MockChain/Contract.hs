{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.Contract where

import qualified Cardano.Api as C
import qualified Cardano.Node.Emulator as Emulator
import Control.Arrow
import qualified Control.Lens as Lens
import Control.Monad.Except
import Cooked.MockChain.Balancing
import Cooked.MockChain.BlockChain
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Ledger.Address as Ledger
import qualified Ledger.Tx as Ledger
import qualified Ledger.Tx.CardanoAPI as Ledger
import Optics.Core
import Plutus.Contract (AsContractError, Contract (..), ContractError)
import qualified Plutus.Contract as Contract
import Prettyprinter (emptyDoc)

instance AsContractError e => MonadFail (Contract w s e) where
  fail = Contract.throwError @e . Lens.review Contract._OtherContractError . Text.pack

class AsMockChainError e where
  mockChainErrorP :: Prism' e MockChainError

instance AsMockChainError ContractError where
  mockChainErrorP =
    prism'
      (Lens.review Contract._OtherContractError . Text.pack . show) -- TODO use pretty printing for errors?
      (const Nothing)

instance AsMockChainError e => MonadError MockChainError (Contract w s e) where
  throwError = Contract.throwError @e . review mockChainErrorP
  catchError act handler =
    Contract.handleError
      ( \case
          Left err -> Contract.throwError err
          Right mcErr -> handler mcErr
      )
      $ Contract.mapError
        ( \err -> case err ^? mockChainErrorP of
            Nothing -> Left err
            Just mcErr -> Right mcErr
        )
        act

instance (AsMockChainError e, AsContractError e) => MonadBlockChainBalancing (Contract w s e) where
  getParams = Contract.getParams
  utxosAtLedger address = do
    theNetworkId <- Emulator.pNetworkId <$> getParams
    let mCardanoAddress = Ledger.toCardanoAddressInEra theNetworkId address
    case mCardanoAddress of
      Left err -> throwError $ OtherMockChainError err
      Right cardanoAddress -> do
        decoratedTxOuts <- Contract.utxosAt cardanoAddress
        let mTxOuts =
              mapM
                (\(oref, dtxout) -> right (oref,) (Ledger.toTxOut theNetworkId dtxout))
                . Map.toList
                $ decoratedTxOuts
        case mTxOuts of
          Left err -> throwError $ OtherMockChainError err
          Right txOuts -> return txOuts
  validatorFromHash = Contract.validatorFromHash
  datumFromHash dHash = fmap (,emptyDoc) <$> Contract.datumFromHash dHash
  txOutByRefLedger oref = do
    theNetworkId <- Emulator.pNetworkId <$> getParams
    mDecoratedTxOut <- Contract.txOutFromRef oref
    case mDecoratedTxOut of
      Nothing -> return Nothing
      Just decoratedTxOut -> case Ledger.toTxOut theNetworkId decoratedTxOut of
        Left err -> throwError $ OtherMockChainError err
        Right txOut -> return . Just $ txOut

instance (AsMockChainError e, AsContractError e) => MonadBlockChainWithoutValidation (Contract w s e) where
  allUtxosLedger = throwError $ OtherMockChainError "allUtxosLedger can not be used in the Contract monad"
  ownPaymentPubKeyHash = Ledger.unPaymentPubKeyHash <$> Contract.ownFirstPaymentPubKeyHash
  currentTime = fst <$> Contract.currentNodeClientTimeRange
  currentSlot = Contract.currentNodeClientSlot
  awaitTime = Contract.awaitTime
  awaitSlot = Contract.awaitSlot

instance (AsMockChainError e, AsContractError e) => MonadBlockChain (Contract w s e) where
  validateTxSkel skelUnbal = do
    (skel, fee, collateralInputs) <- balancedTxSkel skelUnbal
    tx <- balancedTx (skel, fee, collateralInputs)
    Contract.submitBalancedTx . Ledger.CardanoApiTx . flip Ledger.SomeTx C.BabbageEraInCardanoMode $ tx
