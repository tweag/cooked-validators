{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Plutus.Contract (Contract (..))
import qualified Plutus.Contract as Contract

instance Contract.AsContractError e => MonadFail (Contract w s e) where
  fail = Contract.throwError @e . Lens.review Contract._OtherContractError . Text.pack

instance Contract.AsContractError e => MonadError MockChainError (Contract w s e) where
  throwError = Contract.throwError @e . Lens.review Contract._OtherContractError . Text.pack . show -- TODO maybe use pretty printing for MockChain Error, and have @render . pretty@, instead of @Text.pack . show@?
  catchError act handler =
    Contract.handleError
      handler
      ( Contract.mapError
          (OtherMockChainError . (Lens.^? Contract._ContractError))
          act
      )

instance Contract.AsContractError e => MonadBlockChainWithoutValidation (Contract w s e) where
  getParams = Contract.getParams
  allUtxosLedger = throwError $ OtherMockChainError "allUtxosLedger can not be used in the Contract monad"
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
  datumFromHash dHash = fmap (,"Datum pretty printing in the Contract monad not implemented") <$> Contract.datumFromHash dHash
  txOutByRefLedger oref = do
    theNetworkId <- Emulator.pNetworkId <$> getParams
    mDecoratedTxOut <- Contract.txOutFromRef oref
    case mDecoratedTxOut of
      Nothing -> return Nothing
      Just decoratedTxOut -> case Ledger.toTxOut theNetworkId decoratedTxOut of
        Left err -> throwError $ OtherMockChainError err
        Right txOut -> return . Just $ txOut
  ownPaymentPubKeyHash = Ledger.unPaymentPubKeyHash <$> Contract.ownFirstPaymentPubKeyHash
  currentTime = fst <$> Contract.currentNodeClientTimeRange
  currentSlot = Contract.currentNodeClientSlot
  awaitTime = Contract.awaitTime
  awaitSlot = Contract.awaitSlot

instance Contract.AsContractError e => MonadBlockChain (Contract w s e) where
  validateTxSkel skelUnbal = do
    (skel, fee, collateralInputs) <- balancedTxSkel skelUnbal
    tx <- balancedTx (skel, fee, collateralInputs)
    Contract.submitBalancedTx . Ledger.CardanoApiTx . flip Ledger.SomeTx C.BabbageEraInCardanoMode $ tx
