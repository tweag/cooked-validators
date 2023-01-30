{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.Contract where

import qualified Cardano.Node.Emulator as Emulator
import Control.Lens (review)
import Control.Lens.Prism (prism')
import Control.Monad.Except
import Cooked.MockChain.BlockChain
import qualified Data.Map as Map
import qualified Ledger.Address as Ledger
import qualified Ledger.Tx as Ledger
import Plutus.Contract (Contract (..))
import qualified Plutus.Contract as Contract

-- instance Contract.AsContractError e => MonadFail (Contract w s e) where
--   fail = Contract.throwError . review Contract._OtherContractError . Text.pack

-- instance MonadError MockChainError (Contract w s e) where

instance Contract.AsContractError e => MonadError MockChainError (Contract w s e)

instance Contract.AsContractError e => MonadBlockChainWithoutValidation (Contract w s e) where
  getParams = Contract.getParams
  allUtxosLedger = throwError $ review Contract._OtherContractError ""
  utxosAtLedger address = Contract.utxosAt $ undefined address
  validatorFromHash = Contract.validatorFromHash
  datumFromHash dHash = fmap (,"Datum pretty printing in the Contract monad not implemented") <$> Contract.datumFromHash dHash
  txOutByRefLedger oref = do
    theNetworkId <- Emulator.pNetworkId <$> getParams
    mDecoratedTxOut <- Contract.txOutFromRef oref
    case mDecoratedTxOut of
      Nothing -> return Nothing
      Just decoratedTxOut -> case Ledger.toTxOut theNetworkId decoratedTxOut of
        Left err -> undefined
        Right txOut -> return . Just $ txOut
  ownPaymentPubKeyHash = Ledger.unPaymentPubKeyHash <$> Contract.ownFirstPaymentPubKeyHash
  currentTime = fst <$> Contract.currentNodeClientTimeRange
  currentSlot = Contract.currentNodeClientSlot
  awaitTime = Contract.awaitTime
  awaitSlot = Contract.awaitSlot

-- instance Contract.AsContractError e => MonadBlockChain (Contract w s e) where
--   validateTxSkel skelUnbal = do
--     undefined
--     -- let balancingWallet =
--     --       case txOptBalanceWallet . txSkelOpts $ skelUnbal of
--     --         BalanceWithFirstSigner -> NEList.head (txSkelSigners skelUnbal)
--     --         BalanceWith wallet -> wallet
--     -- let balancingWalletPkh = walletPKHash balancingWallet
--     -- let collateralWallet = balancingWallet
--     -- (skel, fee) <-
--     --   if txOptBalance . txSkelOpts $ skelUnbal
--     --     then
--     --       setFeeAndBalance
--     --         balancingWalletPkh
--     --         skelUnbal
--     --     else return (skelUnbal, Fee 0)
--     -- collateralInputs <- calcCollateral collateralWallet -- TODO: Why is it OK to balance first and then add collateral?
--     -- params <- asks mceParams
--     -- managedData <- gets mcstDatums
--     -- managedTxOuts <- gets $ utxoIndexToTxOutMap . mcstIndex
--     -- managedValidators <- gets mcstValidators
--     -- case generateTxBodyContent def {gtpCollateralIns = collateralInputs, gtpFee = fee} params managedData managedTxOuts managedValidators skel of
--     --   Left err -> throwError $ MCEGenerationError err
--     --   Right txBodyContent -> Contract.submitBalancedTx . Pl.CardanoApiEmulatorEraTx . undefined . txBodyContent
