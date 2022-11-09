{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.Monad.Contract where

import qualified Cardano.Api as Api
import Control.Lens (review)
import Control.Monad
import Cooked.MockChain.Misc
import Cooked.MockChain.Monad
import Cooked.Tx.Constraints
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Void
import qualified Ledger as Pl
import qualified Ledger.Tx.CardanoAPI as Pl
import qualified Plutus.Contract as C
import qualified PlutusTx as Pl

-- TODO shall MonadFail really be the constraint on the MonadBlockChain class?
instance (C.AsContractError e) => MonadFail (C.Contract w s e) where
  fail = C.throwError . review C._OtherContractError . T.pack

instance (C.AsContractError e) => MonadBlockChain (C.Contract w s e) where
  validateTxSkel TxSkel {txConstraints, txOpts} = do
    let (lkups, constrs) = toLedgerConstraint @Constraints @Void (toConstraints txConstraints)
    tx <- C.submitTxConstraintsWith lkups constrs
    when (awaitTxConfirmed txOpts) $ C.awaitTxConfirmed $ Pl.getCardanoTxId tx
    return tx

  utxosSuchThat addr datumPred = do
    allUtxos <- M.toList <$> C.utxosAt addr
    maybeUtxosWithDatums <- forM allUtxos $ \utxo -> do
      Right (Pl.TxOut (Api.TxOut _ cVal _ _)) <- pure $ Pl.toTxOut theNetworkId $ snd utxo
      let val = Pl.fromCardanoTxOutValue cVal
      datum <- datumFromTxOut $ snd utxo
      let typedDatum = datum >>= Pl.fromBuiltinData . Pl.getDatum
      pure $
        if datumPred typedDatum val
          then Just (utxo, typedDatum)
          else Nothing
    pure $ catMaybes maybeUtxosWithDatums

  utxosSuchThisAndThat _ _ = error "Contract does not support retrieving the UTxOs without a specified address"

  txOutByRef ref = do
    mcito <- C.unspentTxOutFromRef ref
    case mcito of
         Nothing -> pure Nothing
         Just cito -> case Pl.toTxOut theNetworkId cito of
                           Left e -> fail $ "Error converting " <> show ref <> " to tx out: " <> show e
                           Right txOut -> pure $ Just txOut

  ownPaymentPubKeyHash = fmap Pl.unPaymentPubKeyHash C.ownFirstPaymentPubKeyHash

  currentSlot = C.currentSlot
  currentTime = C.currentTime
  awaitSlot = C.awaitSlot
  awaitTime = C.awaitTime

  datumFromTxOut Pl.PublicKeyChainIndexTxOut {} = pure Nothing
  -- datum is always present in the nominal case, guaranteed by chain-index
  -- TODO PORT shall we use the _mdh when it's Just?
  datumFromTxOut (Pl.ScriptChainIndexTxOut _ _ (dh, _mdh) _ _) = C.datumFromHash dh
