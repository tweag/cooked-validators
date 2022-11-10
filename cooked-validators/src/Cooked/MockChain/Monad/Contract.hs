{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.Monad.Contract where

import Control.Lens (review)
import Control.Monad
import Cooked.MockChain
import Cooked.MockChain.Monad
import Cooked.Tx.Constraints
import Cooked.Tx.Constraints.Type
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Void
import qualified Ledger as Pl
import qualified Ledger.Scripts as Pl
import qualified Plutus.Contract as C
import qualified PlutusTx as Pl

-- TODO shall MonadFail really be the constraint on the MonadBlockChain class?
instance (C.AsContractError e) => MonadFail (C.Contract w s e) where
  fail = C.throwError . review C._OtherContractError . T.pack

instance (C.AsContractError e) => MonadBlockChain (C.Contract w s e) where
  validateTxSkel skel@TxSkel {_txSkelOpts = txOpts} = do
    let (lkups, constrs) = toLedgerConstraint @_ @Void skel
    tx <- C.submitTxConstraintsWith lkups constrs
    when (awaitTxConfirmed txOpts) $ C.awaitTxConfirmed $ Pl.getCardanoTxId tx
    return tx

  utxosSuchThat addr datumPred = do
    allUtxos <- M.toList <$> C.utxosAt addr
    maybeUtxosWithDatums <- forM allUtxos $ \utxo -> do
      let (Pl.TxOut _ val _) = Pl.toTxOut $ snd utxo
      datum <- datumFromTxOut $ snd utxo
      let typedDatum = datum >>= Pl.fromBuiltinData . Pl.getDatum
      pure $
        if datumPred typedDatum val
          then Just (utxo, typedDatum)
          else Nothing
    pure $ catMaybes maybeUtxosWithDatums

  utxosSuchThisAndThat _ _ = error "Contract does not support retrieving the UTxOs without a specified address"

  txOutByRef ref = fmap Pl.toTxOut <$> C.unspentTxOutFromRef ref

  ownPaymentPubKeyHash = fmap Pl.unPaymentPubKeyHash C.ownFirstPaymentPubKeyHash

  currentSlot = C.currentSlot
  currentTime = C.currentTime
  awaitSlot = C.awaitSlot
  awaitTime = C.awaitTime

  datumFromHash = C.datumFromHash
