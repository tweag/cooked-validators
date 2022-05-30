{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.Monad.Contract where

import Control.Lens (review)
import Control.Monad
import Cooked.MockChain.Monad
import Cooked.Tx.Constraints
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Void
import qualified Ledger as Pl
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
      let (Pl.TxOut _ val _) = Pl.toTxOut $ snd utxo
      datum <- datumFromTxOut $ snd utxo
      let typedDatum = datum >>= Pl.fromBuiltinData . Pl.getDatum
      pure $
        if datumPred typedDatum val
          then Just (utxo, typedDatum)
          else Nothing
    pure $ catMaybes maybeUtxosWithDatums

  txOutByRef ref = fmap Pl.toTxOut <$> C.unspentTxOutFromRef ref

  ownPaymentPubKeyHash = fmap Pl.unPaymentPubKeyHash C.ownPaymentPubKeyHash

  currentSlot = C.currentSlot
  currentTime = C.currentTime
  awaitSlot = C.awaitSlot
  awaitTime = C.awaitTime

datumFromTxOut :: (C.AsContractError e) => Pl.ChainIndexTxOut -> C.Contract w s e (Maybe Pl.Datum)
datumFromTxOut (Pl.PublicKeyChainIndexTxOut {}) = pure Nothing
datumFromTxOut (Pl.ScriptChainIndexTxOut _ _ (Right d) _) = pure $ Just d
-- datum is always present in the nominal case, guaranteed by chain-index
datumFromTxOut (Pl.ScriptChainIndexTxOut _ _ (Left dh) _) = C.datumFromHash dh
