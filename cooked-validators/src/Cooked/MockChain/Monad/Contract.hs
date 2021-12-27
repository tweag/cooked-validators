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
  fail = C.throwError . review C._OtherError . T.pack

instance (C.AsContractError e) => MonadBlockChain (C.Contract w s e) where
  validateTxSkelOpts opts txSkel0 = do
    let (lkups, constrs, additionalSigners) = toLedgerConstraints @Void (txConstraints txSkel0)
    unless (null additionalSigners) $
      fail "validateTxSkel: cannot validate a tx that requires others to sign when running in the Contract monad"
    txId <- Pl.getCardanoTxId <$> C.submitTxConstraintsWith lkups constrs
    when (awaitTxConfirmed opts) $ C.awaitTxConfirmed txId
    return txId

  utxosSuchThat addr datumPred = do
    allUtxos <- M.toList <$> C.utxosAt addr
    maybeUtxosWithDatums <- forM allUtxos $ \utxo -> do
      let (Pl.TxOut _ val datumHash) = Pl.toTxOut $ snd utxo
      datum <- maybe (pure Nothing) C.datumFromHash datumHash
      let typedDatum = datum >>= Pl.fromBuiltinData . Pl.getDatum
      pure $
        if datumPred typedDatum val
          then Just (utxo, typedDatum)
          else Nothing
    pure $ catMaybes maybeUtxosWithDatums

  txOutByRef ref = fmap Pl.toTxOut <$> C.txOutFromRef ref

  currentSlot = C.currentSlot
  currentTime = C.currentTime
  awaitSlot = C.awaitSlot
  awaitTime = C.awaitTime
