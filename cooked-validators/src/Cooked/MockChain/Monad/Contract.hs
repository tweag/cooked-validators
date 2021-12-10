{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cooked.MockChain.Monad.Contract where

import Control.Lens (review)
import Control.Monad
import Cooked.MockChain.Monad
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Ledger as Pl
import qualified Ledger.Constraints as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Tx as Pl
import qualified Plutus.Contract as C
import qualified Plutus.Contract.Request as C
import qualified Plutus.Contract.Types as C
import qualified PlutusTx as Pl

-- TODO shall MonadFail really be the constraint on the MonadMockChain class?
instance (C.AsContractError e) => MonadFail (C.Contract w s e) where
  fail = C.throwError . review C._OtherError . T.pack

instance (C.AsContractError e) => MonadMockChain (C.Contract w s e) where
  validateTxSkel txSkel = _

  utxosSuchThat addr datumPred = do
    allUtxos <- M.toList <$> C.utxosAt addr
    maybeUtxosWithDatums <- forM allUtxos $ \utxo -> do
      let (Pl.TxOut _ val datumHash) = Pl.toTxOut $ snd utxo
      datum <- maybe (pure Nothing) C.datumFromHash datumHash
      let typedDatum = datum >>= Pl.fromBuiltinData . Pl.getDatum
      pure $ if datumPred typedDatum val
                then Just (utxo, typedDatum)
                else Nothing
    pure $ catMaybes maybeUtxosWithDatums

  txOutByRef ref = fmap Pl.toTxOut <$> C.txOutFromRef ref

  getSlotConfig = _

  getCurrentSlot = Pl.getSlot <$> C.currentSlot

  waitNumSlots cnt = do
    slot <- getCurrentSlot
    void $ C.awaitSlot $ Pl.Slot $ cnt + slot
