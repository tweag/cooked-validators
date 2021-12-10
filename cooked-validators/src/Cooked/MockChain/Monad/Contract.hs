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

-- TODO shall MonadFail really be the constraint on the MonadMockChain class?
instance (C.AsContractError e) => MonadFail (C.Contract w s e) where
  fail = C.throwError . review C._OtherError . T.pack

instance (C.AsContractError e) => MonadMockChain (C.Contract w s e) where
  validateTxSkel txSkel0 = do
    let (lkups, constrs, additionalSigners) = toLedgerConstraints @Void (txConstraints txSkel0)
    unless (null additionalSigners) $
      fail "validateTxSkel: cannot validate a tx that requires others to sign when running in the Contract monad"
    tx <- C.submitTxConstraintsWith lkups constrs
    C.awaitTxConfirmed (Pl.getCardanoTxId tx)

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

{- TODO: Trying to remove getSlotConfig from everywhere; if we really cant, then
there is always infering the slot config from observation!

-- Hack to get a slot config from observations with a little linear extrapolation
-- assuming that time increases linearly :)
hackedObserveSlotConfig :: (C.AsContractError e) => C.Contract w s e Pl.SlotConfig
hackedObserveSlotConfig = do
  s0 <- C.waitNSlots 1
  t0ms <- Pl.getPOSIXTime <$> C.currentTime
  s1 <- C.waitNSlots 1
  t1ms <- Pl.getPOSIXTime <$> C.currentTime
  -- time = m * slot + b
  let deltaT = fromIntegral (t0ms - t1ms) :: Double
  let m = deltaT / (fromIntegral $ s0 - s1)
  let b = fromIntegral t1ms - m * fromIntegral s1
  return $ Pl.SlotConfig (t1ms - t0ms) (Pl.POSIXTime $ round b)
-}
