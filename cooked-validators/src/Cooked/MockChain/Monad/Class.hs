{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cooked.MockChain.Monad.Class where

import Control.Arrow (second)
import Cooked.MockChain.Time
import Cooked.Tx.Constraints
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Void
import qualified Ledger as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Typed.Scripts as Pl (DatumType, TypedValidator, validatorScript)
import qualified PlutusTx as Pl (FromData)

class (MonadFail m) => MonadMockChain m where
  generateTx :: TxSkel -> m Pl.Tx
  validateTx :: Pl.Tx -> m ()
  index :: m (M.Map Pl.TxOutRef Pl.TxOut)
  slotCounter :: m SlotCounter
  modifySlotCounter :: (SlotCounter -> SlotCounter) -> m ()
  utxosSuchThat ::
    (Pl.FromData a) =>
    Pl.Address ->
    (Maybe a -> Pl.Value -> Bool) ->
    m [(SpendableOut, Maybe a)]

spendableRef :: (MonadMockChain m) => Pl.TxOutRef -> m SpendableOut
spendableRef txORef = do
  Just txOut <- M.lookup txORef <$> index
  return (txORef, fromJust (Pl.fromTxOut txOut))

{-
 - This is not being used anywhere and it introduces a weird dependency cycle
spentByPK :: MonadMockChain m => Pl.PubKeyHash -> Pl.Value -> m [Constraint]
spentByPK pkh val = do
  allOuts <- pkUtxos pkh
  let (toSpend, leftOver) = spendValueFrom val $ map (second Pl.toTxOut) allOuts
  (PaysPK pkh leftOver :) . map SpendsPK <$> mapM spendableRef toSpend
-}

-- | Public-key UTxO's have no datum, hence, can be selected easily with
--  a simpler variant of 'utxosSuchThat'
pkUtxosSuchThat :: (MonadMockChain m) => Pl.PubKeyHash -> (Pl.Value -> Bool) -> m [SpendableOut]
pkUtxosSuchThat pkh pred =
  map fst
    <$> utxosSuchThat @_ @Void
      (Pl.Address (Pl.PubKeyCredential pkh) Nothing)
      (maybe pred absurd)

-- | Script UTxO's always have a datum, hence, can be selected easily with
--  a simpler variant of 'utxosSuchThat'. It is important to pass a value for type variable @a@
--  with an explicit type application to make sure the conversion to and from 'Pl.Datum' happens correctly.
scriptUtxosSuchThat ::
  (MonadMockChain m, Pl.FromData (Pl.DatumType tv)) =>
  Pl.TypedValidator tv ->
  (Pl.DatumType tv -> Pl.Value -> Bool) ->
  m [(SpendableOut, Pl.DatumType tv)]
scriptUtxosSuchThat v pred =
  map (second fromJust)
    <$> utxosSuchThat
      (Pl.Address (Pl.ScriptCredential $ Pl.validatorHash $ Pl.validatorScript v) Nothing)
      (maybe (const False) pred)

-- | Returns the output associated with a given reference
outFromOutRef :: (MonadMockChain m) => Pl.TxOutRef -> m Pl.TxOut
outFromOutRef outref = do
  mo <- M.lookup outref <$> index
  case mo of
    Just o -> return o
    Nothing -> fail ("No output associated with: " ++ show outref)

-- | Return all utxos belonging to a pubkey
pkUtxos :: (MonadMockChain m) => Pl.PubKeyHash -> m [SpendableOut]
pkUtxos = flip pkUtxosSuchThat (const True)

-- | Return all utxos belonging to a pubkey, but keep them as 'Pl.TxOut'. This is
--  for internal use.
pkUtxos' :: (MonadMockChain m) => Pl.PubKeyHash -> m [(Pl.TxOutRef, Pl.TxOut)]
pkUtxos' pkh = map (second go) <$> pkUtxos pkh
  where
    go (Pl.PublicKeyChainIndexTxOut a v) = Pl.TxOut a v Nothing
    go _ = error "pkUtxos must return only Pl.PublicKeyChainIndexTxOut's"

-- * Time management in the monad.

freezeTime :: (MonadMockChain m) => m ()
freezeTime = modifySlotCounter scFreezeTime

waitSlots :: (MonadMockChain m) => Integer -> m ()
waitSlots n = modifySlotCounter (scWaitSlots n)

waitTime :: (MonadMockChain m) => Pl.POSIXTime -> m ()
waitTime mSec = modifySlotCounter (scWait mSec)

slotIs :: (MonadMockChain m) => Integer -> m ()
slotIs n = modifySlotCounter (scSlotIs n)

timeIs :: (MonadMockChain m) => Pl.POSIXTime -> m ()
timeIs t = modifySlotCounter (scTimeIs t)
