{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Cooked.MockChain
  ( module Cooked.MockChain.Base,
    module Cooked.MockChain.Wallet,
    -- Our type for UTxOS
    SpendableOut,
    spendableRef,

    -- * Validating Transactions
    validateTx,

    -- * Selecting UTxO's
    utxosSuchThat,
    pkUtxosSuchThat,
    pkUtxos,
    pkUtxos',
    scriptUtxosSuchThat,
    outFromOutRef,

    -- * Slot Management
    slot,
  )
where

import Control.Arrow (second)
import Control.Monad.Except
import Control.Monad.State
import Cooked.MockChain.Base
import Cooked.MockChain.Wallet
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import qualified Data.Set as S
import Data.Void
import qualified Ledger as Pl
import qualified Ledger.Credential as Pl
import qualified Ledger.Typed.Scripts as Pl (DatumType, TypedValidator, validatorScript)
import qualified PlutusTx as Pl

-- | A 'SpendableOut' is an outref that is ready to be spend; with its
--  underlying 'Pl.ChainIndexTxOut'.
type SpendableOut = (Pl.TxOutRef, Pl.ChainIndexTxOut)

spendableRef :: (Monad m) => Pl.TxOutRef -> MockChainT m SpendableOut
spendableRef txORef = do
  Just txOut <- gets (M.lookup txORef . Pl.getIndex . mcstIndex)
  return (txORef, fromJust (Pl.fromTxOut txOut))

-- * Validating Transactions

-- | Validates a transaction and, upon success, updates the utxo map; You can generate
--  transactions with the helpers from "Cooked.Tx.Generator".
validateTx :: (Monad m) => Pl.Tx -> MockChainT m ()
validateTx tx = do
  s <- slot
  ix <- gets mcstIndex
  let res = Pl.runValidation (Pl.validateTransaction s tx) ix
  -- case trace (show $ snd res) $ fst res of
  case fst res of
    (Just err, _) -> throwError (MCEValidationError err)
    (Nothing, ix') -> do
      -- Validation succeeded; now we update the indexes and the managed datums.
      -- The new mcstIndex is just `ix'`; the new mcstDatums is computed by
      -- removing the datum hashes have been consumed and adding
      -- those that have been created in `tx`.
      let consumedIns = map Pl.txInRef $ S.toList (Pl.txInputs tx) ++ S.toList (Pl.txCollateral tx)
      consumedDHs <- catMaybes <$> mapM (fmap Pl.txOutDatumHash . outFromOutRef) consumedIns
      let consumedDHs' = M.fromList $ zip consumedDHs (repeat ())
      modify'
        ( \st ->
            st
              { mcstIndex = ix',
                mcstDatums =
                  (mcstDatums st `M.difference` consumedDHs')
                    `M.union` Pl.txData tx
              }
        )

-- * Selecting UTxO's

-- | Returns a list of spendable outputs that belong to a given address and satisfy a given predicate;
--  Additionally, return the datum present in there if it happened to be a script output. It is important
--  to use @-XTypeApplications@ and pass a value for type variable @a@ below.
utxosSuchThat ::
  forall a m.
  (Monad m, Pl.FromData a) =>
  Pl.Address ->
  (Maybe a -> Pl.Value -> Bool) ->
  MockChainT m [(SpendableOut, Maybe a)]
utxosSuchThat addr datumPred = do
  ix <- gets (Pl.getIndex . mcstIndex)
  let ix' = M.filter ((== addr) . Pl.txOutAddress) ix
  mapMaybe (fmap assocl . rstr) <$> mapM (\(oref, out) -> (oref,) <$> go oref out) (M.toList ix')
  where
    go :: Pl.TxOutRef -> Pl.TxOut -> MockChainT m (Maybe (Pl.ChainIndexTxOut, Maybe a))
    go oref (Pl.TxOut oaddr val mdatumH) =
      case Pl.addressCredential oaddr of
        -- A PK credential has no datum; just check whether we want to select this output or not.
        Pl.PubKeyCredential _ ->
          if datumPred Nothing val
            then return . Just $ (Pl.PublicKeyChainIndexTxOut oaddr val, Nothing)
            else return Nothing
        -- A script credential, on the other hand, must have a datum. Hence, we'll go look on our map of
        -- managed datum for a relevant datum, try to convert it to a value of type @a@ then see
        -- if the user wants to select said output.
        Pl.ScriptCredential (Pl.ValidatorHash vh) -> do
          managedDatums <- gets mcstDatums
          datumH <- maybe (fail $ "ScriptCredential with no datum hash: " ++ show oref) return mdatumH
          datum <-
            maybe
              (fail $ "Unmanaged datum with hash: " ++ show datumH ++ " at: " ++ show oref)
              return
              $ M.lookup datumH managedDatums
          a <-
            maybe
              (fail $ "Can't convert from builtin data at: " ++ show oref ++ "; are you sure this is the right type?")
              return
              (Pl.fromBuiltinData (Pl.getDatum datum))
          if datumPred (Just a) val
            then return . Just $ (Pl.ScriptChainIndexTxOut oaddr (Left $ Pl.ValidatorHash vh) (Right datum) val, Just a)
            else return Nothing

-- | Public-key UTxO's have no datum, hence, can be selected easily with
--  a simpler variant of 'utxosSuchThat'
pkUtxosSuchThat :: (Monad m) => Pl.PubKeyHash -> (Pl.Value -> Bool) -> MockChainT m [SpendableOut]
pkUtxosSuchThat pkh pred =
  map fst
    <$> utxosSuchThat @Void
      (Pl.Address (Pl.PubKeyCredential pkh) Nothing)
      (maybe pred absurd)

-- | Return all utxos belonging to a pubkey
pkUtxos :: (Monad m) => Pl.PubKeyHash -> MockChainT m [SpendableOut]
pkUtxos = flip pkUtxosSuchThat (const True)

-- | Return all utxos belonging to a pubkey, but keep them as 'Pl.TxOut'. This is
--  for internal use.
pkUtxos' :: (Monad m) => Pl.PubKeyHash -> MockChainT m [(Pl.TxOutRef, Pl.TxOut)]
pkUtxos' pkh = map (second go) <$> pkUtxos pkh
  where
    go (Pl.PublicKeyChainIndexTxOut a v) = Pl.TxOut a v Nothing
    go _ = error "pkUtxos must return only Pl.PublicKeyChainIndexTxOut's"

-- | Script UTxO's always have a datum, hence, can be selected easily with
--  a simpler variant of 'utxosSuchThat'. It is important to pass a value for type variable @a@
--  with an explicit type application to make sure the conversion to and from 'Pl.Datum' happens correctly.
scriptUtxosSuchThat ::
  forall tv m.
  (Monad m, Pl.FromData (Pl.DatumType tv)) =>
  Pl.TypedValidator tv ->
  (Pl.DatumType tv -> Pl.Value -> Bool) ->
  MockChainT m [(SpendableOut, Pl.DatumType tv)]
scriptUtxosSuchThat v pred =
  map (second fromJust)
    <$> utxosSuchThat
      (Pl.Address (Pl.ScriptCredential $ Pl.validatorHash $ Pl.validatorScript v) Nothing)
      (maybe (const False) pred)

-- | Returns the output associated with a given reference
outFromOutRef :: (Monad m) => Pl.TxOutRef -> MockChainT m Pl.TxOut
outFromOutRef outref = do
  mo <- gets (M.lookup outref . Pl.getIndex . mcstIndex)
  case mo of
    Just o -> return o
    Nothing -> fail ("No output associated with: " ++ show outref)

-- | Returns the current internal slot count.
slot :: (Monad m) => MockChainT m Pl.Slot
slot = gets (Pl.Slot . mcscCurrentSlot . mcstSlotCtr)

-- * Utilities

rstr :: (Monad m) => (a, m b) -> m (a, b)
rstr (a, mb) = (a,) <$> mb

assocl :: (a, (b, c)) -> ((a, b), c)
assocl (a, (b, c)) = ((a, b), c)
